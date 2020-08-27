{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BlockArguments   #-}

module Form where

-- TODO: VSCode

import qualified Control.Concurrent.Async as Async
import qualified Data.Map                 as M
import qualified Data.Text                as T
import qualified System.Environment       as Env
import qualified System.Process           as P

import           Control.Arrow            (left)
import           Control.Monad.IO.Class
import           Lens.Micro               (Lens', lens, (&), (.~))
import           Lens.Micro.TH
import           System.Exit              (ExitCode (..))

import qualified Graphics.Vty             as V

import           Brick
import           Data.List                (intercalate)
import           Data.List.Split
import           Data.Maybe

import qualified Brick.Widgets.Border     as B
import qualified Brick.Widgets.Center     as C
import qualified Brick.Widgets.Edit       as E
import qualified Brick.BChan              as BChan

import           Brick.Forms              (Form, checkboxField, editTextField,
                                           focusedFormInputAttr, formFocus,
                                           formState, handleFormEvent,
                                           invalidFormInputAttr, newForm,
                                           renderForm, setFormFocus, (@@=))

import           Brick.Focus              (focusGetCurrent, focusRingCursor)


-- Types and Lenses

data Name
  = Segment String
  | Command
  deriving (Eq, Ord, Show)

type PathSegments = M.Map String Bool

data Hide a = Hide { unHide :: a }

instance Show (Hide a) where show (Hide _) = "HIDDEN"

type ProcessResult = (ExitCode, String, String)

type Bus = BChan.BChan ()

data AppState = AppState
    { _command  :: T.Text
    , _segments :: PathSegments
    , _action   :: Hide (Async.Async ProcessResult)
    , _output   :: Either String (ExitCode, String, String)
    }
    deriving (Show)

makeLenses ''AppState

seg :: String -> Lens' PathSegments Bool
seg k = lens (fromMaybe False . M.lookup k) (\m b -> M.insert k b m)


-- Forms

mkForm :: AppState -> Form AppState e Name
mkForm state =
    let label s w = padBottom (Pad 1) $ (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in flip newForm state $
      [ label "Command" @@= editTextField command Command (Just 1)
      ] ++ map (makeSegInput) (M.toList $ _segments state)

  where
  makeSegInput (k, _v) = checkboxField (segments . seg k) (Segment k) (T.pack k)

style :: AttrMap
style = attrMap V.defAttr
  [ (E.editAttr          , V.white `on` V.black)
  , (E.editFocusedAttr   , V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form AppState e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
  where
    out "" = ""
    out o  = "\n\nSTDOUT:\n\n" <> o
    err "" = ""
    err e  = "\n\nSTDERR:\n\n" <> e
    form   = B.border $ padTop (Pad 1) $ hLimit 88 $ renderForm f
    help   = padTop (Pad 1) $ B.borderWithLabel (str "Output") (str body)
    body   = case _output (formState f) of
               Right (ExitSuccess, o, _) -> o
               Left s                    -> s
               Right (c, o, e)           -> "Error: " <> show c <> out o <> err e

segs :: Form AppState e n -> PathSegments
segs = _segments . formState

cmd :: Form AppState e n -> String
cmd = T.unpack . _command . formState

-- TODO: updatePreserveFocus :: ...

setFF :: Eq n => Form s e n -> Form s e n -> Form s e n
setFF s s' =
  case focusGetCurrent (formFocus s) of
    Nothing -> s'
    Just f  -> setFormFocus f s'

eventHandler :: Bus -> Form AppState e Name -> BrickEvent Name e -> EventM Name (Next (Form AppState e Name))
eventHandler _ s (VtyEvent (V.EvResize {}))            = continue s
eventHandler _ s (VtyEvent (V.EvKey V.KEsc _))         = halt s
eventHandler chan s e = do
  s' <- handleFormEvent e s >>= resolveAction

  let
    psegs = segs s
    c     = cmd s
    c'    = cmd s'

  case (e, focusGetCurrent (formFocus s')) of
    (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]), _)    -> halt s
    (VtyEvent (V.EvKey (V.KChar 'q') _), Just Command) -> continue s'
    (VtyEvent (V.EvKey (V.KChar 'q') _), _)            -> halt s
    _ | psegs == segs s' && c == c'                    -> continue s'
    _ -> do
      liftIO $ Async.cancel $ unHide $ _action $ formState $ s'
      a <- getOut chan c' (getPath s')
      s' & formState & action .~ a & mkForm & setFF s' & continue

resolveAction :: MonadIO m => Form AppState e Name -> m (Form AppState e Name)
resolveAction s = do
  p <- liftIO $ Async.poll $ unHide $ _action $ formState s
  case p of
    Nothing -> return $ s & formState & output .~ Left "Loading" & mkForm & setFF s
    Just r  -> return $ s & formState & output .~ left show r & mkForm & setFF s

app :: Bus -> App (Form AppState e Name) e Name
app chan =
    App { appDraw         = draw
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent   = return
        , appAttrMap      = const style
        , appHandleEvent  = eventHandler chan
        }

getOut :: MonadIO m => Bus -> [Char] -> [[Char]] -> m (Hide (Async.Async (ExitCode, String, String)))
getOut c s p = liftIO $ do
  let
    p1 = P.shell s
    p2 = p1 { P.env = Just [("PATH", mkPath p)] }

  a <- Async.async do
    r <- P.readCreateProcessWithExitCode p2 ""
    _ <- BChan.writeBChanNonBlocking c () -- Immediately update once completed
    return r

  _ <- BChan.writeBChanNonBlocking c () -- Immediately update as pending
  return $ Hide a

mkPath :: [String] -> String
mkPath  = intercalate ":"

getPath :: Form AppState e n -> [String]
getPath = map fst . filter snd . M.toList . _segments . formState

initialCommand :: String
initialCommand = "ls"

main :: IO ()
main = do
    path <- splitOn ":" <$> Env.getEnv "PATH"
    chan <- BChan.newBChan 1 -- Event prompting channel
    ls   <- getOut chan initialCommand path

    -- Wake up the eventloop to check for async actions periodically
    -- Not required now that `getOut` signals its status
    {-
    _ <- Async.async do
          forever do
            Conc.threadDelay 100000
            BChan.writeBChanNonBlocking chan ()
            -}

    let
      buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

      state =
          AppState
            { _command  = T.pack initialCommand
            , _segments = M.fromList $ zip path (repeat True)
            , _action   = ls
            , _output   = Left "Nothing Yet!"
            }

      form = mkForm state

    initialVty <- buildVty
    result     <- customMain initialVty buildVty (Just chan) (app chan) form

    putStrLn $ ("PATH=" ++) $ mkPath $ getPath result
