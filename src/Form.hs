{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BlockArguments   #-}

module Form where

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

import           Data.List                (intercalate)
import           Data.List.Split
import           Data.Maybe

import Brick ((<+>), (<=>))
import qualified Brick
import qualified Brick.Widgets.Border  as Brick
import qualified Brick.Widgets.Center  as Brick
import qualified Brick.Widgets.Edit    as Brick
import qualified Brick.BChan           as Brick
import qualified Brick.Focus           as Brick
import           Brick.Forms           as Brick

-- Types and Lenses

data Name
  = Segment String
  | Command
  | OutPort
  deriving (Eq, Ord, Show)

type PathSegments = M.Map String Bool

data Hide a = Hide { unHide :: a }

instance Show (Hide a) where show (Hide _) = "HIDDEN"

type ProcessResult = (ExitCode, String, String)

type Bus = Brick.BChan ()

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
    let label s w = Brick.padBottom (Brick.Pad 1) $ (Brick.vLimit 1 $ Brick.hLimit 15 $ Brick.str s <+> Brick.fill ' ') <+> w
    in flip newForm state $
      [ label "Command" @@= editTextField command Command (Just 1)
      ] ++ map (makeSegInput) (M.toList $ _segments state)

  where
  makeSegInput (k, _v) = checkboxField (segments . seg k) (Segment k) (T.pack k)

style :: Brick.AttrMap
style = Brick.attrMap V.defAttr
  [ (Brick.editAttr,        V.white `Brick.on` V.black)
  , (Brick.editFocusedAttr, V.black `Brick.on` V.yellow)
  , (invalidFormInputAttr,  V.white `Brick.on` V.red)
  , (focusedFormInputAttr,  V.black `Brick.on` V.yellow)
  ]

draw :: Form AppState e Name -> [Brick.Widget Name]
draw f = [ Brick.padTop (Brick.Pad 2) (Brick.hCenter form) <=> Brick.padAll 2 (Brick.str "Keys: Up/Down/Left/Right") <=> Brick.hCenter outW]
  where
    out "" = ""
    out o  = "\n\nSTDOUT:\n\n" <> o
    err "" = ""
    err e  = "\n\nSTDERR:\n\n" <> e
    form   = Brick.borderWithLabel (Brick.str " ~ Path Explorer ~ ") $ Brick.hLimit 88 $ renderForm f
    outW   = Brick.padTop (Brick.Pad 1) $ Brick.borderWithLabel (Brick.str (fst body))
           $ Brick.vLimit 44 $ Brick.hLimit 88 (Brick.viewport OutPort Brick.Vertical (Brick.str (snd body)))
    body   = case _output (formState f) of
               Right (ExitSuccess, o, _) -> ("Success", o)
               Left s                    -> ("", s)
               Right (c, o, e)           -> ("Failure", "Error: " <> show c <> out o <> err e)

segs :: Form AppState e n -> PathSegments
segs = _segments . formState

cmd :: Form AppState e n -> String
cmd = T.unpack . _command . formState

-- TODO: updatePreserveFocus :: ...

setFF :: Eq n => Form s e n -> Form s e n -> Form s e n
setFF s s' =
  case Brick.focusGetCurrent (formFocus s) of
    Nothing -> s'
    Just f  -> setFormFocus f s'

eventHandler :: Bus -> Form AppState e Name -> Brick.BrickEvent Name e -> Brick.EventM Name (Brick.Next (Form AppState e Name))
eventHandler _ s (Brick.VtyEvent (V.EvResize {}))    = Brick.continue s
eventHandler _ s (Brick.VtyEvent (V.EvKey V.KEsc _)) = Brick.halt s
eventHandler chan s e = do
  s' <- handleFormEvent e s >>= resolveAction

  let
    psegs = segs s
    c     = cmd s
    c'    = cmd s'
    sp    = Brick.viewportScroll OutPort

  case (e, Brick.focusGetCurrent (formFocus s')) of
    (Brick.VtyEvent (V.EvKey V.KUp _), _)                             -> Brick.vScrollBy sp (-1)  >> Brick.continue s'
    (Brick.VtyEvent (V.EvKey V.KDown _), _)                           -> Brick.vScrollBy sp 1     >> Brick.continue s'
    (Brick.VtyEvent (V.EvKey V.KLeft _), _)                           -> Brick.vScrollBy sp (-10) >> Brick.continue s'
    (Brick.VtyEvent (V.EvKey V.KRight _), _)                          -> Brick.vScrollBy sp 10    >> Brick.continue s'
    (Brick.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]), _)             -> Brick.halt s
    (Brick.VtyEvent (V.EvKey (V.KChar 'q') _), x) | x /= Just Command -> Brick.halt s
    _ | psegs == segs s' && c == c'                                   -> Brick.continue s'
    _ -> do
      liftIO $ Async.cancel $ unHide $ _action $ formState $ s'
      a <- getOut chan c' (getPath s')
      s' & formState & action .~ a & mkForm & setFF s' & Brick.continue

resolveAction :: MonadIO m => Form AppState e Name -> m (Form AppState e Name)
resolveAction s = do
  p <- liftIO $ Async.poll $ unHide $ _action $ formState s
  case p of
    Nothing -> return $ s & formState & output .~ Left "Loading" & mkForm & setFF s
    Just r  -> return $ s & formState & output .~ left show r & mkForm & setFF s

app :: Bus -> Brick.App (Form AppState e Name) e Name
app chan =
  Brick.App
    { Brick.appDraw         = draw
    , Brick.appChooseCursor = Brick.focusRingCursor formFocus
    , Brick.appStartEvent   = return
    , Brick.appAttrMap      = const style
    , Brick.appHandleEvent  = eventHandler chan
    }

getOut :: MonadIO m => Bus -> [Char] -> [[Char]] -> m (Hide (Async.Async (ExitCode, String, String)))
getOut c s p = liftIO $ do
  let
    p1 = P.shell s
    p2 = p1 { P.env = Just [("PATH", mkPath p)] }

  a <- Async.async do
    r <- P.readCreateProcessWithExitCode p2 ""
    _ <- Brick.writeBChanNonBlocking c () -- Immediately update once completed
    return r

  _ <- Brick.writeBChanNonBlocking c () -- Immediately update as pending
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
    chan <- Brick.newBChan 1 -- Event prompting channel
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
    result     <- Brick.customMain initialVty buildVty (Just chan) (app chan) form

    putStrLn $ ("PATH=" ++) $ mkPath $ getPath result
