{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Form where

import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified System.Environment   as Env
import qualified System.Process       as P

import           System.Exit          (ExitCode(..))
import           Lens.Micro           (Lens', lens, (&), (.~))
import           Lens.Micro.TH
import           Control.Monad.IO.Class

import qualified Graphics.Vty         as V

import           Brick
import           Data.List            (intercalate)
import           Data.List.Split
import           Data.Maybe

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit   as E

import           Brick.Forms          (Form, checkboxField, editTextField,
                                       focusedFormInputAttr, formFocus, setFormFocus,
                                       formState, handleFormEvent,
                                       invalidFormInputAttr, newForm,
                                       renderForm, (@@=))

import           Brick.Focus          (focusGetCurrent, focusRingCursor)


-- Types and Lenses

data Name = Segment String
          | Command
          deriving (Eq, Ord, Show)

type SegmentMap = M.Map String Bool

data AppState =
    AppState
      { _command  :: T.Text
      , _segments :: SegmentMap
      , _output   :: Maybe (ExitCode, String, String)
      }
      deriving (Show)

makeLenses ''AppState

seg :: String -> Lens' SegmentMap Bool
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
               Nothing                  -> "Unknown Error"
               Just (ExitSuccess, o, _) -> o
               Just (c, o, e)           -> "Error: " <> show c <> out o <> err e

segs :: Form AppState e n -> SegmentMap
segs = _segments . formState

cmd :: Form AppState e n -> String
cmd = T.unpack . _command . formState

eventHandler :: Form AppState e Name -> BrickEvent Name e -> EventM Name (Next (Form AppState e Name))
eventHandler s (VtyEvent (V.EvResize {}))            = continue s
eventHandler s (VtyEvent (V.EvKey V.KEsc _))         = halt s
eventHandler s e = do
  s' <- handleFormEvent e s

  let
    psegs = segs s
    c     = cmd s
    c'    = cmd s'

  case (e, focusGetCurrent (formFocus s')) of
    (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]), _)    -> halt s
    (VtyEvent (V.EvKey (V.KChar 'q') _), Just Command) -> continue s'
    (VtyEvent (V.EvKey (V.KChar 'q') _), _)            -> halt s
    _ | psegs == segs s' && c == c'                    -> continue s'
    (_, Nothing)                                       -> continue s'
    (_, Just f)                                        -> do
      o <- liftIO $ getOut c' (getPath s')
      s' & formState & output .~ (Just o) & mkForm & setFormFocus f & continue

app :: App (Form AppState e Name) e Name
app =
    App { appDraw         = draw
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent   = return
        , appAttrMap      = const style
        , appHandleEvent  = eventHandler
        }

getOut :: String -> [String] -> IO (ExitCode, String, String)
getOut s p = do
  let
    p1 = P.shell s
    p2 = p1 { P.env = Just [("PATH", mkPath p)] }

  P.readCreateProcessWithExitCode p2 ""

mkPath :: [[Char]] -> [Char]
mkPath  = intercalate ":"

getPath :: Form AppState e n -> [String]
getPath = map fst . filter snd . M.toList . _segments . formState

main :: IO ()
main = do
    let initialCommand = "ls"

    path <- splitOn ":" <$> Env.getEnv "PATH"
    out  <- Just <$> getOut initialCommand path

    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        state =
          AppState
            { _command  = T.pack initialCommand
            , _segments = M.fromList $ zip path (repeat True)
            , _output   = out
            }

        form = mkForm state

    initialVty <- buildVty
    result     <- customMain initialVty buildVty Nothing app form

    putStrLn $ ("PATH=" ++) $ mkPath $ getPath result

