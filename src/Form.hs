{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Form where

import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified System.Environment   as Env
import qualified System.Process       as P

import           GHC.IO.Exception     (ExitCode)
import           Lens.Micro           (Lens', lens, (.~), (&))
import           Lens.Micro.TH

import qualified Graphics.Vty         as V

import           Brick
import           Data.List            (intercalate)
import           Data.List.Split
import           Data.Maybe

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit   as E

import           Brick.Forms          (Form, checkboxField, setFormFocus,
                                       editTextField, focusedFormInputAttr,
                                       formFocus, formState, handleFormEvent,
                                       invalidFormInputAttr,
                                       newForm, renderForm,
                                       (@@=))

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
seg k = lens (fromMaybe False . M.lookup k) (\m b -> if b then M.insert k True m else M.delete k m)


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
    form = B.border $ padTop (Pad 1) $ hLimit 88 $ renderForm f
    help = padTop (Pad 1) $ B.borderWithLabel (str "Output") body
    body = str $ show $ _output $ formState f

eventHandler :: Form AppState e Name -> BrickEvent Name e -> EventM Name (Next (Form AppState e Name))
eventHandler s ev =
  case ev of
      VtyEvent (V.EvResize {})       -> continue s
      VtyEvent (V.EvKey V.KEsc [])   -> halt s
      _ -> do
        s' <- handleFormEvent ev s
        if focusGetCurrent (formFocus s') == Just Command
           then s' & formState & output .~ Nothing & mkForm & continue
           else do
             case ev of
               VtyEvent (V.EvKey (V.KChar 'q') [])        -> halt s
               VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt s
               _                                          -> continue s'

app :: App (Form AppState e Name) e Name
app =
    App { appDraw         = draw
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent   = return
        , appAttrMap      = const style
        , appHandleEvent  = eventHandler
        }

getOut :: [Char] -> [Char] -> IO (ExitCode, String, String)
getOut s p = do
  let
    p1 = P.shell s
    p2 = p1 { P.env = Just [("PATH", p)] }

  P.readCreateProcessWithExitCode p2 ""

main :: IO ()
main = do
    path <- Env.getEnv "PATH"
    out  <- Just <$> getOut "date +s" []

    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        state =
          AppState
            { _command  = "date +s"
            , _segments = M.fromList $ zip (splitOn ":" path) (repeat True)
            , _output   = out
            }

        form = mkForm state

    initialVty <- buildVty
    result     <- customMain initialVty buildVty Nothing app form

    putStrLn $ ("PATH=" ++) $  intercalate ":" $ M.keys $ _segments $ formState result

