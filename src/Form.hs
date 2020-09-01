{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE PatternSynonyms   #-}

module Form where

import           Control.Arrow            (left)
import           Lens.Micro               (Lens', lens, (&), (^.), (.~), (%~))
import           System.Exit              (ExitCode (..))
import           Data.List                (findIndex, intercalate, nub)
import           Data.List.Split
import           Data.Maybe
import           Lens.Micro.TH
import           Control.Monad.IO.Class

import qualified Control.Concurrent.Async as Async
import qualified Data.Text                as T
import qualified System.Environment       as Env
import qualified System.Process           as P

import Brick       ((<+>), (<=>))
import Brick.Forms ((@@=))
import qualified Brick
import qualified Brick.Widgets.Border  as Brick
import qualified Brick.Widgets.Center  as Brick
import qualified Brick.Widgets.Edit    as Brick
import qualified Brick.BChan           as Brick
import qualified Brick.Focus           as Brick
import qualified Brick.Forms           as Brick
import qualified Graphics.Vty          as V
import qualified Graphics.Vty.Input.Events as Brick

-- Types and Lenses

data    Name = Segment String | Command | OutPort deriving (Eq, Ord, Show)
type    PathSegments = [(String, Bool)]
type    ProcessResult = (ExitCode, String, String)
type    Bus = Brick.BChan ()
newtype Hide a = Hide { unHide :: a }

instance Show (Hide a) where show (Hide _) = "HIDDEN"

data AppState = AppState
    { _command  :: T.Text
    , _segments :: PathSegments
    , _action   :: Hide (Async.Async ProcessResult)
    , _output   :: Either String (ExitCode, String, String)
    }
    deriving (Show)

makeLenses ''AppState

formStateLens :: Lens' (Brick.Form s e n) s
formStateLens = lens Brick.formState (\f s -> f { Brick.formState = s })

-- | 'lookup' style lens with default value
seg :: Eq a => b -> a -> Lens' [(a,b)] b
seg d k = lens (fromMaybe d . lookup k) (\m b -> map (\(x,y) -> if x == k then (x,b) else (x,y)) m)

-- Forms

style :: Brick.AttrMap
style = Brick.attrMap V.defAttr
  [ (Brick.editAttr,             V.white `Brick.on` V.black)
  , (Brick.editFocusedAttr,      V.black `Brick.on` V.yellow)
  , (Brick.invalidFormInputAttr, V.white `Brick.on` V.red)
  , (Brick.focusedFormInputAttr, V.black `Brick.on` V.yellow)
  ]

draw :: Brick.Form AppState e Name -> [Brick.Widget Name]
draw f = [ Brick.padTop (Brick.Pad 2) (Brick.hCenter form) <=> Brick.hCenter outW]
  where
    helpC  = " Path Explorer ~ (Keys: Enter/Space/Tab/C-n/C-p/C-u/C-d) "
    helpO  = " ~ (Keys: Up/Down/Left/Right/C-j/C-k/C-l) "
    out "" = ""
    out o  = "\n\nSTDOUT:\n\n" <> o
    err "" = ""
    err e  = "\n\nSTDERR:\n\n" <> e
    form   = Brick.borderWithLabel (Brick.str helpC)
           $ Brick.hLimit 88 $ Brick.renderForm f
    outW   = Brick.padTop (Brick.Pad 1) $ Brick.borderWithLabel (Brick.str (" " <> fst body <> helpO))
           $ Brick.vLimit 44 $ Brick.hLimit 88 (Brick.viewport OutPort Brick.Vertical (Brick.str (snd body)))
    body   = case _output (Brick.formState f) of
               Right (ExitSuccess, o, _) -> ("Success", o)
               Left s                    -> (mempty, s)
               Right (c, o, e)           -> ("Failure", "Error: " <> show c <> out o <> err e)

segs :: Brick.Form AppState e n -> PathSegments
segs = _segments . Brick.formState

cmd :: Brick.Form AppState e n -> String
cmd = T.unpack . _command . Brick.formState

reorder :: Int -> Brick.Form AppState e Name -> Brick.EventM n (Brick.Next (Brick.Form AppState e Name))
reorder d s = Brick.continue
  case Brick.focusGetCurrent $ Brick.formFocus s of
    Just (Segment n) ->
      case findIndex ((== n) . fst) (s ^. formStateLens . segments) of
        Just i -> s & Brick.formState & segments %~ reorderL i d & deriveForm s
        _ -> s
    _ -> s

reorderL :: Int -> Int -> [a] -> [a]
reorderL n d o =
  case splitAt n o of
    (x,y:z) -> let (x',z') = splitAt (n+d) (x++z) in x' ++ [y] ++ z'
    _       -> o

pattern VtyC :: Char -> [Brick.Modifier] -> Brick.BrickEvent n e
pattern VtyC c ms = Brick.VtyEvent (V.EvKey (V.KChar c) ms)

pattern VtyE :: Brick.Key -> [Brick.Modifier] -> Brick.BrickEvent n e
pattern VtyE k ms = Brick.VtyEvent (V.EvKey k ms)

focus :: Eq n => (Brick.FocusRing n -> Brick.FocusRing n) -> Brick.Form s e n -> Brick.Form s e n
focus fun s = case nf of
  Nothing -> s
  Just x  -> Brick.setFormFocus x s
  where
    f  = Brick.formFocus s
    nf = Brick.focusGetCurrent $ fun f

eventHandler :: Bus -> Brick.Form AppState e Name -> Brick.BrickEvent Name e -> Brick.EventM Name (Brick.Next (Brick.Form AppState e Name))
eventHandler _ s (Brick.VtyEvent (V.EvResize {}))    = Brick.continue s
eventHandler _ s (Brick.VtyEvent (V.EvKey V.KEsc _)) = Brick.halt s
eventHandler chan s e = do
  s' <- Brick.handleFormEvent e s >>= resolveAction

  let
    psegs = segs s
    c     = cmd s
    c'    = cmd s'
    sp    = Brick.viewportScroll OutPort
    f     = Brick.formFocus s'
    cf    = Brick.focusGetCurrent f

  case e of
    VtyC 'q' _ | cf /= Just Command -> Brick.halt s

    VtyC 'c' [V.MCtrl] -> Brick.halt s
    VtyC 'n' [V.MCtrl] -> Brick.continue $ focus Brick.focusNext s'
    VtyC 'p' [V.MCtrl] -> Brick.continue $ focus Brick.focusPrev s'
    VtyC 'k' [V.MCtrl] -> Brick.vScrollBy sp (-1) >> Brick.continue s'
    VtyC 'j' [V.MCtrl] -> Brick.vScrollBy sp 1    >> Brick.continue s'
    VtyC 'l' [V.MCtrl] -> Brick.vScrollBy sp 1    >> Brick.continue s'

    VtyC 'u' [V.MCtrl] -> reorder (-1) s'
    VtyC 'd' [V.MCtrl] -> reorder (1)  s'

    VtyE V.KUp    _ -> Brick.vScrollBy sp (-1)  >> Brick.continue s'
    VtyE V.KDown  _ -> Brick.vScrollBy sp 1     >> Brick.continue s'
    VtyE V.KLeft  _ -> Brick.vScrollBy sp (-10) >> Brick.continue s'
    VtyE V.KRight _ -> Brick.vScrollBy sp 10    >> Brick.continue s'
    VtyE V.KEnter _ -> Brick.setFormFocus Command s' & Brick.continue

    _ | psegs == segs s' && c == c' -> Brick.continue s'

    _ -> do
      liftIO $ Async.cancel $ unHide $ _action $ Brick.formState $ s'
      a <- getOut chan c' (getPath s')
      s' & formStateLens . action .~ a & Brick.continue

resolveAction :: MonadIO m => Brick.Form AppState e Name -> m (Brick.Form AppState e Name)
resolveAction s = do
  p <- liftIO $ Async.poll $ unHide $ _action $ Brick.formState s
  case p of
    Nothing -> return $ s & formStateLens . output .~ Left "Loading"
    Just r  -> return $ s & formStateLens . output .~ left show r

app :: Bus -> Brick.App (Brick.Form AppState e Name) e Name
app chan =
  Brick.App
    { Brick.appDraw         = draw
    , Brick.appChooseCursor = Brick.focusRingCursor Brick.formFocus
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

getPath :: Brick.Form AppState e n -> [String]
getPath = map fst . filter snd . _segments . Brick.formState

initialCommand :: String
initialCommand = "ls"

deriveForm :: Brick.Form s e1 Name -> AppState -> Brick.Form AppState e2 Name
deriveForm f s =
  case Brick.focusGetCurrent (Brick.formFocus f) of
    Nothing -> mkForm s
    Just f' -> Brick.setFormFocus f' $ mkForm s

mkForm :: AppState -> Brick.Form AppState e Name
mkForm state =
  flip Brick.newForm state $
    (label "Command" @@= Brick.editTextField command Command (Just 1))
    : map makeSegInput (_segments state)

  where
    label s w            = Brick.padBottom (Brick.Pad 1) $ (Brick.vLimit 1 $ Brick.hLimit 15 $ Brick.str s <+> Brick.fill ' ') <+> w
    makeSegInput (k, _v) = Brick.checkboxField (segments . seg False k) (Segment k) (T.pack k)

main :: IO ()
main = do
    path <- nub . splitOn ":" <$> Env.getEnv "PATH" -- Remove duplicates
    chan <- Brick.newBChan 1 -- Event prompting channel
    ls   <- getOut chan initialCommand path

    let
      buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v

      appState =
        AppState
          { _command  = T.pack initialCommand
          , _segments = zip path (repeat True)
          , _action   = ls
          , _output   = Left "Nothing Yet!"
          }

    initialVty <- buildVty
    result     <- Brick.customMain initialVty buildVty (Just chan) (app chan) (mkForm appState)

    putStrLn $ ("PATH=" ++) $ mkPath $ getPath result
