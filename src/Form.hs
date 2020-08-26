{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes        #-}

module Form where

import qualified System.Environment   as Env
import qualified Data.Map             as M
import qualified Data.Text            as T

import           Lens.Micro           ((^.), lens, Lens')
import           Lens.Micro.TH

import qualified Graphics.Vty         as V

import           Brick
import           Data.Maybe
import           Data.List.Split

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit   as E

import           Brick.Forms          (Form, allFieldsValid, checkboxField,
                                       editPasswordField, editShowableField,
                                       editTextField, focusedFormInputAttr,
                                       formFocus, formState, handleFormEvent,
                                       invalidFields, invalidFormInputAttr,
                                       newForm, radioField, renderForm,
                                       setFieldValid, (@@=))

import           Brick.Focus          (focusGetCurrent, focusRingCursor)

type SegmentMap = M.Map String Bool

seg :: String -> Lens' SegmentMap Bool
seg k = lens (fromMaybe False . M.lookup k) (\m b -> M.insert k b m)

data Name = NameField
          | AgeField
          | BikeField
          | HandedField
          | PasswordField
          | LeftHandField
          | RightHandField
          | AmbiField
          | AddressField
          | Segment String
          | Command
          deriving (Eq, Ord, Show)

data Handedness = LeftHanded | RightHanded | Ambidextrous deriving (Show, Eq)

data AppState =
    AppState { _name      :: T.Text
             , _age       :: Int
             , _address   :: T.Text
             , _ridesBike :: Bool
             , _handed    :: Handedness
             , _password  :: T.Text
             , _command   :: T.Text
             , _segments  :: SegmentMap
             }
             deriving (Show)

makeLenses ''AppState

-- This form is covered in the Brick User Guide; see the "Input Forms" section.
mkForm :: AppState -> Form AppState e Name
mkForm state =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 15 $ str s <+> fill ' ') <+> w
    in flip newForm state $
      [ label "Name" @@= editTextField name NameField (Just 1)
      , label "Address" @@= B.borderWithLabel (str "Mailing") @@= editTextField address AddressField (Just 3)
      , label "Age" @@= editShowableField age AgeField
      , label "Password" @@= editPasswordField password PasswordField
      , label "Dominant hand" @@=
                   radioField handed [ (LeftHanded, LeftHandField, "Left")
                                     , (RightHanded, RightHandField, "Right")
                                     , (Ambidextrous, AmbiField, "Both")
                                     ]
      , label "" @@= checkboxField ridesBike BikeField "Do you ride a bicycle?"
      , label "Command" @@= editTextField command Command (Just 1)
      ] ++ map (makeSegInput label) (M.toList $ _segments state)

-- makeSegInput :: Data.String.IsString t => (t -> Widget Name -> Widget Name) -> (String, b) -> AppState -> Brick.Forms.FormFieldState AppState e Name
makeSegInput label (k, _v) = label "" @@= checkboxField (segments . seg k) (Segment k) (T.pack k)

style :: AttrMap
style = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form AppState e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Name is free-form text\n" <>
                     "- Age must be an integer (try entering an\n" <>
                     "  invalid age!)\n" <>
                     "- Handedness selects from a list of options\n" <>
                     "- The last option is a checkbox\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

eventHandler :: Form AppState e Name -> BrickEvent Name e -> EventM Name (Next (Form AppState e Name))
eventHandler s ev =
  case ev of
      VtyEvent (V.EvResize {})     -> continue s
      VtyEvent (V.EvKey V.KEsc [])   -> halt s
      VtyEvent (V.EvKey V.KEnter []) | focusGetCurrent (formFocus s) /= Just AddressField -> halt s
      _ -> do
          s' <- handleFormEvent ev s

          -- Example of external validation:
          -- Require age field to contain a value that is at least 18.
          continue $ setFieldValid ((formState s')^.age >= 18) AgeField s'

app :: App (Form AppState e Name) e Name
app =
    App { appDraw         = draw
        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent   = return
        , appAttrMap      = const style
        , appHandleEvent  = eventHandler
        }

main :: IO ()
main = do
    path <- Env.getEnv "PATH"

    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = AppState { _name      = ""
                                   , _address   = ""
                                   , _age       = 0
                                   , _handed    = RightHanded
                                   , _ridesBike = False
                                   , _password  = ""
                                   , _command   = "date"
                                   , _segments  = M.fromList $ zip (splitOn ":" path) (repeat True)
                                   }

        f = setFieldValid False AgeField $ mkForm initialUserInfo

    initialVty <- buildVty
    result     <- customMain initialVty buildVty Nothing app f

    putStrLn "The starting form state was:"
    print initialUserInfo

    putStrLn "The final form state was:"
    print $ formState result

    if allFieldsValid result
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields result)

