module Main
  ( main
  )
where

import qualified Data.Map                      as Map
import           Control.Monad.Fix              ( MonadFix )
import           Protolude
import           Control.Monad                  ( void )
import qualified Data.Text                     as T
import           Reflex.Dom                    as RD

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "reflex-stone"
  inputMode <- inputModeDropdown
  dText     <- buttonTextInput inputMode
  clicked   <- stoneButton dText
  cnt       <- foldDyn (+) (0 :: Int) $ 1 <$ clicked
  elClass "p" "result" $ dyn_ $ ffor cnt $ \case
    0 -> text "Go ahead and hit the stone."
    n -> text . T.unwords $ [show n, " heads!"]

data InputMode = Realtime | OnKeypressEnter
               deriving (Eq, Show, Ord)

inputModeDropdown
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m (Dynamic t InputMode)
inputModeDropdown =
  RD.dropdown Realtime (constDyn ops) def <&> RD._dropdown_value
 where
  ops = Map.fromList [(Realtime, "Realtime"), (OnKeypressEnter, "On enter")]

buttonTextInput
  :: (MonadHold t m, MonadFix m, DomBuilder t m)
  => Dynamic t InputMode
  -> m (Dynamic t T.Text)
buttonTextInput imode = do
  input@RD.InputElement { _inputElement_value = dVal } <-
    RD.inputElement
    $ def
    & (  inputElementConfig_elementConfig
      .  elementConfig_initialAttributes
      .~ ("placeholder" =: "Stone!")
      )
  case undefined of
    Realtime -> pure dVal
    -- eEnterPressed: tags the enter keypress event with the current value of the input field.
    -- send: fires on each keypress of `Enter` in the `input` field.
    OnKeypressEnter ->
      let eValAtEnter = tag (RD.current dVal) send
          send        = keypress Enter input
      in  RD.holdDyn "" eValAtEnter


stoneButton
  :: (MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text
  -> m (Event t ())
stoneButton dText = do
  let attr = "style" =: "font-size: 200%;"
  -- dText <- RD.holdDyn "Stone!" eText
  clickEvent $ elAttr' "button" attr (RD.dynText dText)

stone :: DomBuilder t m => m ()
stone = text "stone"

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent
  :: (DomBuilder t m, HasDomEvent t target 'ClickTag)
  => m (target, a)
  -> m (Event t ())
clickEvent = fmap (void . domEvent Click . fst)
