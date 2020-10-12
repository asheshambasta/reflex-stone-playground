{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Main
  ( main
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Protolude
import           Control.Monad                  ( void )
import qualified Data.Text                     as T
import           Reflex.Dom                    as RD

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "reflex-stone"
  dText   <- buttonTextInput
  clicked <- stoneButton dText
  cnt     <- foldDyn (+) (0 :: Int) $ 1 <$ clicked
  elClass "p" "result" $ dyn_ $ ffor cnt $ \case
    0 -> text "Go ahead and hit the stone."
    n -> text . T.unwords $ [show n, " heads!"]

buttonTextInput
  :: (MonadHold t m, MonadFix m, DomBuilder t m) => m (Dynamic t T.Text)
buttonTextInput = do
  rec let send = keypress Enter input

      input <-
        RD.inputElement
        $ def
                    -- & (inputElementConfig_setValue .~ fmap _foo send)
        & (  inputElementConfig_elementConfig
          .  elementConfig_initialAttributes
          .~ ("placeholder" =: "Stone!")
          )

  -- pure $ tag (RD.current $ _inputElement_value input) send
  pure $ _inputElement_value input

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
