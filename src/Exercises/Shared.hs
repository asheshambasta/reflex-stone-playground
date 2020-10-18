{-# LANGUAGE RecursiveDo #-}
module Exercises.Shared
  ( Product(..)
  , Money(..)
  , Stock(..)
  , ProductStock(..)
  , dispMoney
  , dispProduct
  , dispProductStock
  , showMoney
  , showProduct
  , carrot
  , celery
  , cucumber
  )
where

import           Data.Default.Class             ( Default(..) )
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Clicks              ( clickEvent' )
import qualified Data.Map                      as M
import           Lib.Reflex.Buttons             ( mkButtonConstText )
import           Reflex.Dom
import           Protolude               hiding ( Product )

-- * Types

newtype Stock = Stock Int deriving (Eq, Show, Num, Ord) via Int

instance Default Stock where
  def = 5

newtype Money = Money Int
              deriving (Eq, Show, Num, Ord) via Int

data Product = Product
  { pName :: Text
  , pCost :: Money
  }
  deriving (Eq, Show)

data ProductStock = ProductStock
  { psProduct :: Product
  , psStock   :: Stock
  }
  deriving (Eq, Show)

carrot :: Product
carrot = Product "Carrot" 1

celery :: Product
celery = Product "Celery" 2

cucumber :: Product
cucumber = Product "Cucumber" 3

dispMoney :: (PostBuild t m, DomBuilder t m) => Dynamic t Money -> m ()
dispMoney dMoney = elClass "span" "money" $ dynText (showMoney <$> dMoney)

dispProduct :: DomBuilder t m => Product -> m (Event t ())
dispProduct Product {..} = elAttr "div" ("style" =: prodStyle) $ do
  text pName
  text " "
  mkButtonConstText attrs $ "Buy: " <> showMoney pCost
 where
  attrs     = "style" =: "color: blue;"
  prodStyle = "border: solid; border-color: red #32a1ce;"

showMoney :: Money -> Text
showMoney (Money m) = "$" <> show m

showProduct :: Product -> Text
showProduct Product {..} = pName <> " @ " <> showMoney pCost

{- |
Displays a product along with its stock. The click event
indicates the user clicking on the radio button to select the given product.

This click event currently only reports the product being selected; and not the current stock value.
If we were to report the stock value; this value would be the stock value at the time of click. This can be misused.
-}
dispProductStock
  :: (DomBuilder t m, PostBuild t m, MonadFix m)
  => Product
  -> Dynamic t Stock
  -> m (Event t Product)
dispProductStock prod@Product {..} dStock =
  elAttr "div" attrs $ dispProduct prod >> dispStock >> dispSelectRadio
 where
  attrs =
    "style"
      =: "background-color: rgba(255, 255, 128, .5); border: solid; border-color: blue; border-spacing: 5px 1rem;"
  dispStock       = dynText $ mappend "Stock: " . show <$> dStock
  dispSelectRadio = do
    rec
      el "div" $ pure radio
      (radio, _) <-
        elAttr' "input" inputAttrs . elAttr "label" labelAttrs $ text ""
    pure $ clickEvent' radio $> prod
   where
    labelAttrs = "for" =: id
    inputAttrs =
      M.fromList [("type", "radio"), ("id", id), ("name", "prodSelect")]
    id = pName

