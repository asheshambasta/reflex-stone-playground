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
dispMoney dMoney = elAttr "span" mempty $ dynText (showMoney <$> dMoney)

dispProduct :: DomBuilder t m => Product -> m (Event t ())
dispProduct Product {..} = elClass "div" "product-data" $ do
  text pName
  text " "
  mkButtonConstText attrs $ "Buy: " <> showMoney pCost
  where attrs = "style" =: "color: blue;"

showMoney :: Money -> Text
showMoney (Money m) = "$" <> show m

showProduct :: Product -> Text
showProduct Product {..} = pName <> " @ " <> showMoney pCost

dispProductStock
  :: (DomBuilder t m, PostBuild t m, MonadFix m)
  => Product
  -> Dynamic t Stock
  -> m (Event t ProductStock)
dispProductStock prod@Product {..} dStock =
  elClass "div" "product" $ dispProduct prod >> dispStock >> dispSelectRadio
 where
  dispStock       = dynText $ mappend "Stock: " . show <$> dStock
  dispSelectRadio = do
    rec
      el "div" $ pure radio
      (radio, _) <-
        elAttr' "input" inputAttrs . elAttr "label" labelAttrs $ text ""
    let bProdStock = ProductStock prod <$> current dStock
    pure $ tag bProdStock (clickEvent' radio)
   where
    labelAttrs = "for" =: id
    inputAttrs =
      M.fromList [("type", "radio"), ("id", id), ("name", "prodSelect")]
    id = pName

