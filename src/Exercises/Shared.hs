module Exercises.Shared
  ( Product(..)
  , Money(..)
  , Stock(..)
  , ProductStock(..)
  , dispMoney
  , dispProduct
  , showMoney
  , carrot
  , celery
  , cucumber
  )
where

import           Lib.Reflex.Buttons             ( mkButtonConstText )
import           Reflex.Dom
import           Protolude               hiding ( Product )

-- * Types

newtype Stock = Stock Int deriving (Eq, Show, Num, Ord) via Int

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
dispProduct Product {..} = do
  text pName
  text . showMoney $ pCost
  mkButtonConstText attrs "Buy"
  where attrs = "style" =: "color: blue;"

showMoney :: Money -> Text
showMoney (Money m) = "$" <> show m

