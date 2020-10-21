{-# LANGUAGE RecursiveDo, RankNTypes #-}
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

import qualified Data.Map                      as M
import           Data.Default.Class             ( Default(..) )
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Clicks              ( clickEvent' )
import           Lib.Reflex.Buttons             ( mkButtonConstTextClass )
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
  , pImg  :: Text
  }
  deriving (Eq, Show, Ord)

instance Default Product where
  def = carrot

instance Default ProductStock where
  def = ProductStock def def

data ProductStock = ProductStock
  { psProduct :: Product
  , psStock   :: Stock
  }
  deriving (Eq, Show)

carrot :: Product
carrot = Product
  "Carrot"
  1
  "https://i5.walmartimages.ca/images/Enlarge/686/686/6000198686686.jpg"

celery :: Product
celery = Product
  "Celery"
  2
  "https://i5.walmartimages.ca/images/Enlarge/094/529/6000200094529.jpg"

cucumber :: Product
cucumber = Product
  "Cucumber"
  3
  "https://cdn.mos.cms.futurecdn.net/EBEXFvqez44hySrWqNs3CZ.jpg"

dispMoney :: (PostBuild t m, DomBuilder t m) => Dynamic t Money -> m ()
dispMoney dMoney = elDynClass "span" dBadgeClass
  $ dynText (showMoney <$> dMoney)
 where
  dBadgeClass = dMoney
    <&> \m -> if m > 0 then "badge badge-success" else "badge badge-warning"

dispProduct :: DomBuilder t m => Product -> m (Event t ())
dispProduct Product {..} = elAttr "div" cardAttrs $ do
  elAttr "img" imgAttrs $ text ""
  elClass "div" "card-body" . elClass "h5" "card-title" $ text pName
  mkButtonConstTextClass "btn btn-primary" mempty $ "Buy: " <> showMoney pCost
  where imgAttrs = ("class" =: "card-img-top") <> ("src" =: pImg)

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
  -> Bool
  -> Dynamic t Stock
  -> m (Event t Product)
dispProductStock prod@Product {..} preSel dStock =
  elAttr "div" cardAttrs $ dispProduct prod >> dispStock >> dispSelectRadio
 where
  dispStock =
    elClass "h6" "card-subtitle mb-2 text-muted"
      .   dynText
      $   mappend "Stock: "
      .   show
      <$> dStock
  dispSelectRadio = do
    rec
      el "div" $ pure radio
      (radio, _) <-
        elAttr' "input" inputAttrs . elAttr "label" labelAttrs $ text ""
    pure $ clickEvent' radio $> prod
   where
    labelAttrs = "for" =: id
    inputAttrs =
      ("type" =: "radio")
        <> ("id" =: id)
        <> ("name" =: "prodSelect")
        <> if preSel then "checked" =: "" else mempty
    id = pName

cardAttrs :: M.Map Text Text
cardAttrs = ("class" =: "card") <> ("style" =: "width: 18rem;")
