{-# LANGUAGE RecursiveDo, NamedFieldPuns, TypeApplications #-}
{-|
Description: Solutions to https://qfpl.io/posts/reflex/basics/exercises/dynamics/index.html
-}
module Exercises.Ex03
  ( runEx03
  )
where

import qualified Data.Text                     as T
import           Data.Default.Class             ( def )
import qualified Data.Map                      as M
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Buttons             ( mkButtonConstText
                                                , mkButtonConstTextClass
                                                )
import           Protolude               hiding ( Product )
import qualified Reflex.Dom                    as RD
import           Reflex.Dom                     ( (=:) )
import           Exercises.Shared
import           Exercises.Shared.Widgets       ( mainWidgetWithBulma )
import           Exercises.Shared.Widgets.Bulma ( sectionContainer )

runEx03 :: IO ()
runEx03 = mainWidgetWithBulma $ do
  rec inputs                  <- sectionContainer . dispInputs $ oeVend
      outs@Outputs { oeVend } <- ex03 inputs
  sectionContainer . dispOutputs $ outs

data Inputs t = Inputs
  { idMoney    :: RD.Dynamic t Money
  , idCarrot   :: RD.Dynamic t Stock
  , idCelery   :: RD.Dynamic t Stock
  , idCucumber :: RD.Dynamic t Stock
  , idSelected :: RD.Dynamic t ProductStock
  , ieBuy      :: RD.Event t ()
  , ieRefund   :: RD.Event t ()
  }

data Outputs t = Outputs
  { oeVend   :: RD.Event t Product
  , oeChange :: RD.Event t Money
  , oeError  :: RD.Event t VendingErr
  , odChange :: RD.Dynamic t Money
  , odVend   :: RD.Dynamic t (Maybe Product)
  , odCart   :: RD.Dynamic t (M.Map Product Int)
  }

data VendingErr = InsufficientFunds Money Money | InsufficientStock
                deriving (Eq, Show)

dispInputs
  :: (RD.DomBuilder t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m)
  => RD.Event t Product
  -> m (Inputs t)
dispInputs eVend = do

  RD.elClass "h3" "title" $ RD.text "Choose an item."

  idCarrot        <- mkStock def eCarrot
  idCucumber      <- mkStock def eCucumber
  idCelery        <- mkStock def eCelery

  eSelectCarrot   <- dispProductStock carrot True idCarrot
  eSelectCucumber <- dispProductStock cucumber False idCucumber
  eSelectCelery   <- dispProductStock celery False idCelery

  eAddMoney       <- mkButtonConstTextClass "btn-success" mempty "Add money"

  rec
    idMoney <- RD.foldDyn ($) 0 $ RD.mergeWith
      (.)
      [eAddMoney $> (+ 1), ieRefund $> const 0, eVend <&> subtract . pCost]

    dispMoney idMoney

    idSelectedProd <- RD.holdDyn def eSelect

    let eSelect = RD.leftmost [eSelectCarrot, eSelectCucumber, eSelectCelery]
        idSelected =
          idSelectedProd >>= matchProduct idCarrot idCucumber idCelery

    ieRefund <- mkButton "Refund"
    ieBuy    <- mkButton "Buy"

  pure Inputs { .. }
 where
  mkStock init = RD.foldDyn ($) init . reduceWith
  reduceWith e = e $> subtract 1
  -- events indicating the product that was bought.
  eCarrot   = RD.ffilter (== carrot) eVend
  eCelery   = RD.ffilter (== celery) eVend
  eCucumber = RD.ffilter (== cucumber) eVend
  mkButton  = RD.elClass "div" "action-button" . mkButtonConstText mempty
  -- this is ugly but will do for now.
  matchProduct idCarrot idCucumber idCelery p
    | p == carrot   = ProductStock p <$> idCarrot
    | p == cucumber = ProductStock p <$> idCucumber
    | otherwise     = ProductStock p <$> idCelery

dispOutputs
  :: (RD.DomBuilder t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m)
  => Outputs t
  -> m ()
dispOutputs Outputs {..} =
  void . RD.elClass "nav" "level" $ dispChange >> dispVend >> dispError
 where
  dispChange = withinDiv $ withinPs "Change" (showMoney <$> odChange)
  dispVend =
    withinDiv $ withinPs "Last bought" (maybe "--" showProduct <$> odVend)
    -- withinDiv $ do
    --   RD.el "span" $ RD.text "Items:"
    --   RD.dynText (dispCart <$> odCart)
  -- dispCart = M.foldrWithKey
  --   (\prod num msg -> T.unwords [msg, "\n", showProduct prod, "x", show num])
  --   ""
  dispError = do
    dError <- RD.holdDyn "" eError
    withinDiv $ withinPs "" dError
  withinDiv = RD.elClass "div" "level-item has-text-centered" . RD.el "div"
  withinPs heading title =
    RD.elClass "p" "heading" (RD.text heading)
      >> RD.elClass "p" "title" (RD.dynText title)
  eError = RD.leftmost [oeVend $> "", oeError <&> show @VendingErr @Text]

ex03
  :: ( RD.Reflex t
     , MonadFix m
     , RD.MonadHold t m
     , RD.PostBuild t m
     , RD.DomBuilder t m
     )
  => Inputs t
  -> m (Outputs t)
ex03 Inputs {..} =
  let oeError  = RD.filterLeft eBuyAttempt
      oeBuy    = RD.filterRight eBuyAttempt
      oeVend   = oeBuy <&> psProduct
      oeChange = RD.tag bMoney ieRefund
  in  do
        odVend   <- RD.holdDyn Nothing (Just <$> oeVend)
        odChange <- RD.holdDyn 0 oeChange
        odCart   <- RD.foldDyn updateCount mempty oeVend
        dispError
        pure Outputs { .. }

 where
  updateCount     = M.alter $ Just . maybe 1 (+ 1)
  eBuyAttempt     = RD.tag ibSelectedMoney ieBuy <&> checkAttempt
  ibSelectedMoney = (,) <$> bMoney <*> bSelected
  dispError =
    let eErrText = eBuyAttempt <&> either show (const "")
        errAttrs = "style" =: "background-color: red;"
    in  RD.elAttr "div" errAttrs . RD.dynText =<< RD.holdDyn "" eErrText
  bMoney    = RD.current idMoney
  bSelected = RD.current idSelected
  checkAttempt (money, ps@ProductStock {..})
    | psStock <= 0 = Left InsufficientStock
    | money < cost = Left $ InsufficientFunds money cost
    | otherwise    = Right ps
    where cost = pCost psProduct

