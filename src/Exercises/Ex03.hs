{-# LANGUAGE RecursiveDo, NamedFieldPuns, TypeApplications #-}
{-|
Description: Solutions to https://qfpl.io/posts/reflex/basics/exercises/dynamics/index.html
-}
module Exercises.Ex03
  ( runEx03
  )
where

import           Data.Default.Class             ( def )
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Buttons             ( mkButtonConstText )
import           Protolude               hiding ( Product )
import qualified Reflex.Dom                    as RD
import           Exercises.Shared

runEx03 :: IO ()
runEx03 = RD.mainWidget $ do
  rec inputs                  <- dispInputs oeVend
      outs@Outputs { oeVend } <- ex03 inputs
  void $ dispOutputs outs

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
  }

data VendingErr = InsufficientFunds Money Money | InsufficientStock
                deriving (Eq, Show)

dispInputs
  :: (RD.DomBuilder t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m)
  => RD.Event t Product
  -> m (Inputs t)
dispInputs eVend = do
  let -- name of the product that was bought 
      eCarrot   = RD.ffilter (== carrot) eVend
      eCelery   = RD.ffilter (== celery) eVend
      eCucumber = RD.ffilter (== cucumber) eVend

  rec eAddMoney <- mkButtonConstText mempty "Add money"

      idMoney   <- RD.foldDyn ($) 0 $ RD.mergeWith
        (.)
        [eAddMoney $> (+ 1), ieRefund $> const 0, eVend <&> flip (-) . pCost]

      idCarrot        <- mkStock def eCarrot
      idCucumber      <- mkStock def eCucumber
      idCelery        <- mkStock def eCelery

      eSelectCarrot   <- dispProductStock carrot idCarrot
      eSelectCucumber <- dispProductStock cucumber idCucumber
      eSelectCelery   <- dispProductStock celery idCelery

      let eSelect = RD.leftmost [eSelectCarrot, eSelectCucumber, eSelectCelery]
      idSelected <- RD.holdDyn (ProductStock preselected def) eSelect

      ieRefund   <- mkButtonConstText mempty "Refund"
      ieBuy      <- mkButtonConstText mempty "Buy"

  pure Inputs { .. }
 where
  mkStock init = RD.foldDyn ($) init . reduceWith
  reduceWith e = RD.mergeWith (.) [e $> flip (-) 1]
  preselected = carrot

dispOutputs Outputs {..} = dispChange >> dispVend >> dispError
 where
  dispChange = withinDiv $ do
    RD.el "span" $ RD.text "Change: "
    RD.dynText $ showMoney <$> odChange
  dispVend = withinDiv $ do
    RD.el "span" $ RD.text "Last bought: "
    RD.dynText $ maybe "" show <$> odVend
  dispError = withinDiv $ RD.holdDyn "" eError
  withinDiv = RD.elClass "div" "output"
  eError    = RD.leftmost [oeVend $> "", oeError <&> show @VendingErr @Text]

ex03
  :: (RD.Reflex t, RD.MonadHold t m, RD.PostBuild t m, RD.DomBuilder t m)
  => Inputs t
  -> m (Outputs t)
ex03 Inputs {..} =
  let eBuyAttempt     = RD.tag ibSelectedMoney ieBuy <&> uncurry checkAttempt
      oeError         = RD.filterLeft eBuyAttempt
      oeBuy           = RD.filterRight eBuyAttempt
      oeVend          = oeBuy <&> psProduct
      oeChange        = RD.tag bMoney ieRefund
      ibSelectedMoney = (,) <$> bMoney <*> bSelected
  in  do
        odVend   <- RD.holdDyn Nothing (Just <$> oeVend)
        odChange <- RD.holdDyn 0 oeChange
        dispError oeError
        pure Outputs { .. }

 where
  dispError eErr = RD.dynText =<< RD.holdDyn "" (show <$> eErr)
  bMoney    = RD.current idMoney
  bSelected = RD.current idSelected
  checkAttempt money ps@ProductStock {..}
    | psStock <= 0 = Left InsufficientStock
    | money <= 0   = Left $ InsufficientFunds money (pCost psProduct)
    | otherwise    = Right ps
