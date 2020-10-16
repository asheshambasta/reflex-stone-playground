{-# LANGUAGE RecursiveDo #-}
{-|
Description: Solutions to https://qfpl.io/posts/reflex/basics/exercises/dynamics/index.html
-}
module Exercises.Ex03
  ( runEx03
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Buttons             ( mkButtonConstText )
import           Protolude               hiding ( Product )
import qualified Reflex.Dom                    as RD
import           Exercises.Shared

runEx03 :: IO ()
runEx03 = RD.mainWidget $ do
  dispInputs $> ()

data Inputs t = Inputs
  { idMoney    :: RD.Dynamic t Money
  , idCarrot   :: RD.Dynamic t Stock
  , idCelery   :: RD.Dynamic t Stock
  , idCucumber :: RD.Dynamic t Stock
  , idSelected :: RD.Dynamic t Text
  , ieBuy      :: RD.Event t ()
  , ieRefund   :: RD.Event t ()
  }

dispInputs :: (RD.DomBuilder t m, RD.MonadHold t m, MonadFix m) => m (Inputs t)
dispInputs = do
  rec eAddMoney  <- mkButtonConstText mempty "Add money"
      idMoney    <- RD.foldDyn ($) 0 $ RD.mergeWith (.) [eAddMoney $> (+ 1)]
      idCarrot   <- RD.foldDyn ($) 5 $ reduceWith eCarrot
      idCucumber <- RD.foldDyn ($) 5 $ reduceWith eCucumber
      idCelery   <- RD.foldDyn ($) 5 $ reduceWith eCelery
      idSelected <- undefined -- radio
      ieBuy      <- mkButtonConstText mempty "Buy"

      let ibSelected = RD.current idSelected
          -- name of the product that was bought 
          eBought    = RD.tag ibSelected ieBuy
          eCarrot    = RD.ffilter (== carrotName) eBought
          eCelery    = RD.ffilter (== celeryName) eBought
          eCucumber  = RD.ffilter (== cucumberName) eBought

      ieRefund <- mkButtonConstText mempty "Refund"

  pure Inputs { .. }
 where
  reduceWith e = RD.mergeWith (.) [e $> flip (-) 1]
  carrotName   = pName carrot
  celeryName   = pName celery
  cucumberName = pName cucumber
