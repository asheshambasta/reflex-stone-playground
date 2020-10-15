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
  , ibSelected :: RD.Dynamic t Text
  , ieBuy      :: RD.Event t ()
  , ieRefund   :: RD.Event t ()
  }

dispInputs :: (RD.DomBuilder t m, RD.MonadHold t m, MonadFix m) => m (Inputs t)
dispInputs = do
  eAddMoney <- mkButtonConstText mempty "Add money"
  idMoney   <- RD.foldDyn ($) 0 $ RD.mergeWith (.) [eAddMoney $> (+ 1)]
  pure Inputs { .. }
