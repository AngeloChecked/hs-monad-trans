module ExerciseSpec where

import Test.Hspec 
import Exercise ( rDec, rPrintAndInc, rShow, sPrintIncAccum )
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class

spec :: Spec
spec = 
    describe "MonadT" $ do
        it "rdec should get its argument inthe context of reader and return a value decremented by one" $
            runReader rDec 1 `shouldBe` 0 
        it "rDec" $ 
            fmap (runReader rDec) [1..10] `shouldBe` [0,1,2,3,4,5,6,7,8,9]
        it "rShow" $
            runReader rShow 1 `shouldBe` "1"
        it "rShow2" $
            fmap (runReader rShow) [1..10] `shouldBe` ["1","2","3","4","5","6","7","8","9","10"]
        it "traverse rPrintAndInc" $
            do
                x <- traverse (runReaderT rPrintAndInc) [1..10]
                x `shouldBe` [2,3,4,5,6,7,8,9,10,11]
        it "rPrintAndInc" $
            do
                x <- runReaderT rPrintAndInc 1
                x `shouldBe` 2
        it "mapM sPrintIncAccum" $
            do
                x <- mapM (runStateT sPrintIncAccum) [1..5] 
                x `shouldBe` [("1",2),("2",3),("3",4),("4",5),("5",6)]
        it "sPrintIncAccum" $
            do
                x <- runStateT sPrintIncAccum 10 
                x `shouldBe` ("10",11)

        
        
