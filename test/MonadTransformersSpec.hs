module MonadTransformersSpec where

import Test.Hspec
import MonadTransformers


spec :: Spec
spec = 
    describe "MonadTransformers" $
        it "dummy" $
            1 `shouldBe` 1