module ParsingSpec where

import App
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "parsing" $ do
 describe "group n" $ do
     it "" $
        1 `shouldBe` 1

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])
