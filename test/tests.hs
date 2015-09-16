import Test.Tasty
import Test.Tasty.HUnit

import qualified Graphics.PlotFont as PF

import Data.Either

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ renders, substs ]

renders :: TestTree
renders = testGroup "Renders"
  [ canRender "Hello World"
  , cantRender "£"
  ]

canRender  = pfTry PF.render "Can't render" isRight
cantRender = pfTry PF.render "Didn't fail"  isLeft

substs :: TestTree
substs = testGroup "Substitutions"
  [  pfTry PF.render' "Didn't sub missing char" isSubst          "£"
  ,  pfTry PF.render' "Sub'ed extant char"      (not . isSubst)  "A"
  ]

isSubst = (== q)
     where (Right q) = PF.render PF.canvastextFont "?"

pfTry :: (PF.PlotFont -> String -> a) -> String -> (a -> Bool) -> String -> TestTree                       
pfTry f msg p x = testCase x $ assertBool msg $ p $ f PF.canvastextFont x

