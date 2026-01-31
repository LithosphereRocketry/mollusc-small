import Prelude

import Test.Tasty

import qualified Tests.CPU.SimpleSOC

main :: IO ()
main = defaultMain $ testGroup "."
  [
    Tests.CPU.SimpleSOC.socTests
  ]
