import Test.Tasty
import qualified ParserSpec as PS

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [PS.unitTests]
