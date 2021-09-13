module Test where

import ConsoleIO
import Control.Exception (throw)
import Control.Monad.State
import Control.Monad.Writer
import Greeter
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Text.Read (readMaybe)

newtype TestM a = TestM (WriterT [String] (State [String]) a)
  deriving newtype (Functor, Applicative, Monad, MonadState [String], MonadWriter [String])

runTestM :: [String] -> TestM a -> (a, [String], [String])
runTestM input (TestM t) = (x, input', output)
  where
    ((x, output), input') = runState (runWriterT t) input

instance ConsoleIO TestM where
  writeLine = tell . pure
  readLine =
    get >>= \case
      [] -> throw $ userError "stdin empty"
      x : xs -> x <$ put xs

test_oldness :: TestTree
test_oldness =
  testGroup
    "oldness"
    [ testProperty "very young" $ property do
        age <- forAll . Gen.integral $ Range.constant 0 17
        oldness age === "very young",
      testProperty "young" $ property do
        age <- forAll . Gen.integral $ Range.constant 18 34
        oldness age === "young",
      testProperty "old" $ property do
        age <- forAll . Gen.integral $ Range.constant 35 64
        oldness age === "old",
      testProperty "very old" $ property do
        age <- forAll . Gen.integral $ Range.constant 65 120
        oldness age === "very old"
    ]

test_getName :: TestTree
test_getName =
  testGroup
    "getName"
    [ testProperty "name matches" $ property do
        input <- forAll genInput
        name <- forAll genName
        let (name', _, _) = runTestM (name : input) getName
        name === name',
      testProperty "consumes stdin" $ property do
        input <- forAll genInput
        name <- forAll genName
        let (_, remaining, _) = runTestM (name : input) getName
        remaining === input,
      testProperty "outputs prompt" $ property do
        input <- forAll genInput
        let (_, _, output) = runTestM input getName
        output === ["Please enter your name:"]
    ]

test_getAge :: TestTree
test_getAge =
  testGroup
    "getAge"
    [ testProperty "age is parsed" $ property do
        age <- forAll genAge
        input <- forAll genInput
        let (age', _, _) = runTestM (show age : input) getAge
        age' === Just age,
      testProperty "age fails to parse" $ property do
        age <- forAll genJunk
        input <- forAll genInput
        let (age', _, _) = runTestM (age : input) getAge
        age' === Nothing
    ]

hprop_greet_success :: Property
hprop_greet_success = property do
  name <- forAll genName
  goodAge <- forAll genAge
  badAge <- forAll genJunk
  input <- forAll genInput
  let ((), remaining, output) = runTestM (name : badAge : show goodAge : input) greet
  remaining === input
  output
    === [ "Please enter your name:",
          "Please enter your age:",
          "Please enter your age:",
          "Hello " <> name <> "! You are " <> oldness goodAge <> "!"
        ]

genAge :: Gen Int
genAge = Gen.integral (Range.linear 0 100)

genJunk :: Gen String
genJunk = do
  junk <- Gen.string (Range.linear 0 100) Gen.ascii
  case readMaybe @Int junk of
    Nothing -> pure junk
    Just _ -> genJunk

genName :: Gen String
genName = Gen.string (Range.linear 0 100) Gen.unicode

genInput :: Gen [String]
genInput = Gen.list (Range.linear 1 100) genName
