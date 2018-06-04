module Main where

import qualified MainLib
import qualified Test.HUnit  as HU (Assertion, Test(..), assertEqual, assertBool, runTestTT, Counts, failures, errors)
import qualified Control.Monad.Writer (WriterT, tell, execWriter)
import qualified Control.Applicative (Applicative)
import qualified Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Functor.Identity (Identity)
import System.Exit (ExitCode(..),exitWith)

data MockSimpleIO a = MockSimpleIO 
                              { getMessages :: [String], dummy::a }


 
instance MainLib.MyIO MockSimpleIO where
  putStrLn msg = MockSimpleIO [msg] ()

instance Functor MockSimpleIO where
  fmap f (MockSimpleIO msgs a) = MockSimpleIO msgs (f a)

instance Applicative MockSimpleIO where 
  pure v = MockSimpleIO [] v
  MockSimpleIO msgs1 f <*> MockSimpleIO msgs2 a = MockSimpleIO (msgs1 ++ msgs2) (f a)

instance Monad MockSimpleIO where
  return v = MockSimpleIO [] v
  MockSimpleIO msg a >>= f = MockSimpleIO (msg++newMsg) b
                      where
                       MockSimpleIO newMsg b = f a

mainWritesDownProperMessageSimple :: HU.Test 
mainWritesDownProperMessageSimple = HU.TestCase $ do 
                                    let messages = getMessages MainLib.main 
                                    HU.assertEqual "proper message" "Hello, Haskell!" (messages !! 0)
                                    HU.assertEqual "proper message" "Goodbye!" (messages !! 1)

allTests :: HU.Test
allTests = HU.TestList [HU.TestLabel "main message simple" mainWritesDownProperMessageSimple]

main :: IO Int
main = do
    results <-  HU.runTestTT allTests
    if (HU.errors results + HU.failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)


