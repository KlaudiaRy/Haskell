module Main where

import qualified MainLib
import qualified Test.HUnit  as HU (Assertion, Test(..), assertEqual, assertBool, runTestTT, Counts, failures, errors)
import qualified Control.Monad.Writer (WriterT, tell, execWriter)
import qualified Control.Applicative (Applicative)
import qualified Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Functor.Identity (Identity)
import System.Exit (ExitCode(..),exitWith)

data MockIO m a = MockIO 
                              { runMessage :: Control.Monad.Writer.WriterT String m a }
 
instance Monad m => MainLib.MyIO (MockIO m) where
  putStrLn msg = MockIO $ do
                            Control.Monad.Writer.tell msg
                            return ()


mainWritesDownProperMessage :: HU.Test 
mainWritesDownProperMessage = HU.TestCase $ do 
                                    let message = Control.Monad.Writer.execWriter $ runMessage MainLib.main 
                                    HU.assertEqual "proper message" "Hello World!" message


data MockSimpleIO a = MockSimpleIO 
                              { getMessage :: String, dummy::a }
 
instance MainLib.MyIO MockSimpleIO where
  putStrLn msg = MockSimpleIO msg ()

instance Monad MockSimpleIO where
  return v = MockSimpleIO "" v
  MockSimpleIO msg a >>= f = MockSimpleIO (msg++newMsg) b
                      where
                       MockSimpleIO newMsg b = f a

mainWritesDownProperMessageSimple :: HU.Test 
mainWritesDownProperMessageSimple = HU.TestCase $ do 
                                    let message = getMessage MainLib.main 
                                    HU.assertEqual "proper message" "Hello World!" message

allTests :: HU.Test
allTests = HU.TestList [HU.TestLabel "main message" mainWritesDownProperMessage, HU.TestLabel "main message simple" mainWritesDownProperMessageSimple]

main :: IO Int
main = do
    results <-  HU.runTestTT allTests
    if (HU.errors results + HU.failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)


