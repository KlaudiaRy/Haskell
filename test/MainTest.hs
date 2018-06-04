module Main where

import qualified MainLib
import qualified Test.HUnit  as HU (Assertion, Test(..), assertEqual, assertBool, runTestTT, Counts)
import qualified Control.Monad.Writer (WriterT, tell, execWriter)
import qualified Control.Applicative (Applicative)
import qualified Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Functor.Identity (Identity)

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

allTests :: HU.Test
allTests = HU.TestList [HU.TestLabel "main message" mainWritesDownProperMessage]

main :: IO HU.Counts
main = HU.runTestTT allTests


