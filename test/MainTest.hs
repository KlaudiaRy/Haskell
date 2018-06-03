module Main where

import qualified MainLib
import qualified Test.HUnit (Assertion, Test(..), assertEqual, assertBool, runTestTT)
import qualified Test.HUnit.Base (Counts)
import qualified Control.Monad.Writer (WriterT, tell, execWriter)
import qualified Control.Applicative (Applicative)
import qualified Control.Monad.Trans.Class (MonadTrans)
import qualified Data.Functor.Identity (Identity)

data MockIO m a = MockIO 
                              { runMessage :: Control.Monad.Writer.WriterT String m a }
         --                     deriving (Control.Applicative.Applicative, Functor, Monad, Control.Monad.Trans.Class.MonadTrans)

--instance Monad m => Main.MyIO (MockIO m) where
instance Monad m => MainLib.MyIO (MockIO m) where
  putStrLn msg = MockIO $ do
                            Control.Monad.Writer.tell msg
                            return ()


mainWritesDownProperMessage :: Test.HUnit.Test 
mainWritesDownProperMessage = Test.HUnit.TestCase $ do 
                                    let message = Control.Monad.Writer.execWriter $ runMessage MainLib.main 
                                    Test.HUnit.assertEqual "proper message" "Hello World!" message

allTests :: Test.HUnit.Test
allTests = Test.HUnit.TestList [mainWritesDownProperMessage]

main :: IO Test.HUnit.Base.Counts
main = Test.HUnit.runTestTT allTests


