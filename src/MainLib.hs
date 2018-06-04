module MainLib where

import qualified Prelude as P

class MyIO a where
  putStrLn :: P.String -> a ()

instance MyIO P.IO where
  putStrLn = P.putStrLn

main :: (MyIO a, P.Monad a) => a ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Goodbye!"
