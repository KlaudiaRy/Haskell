module MainLib where

import qualified Prelude as P

class MyIO a where
  putStrLn :: P.String -> a ()

instance MyIO P.IO where
  putStrLn = P.putStrLn

main :: MyIO a => a ()
main = putStrLn "Hello, Haskell!"
