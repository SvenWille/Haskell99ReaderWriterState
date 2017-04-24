module Haskell99.P01
    (
    ) where

import Control.Monad.Reader


--basic example , crashes on empty lists
p01_1 :: [a] -> a
p01_1 ls = runReader readerFunction ls
  where
    readerFunction :: Reader [a] a
    readerFunction = do
      list <- ask
      return (last list)


--safe version
p01_2 :: [a] -> Maybe a
p01_2 ls = runReader readerFunction ls
  where
    readerFunction :: Reader [a] (Maybe a)
    readerFunction = do
      list <- ask
      if null list
        then return Nothing
        else return (Just $ last list)


--without the syntactic sugar (simple version)
p01_3 :: [a] -> a
p01_3 ls = runReader readerFunction ls
  where
    readerFunction :: Reader [a] a
    readerFunction  = ask >>= return . last --or use fmap last ask


p01_4 :: [a] -> Maybe a
p01_4 ls = runReader readerFunction ls
  where
    readerFunction :: Reader [a] (Maybe a)
    readerFunction = ask >>= return . (\list -> if null list then Nothing else Just $ last list)  
