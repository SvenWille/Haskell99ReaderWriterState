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
