module Haskell99.Reader.P01
    (
    ) where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe


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


--using local
p01_5 :: [a] -> a
p01_5 = runReader readerFunction
  where
    readerFunction ::  Reader [a] a
    readerFunction  = local reverse ( ask >>= return . head )


p01_6 :: [a] -> a
p01_6 = runReader (withReader reverse readerFunction)
  where
    readerFunction ::  Reader [a] a
    readerFunction  = ask >>= return . head



p01_7 :: [a] -> a
p01_7 = runReader readerFunction
  where
    readerFunction ::  Reader [a] a
    readerFunction  =  reader (head . reverse)


p01_8 :: [a] -> Maybe a
p01_8  = runReader (runMaybeT readerFunction)
  where
    readerFunction ::  (MaybeT (Reader [a] ) a)
    readerFunction = do
      val <- lift ask
      return $ last val

p01_9 :: [a] -> a
p01_9  = runReader readerFunction
  where
    readerFunction :: Reader [a] a
    readerFunction = asks last

{-
p01_10 :: [a] -> a
p01_10 = runReader readerFunction
  where
  -}
