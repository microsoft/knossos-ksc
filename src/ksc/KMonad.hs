-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# OPTIONS_GHC -Wno-unused-matches #-}

module KMonad where

import Data.IORef
import Control.Monad

newtype KM a = KM { unKM :: IORef Uniq -> IO a }

instance Functor KM where
  fmap f km = do { r <- km; return (f r) }

instance Applicative KM where
  pure  = return
  (<*>) = ap

instance Monad KM where
  return x = KM (\_ -> return x)
  KM km >>= k  = KM (\ur -> do { a <- km ur
                              ; unKM (k a) ur })

getUniq :: KM Uniq
getUniq = KM (\ur -> do { uniq <- readIORef ur
                        ; writeIORef ur (uniq+1)
                        ; return uniq })

setUniq :: Uniq -> KM ()
setUniq uniq = KM (\ur -> writeIORef ur uniq)

runKM :: KM a -> IO a
runKM (KM km) = do { ur <- newIORef initialUniq
                   ; km ur }

liftIO :: IO a -> KM a
liftIO io = KM (\ur -> io)

-----------------------------
banner :: String -> KM ()
banner s
  = liftIO $
    do { putStrLn "\n----------------------------"
       ; putStrLn s
       ; putStrLn "----------------------------\n" }


-----------------------------
type Uniq = Int

initialUniq :: Uniq
initialUniq = 1

