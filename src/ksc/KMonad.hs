-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# OPTIONS_GHC -Wno-unused-matches #-}

module KMonad where

import Control.Monad.State hiding (liftIO)
import qualified Control.Monad.State

newtype KMT m a = KM { unKM :: StateT Uniq m a }

-- We use
--  * Uniq state so we can allocate fresh names
--  * IO so we can print debug output and type checker errors
type KM = KMT IO

instance Monad m => Functor (KMT m) where
  fmap f km = do { r <- km; return (f r) }

instance Monad m => Applicative (KMT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (KMT m) where
  return = KM . return
  KM km >>= k  = KM $ do (km >>= (unKM . k))

getUniq :: Monad m => KMT m Uniq
getUniq = KM (do { uniq <- get
                 ; put (uniq+1)
                 ; return uniq })

setUniq :: Monad m => Uniq -> KMT m ()
setUniq uniq = KM (put uniq)

runKM :: Monad m => KMT m a -> m a
runKM (KM km) = evalStateT km initialUniq

liftIO :: IO a -> KM a
liftIO io = KM (Control.Monad.State.liftIO io)

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
