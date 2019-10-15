-- Copyright (c) Microsoft Corporation.
-- Licensed under the MIT license.
{-# OPTIONS_GHC -Wno-unused-matches #-}

module KMonad where

import Control.Monad.State hiding (liftIO)
import qualified Control.Monad.State

-- We use
--  * Uniq state so we can allocate fresh names
--  * IO so we can print debug output and type checker errors
newtype KM a = KM { unKM :: StateT Uniq IO a }

instance Functor KM where
  fmap f km = do { r <- km; return (f r) }

instance Applicative KM where
  pure  = return
  (<*>) = ap

instance Monad KM where
  return = KM . return
  KM km >>= k  = KM $ do (km >>= (unKM . k))

getUniq :: KM Uniq
getUniq = KM (do { uniq <- get
                 ; put (uniq+1)
                 ; return uniq })

setUniq :: Uniq -> KM ()
setUniq uniq = KM (put uniq)

runKM :: KM a -> IO a
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

