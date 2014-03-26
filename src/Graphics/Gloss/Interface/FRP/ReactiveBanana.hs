{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module Graphics.Gloss.Interface.FRP.ReactiveBanana (playBanana, InputEvent) where

import Control.Monad
import Data.Fixed
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A useful type synonym for Gloss event values, to avoid confusion between
--   Gloss and ReactiveBanana.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided
--   Behavior t Picture changes.
playBanana :: Display -- ^ The display method
           -> Color   -- ^ The background colour
           -> Int     -- ^ The refresh rate, in Hertz
           -> Float   -- dt
           -> (forall t. Frameworks t
              => Event t Float
              -> Event t InputEvent
              -> Moment t (Behavior t Picture))
           -- ^ A Moment t action to generate the Picture Behavior, taking
           --   the refresh and input Events with respect to which to build it.
           --   The refresh event generates a Float indicating the time delta
           --   since the last refresh.
           -> IO ()
playBanana display colour frequency dt mPicture = do
  pictureref <- newIORef blank
  acc <- newIORef 0
  (tickHandler,  tick)  <- newAddHandler
  (eventHandler, event) <- newAddHandler
  compile (makeNetwork tickHandler eventHandler $ change pictureref) >>= actuate
  playIO display colour frequency ()
    (\      _ -> readIORef pictureref)
    (\ ev   _ -> event ev)
    (\ time _ -> readIORef acc >>= step time tick >>= writeIORef acc)
  where
    change :: IORef Picture -> Picture -> IO ()
    change = writeIORef
    makeNetwork tickHandler eventHandler change = do
      eTick  <- fromAddHandler tickHandler
      eEvent <- fromAddHandler eventHandler
      bPicture <- mPicture eTick eEvent
      changes bPicture >>= reactimate' . (fmap . fmap) change
      initial bPicture >>= liftIO . change
    step time tick acc = do
        let (n,acc2) = (time + acc) `divMod'` dt
        replicateM_ (fromIntegral n) $ tick time
        return acc2