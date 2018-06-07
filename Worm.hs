{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Module: Worm
Description: Module for whimsically illustrating progress in a terminal with an animated worm.

This module is a library to maybe keep command-line tool users slightly happier, by whimsically illustrating progress in slow-running parallel processes with a mildly entertaining animated worm that crawls slowly from left to right and back again, while displaying textual progress from flexible sources of input.
-}
module Worm where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, readMVar, isEmptyMVar, putMVar, swapMVar)
import Control.Monad (when, unless, forM_)
import Data.List (intercalate)
import System.IO (hPutStr, stderr)

wormWidth = 46
wormFrameDelayMillis = 230


-- * Showing Progress

-- | The 'Progress' type is a general wrapper for "Control.Concurrent.MVar"-ish things we can read and show next to the animation, while another (worker) thread updates them.
--   To add another form of showable progress information, declare a new type which implements 'Progress_', and a constructor (like 'rawProgress') to wrap that type in 'Progress'.
data Progress = forall p. Progress_ p => Progress p

-- | Class for formattable progress information types.
class Progress_ p where
  -- | A function that should (quickly) produce formatted progress information, for example by reading it from a wrapped 'Control.Concurrent.MVar.MVar'.
  formatProgress :: p -> IO String

instance Progress_ Progress where
  formatProgress (Progress p) = formatProgress p

newtype RawProgress = RawProgress (MVar String)
instance Progress_ RawProgress where
  formatProgress (RawProgress mV) = readMVar mV

-- | Displays (in brackets) a 'Control.Concurrent.MVar.MVar String' that you're free to update any way you choose.
rawProgress :: MVar String -> Progress
rawProgress s = Progress $ RawProgress s

newtype CollectingProgress = CollectingProgress (MVar Int, MVar Int)
instance Progress_ CollectingProgress where
  formatProgress (CollectingProgress (done, total)) = do
    done' <- readMVar done
    total' <- readMVar total
    return $ show done' ++ "/" ++ show total'

-- | Displays as (43/200), where either side of the fraction-wall can be independently updated.
collectingProgress :: MVar Int -> MVar Int -> Progress
collectingProgress done total = Progress $ CollectingProgress (done, total)


-- * Performing Animation

_wormAnimationFrames :: [String]  -- finite list of frames
_wormAnimationFrames =
  concatMap (\indent -> map (replicate indent ' ' ++) wormStates) [0,2..wormWidth]
  ++ map (replicate wormWidth ' ' ++) turnAround
  ++ concatMap (\indent -> map (replicate indent ' ' ++) wormStates2) [wormWidth,(wormWidth-2)..0]
  ++ (map (drop 2 . reverse) turnAround)
  where
    wormStates = ["______,  ", " __~__,  ", "  _/\\_,  ", "  __~__,  "]
    turnAround = ["  __~_,   ", "    _~,   ", "   ,_     ", "  ,~~_    ", "  ,__~_   "]
    wormStates2 = [" ,______  ", " ,__~__  ", " ,_/\\_  ", ",__~__  "]

-- | Action you can invoke to atomically stop/erase the worm before outputting real information, so your output goes unmangled.
type Canceller = IO ()

-- | Starts worm animation with given progress information; gives you a 'Canceller' to invoke when finished.
wormProgress :: [Progress] -> IO Canceller
wormProgress progress = do
  shouldStop <- newEmptyMVar
  stopped <- newEmptyMVar
  forkIO $ _showWorm shouldStop stopped 0 (cycle _wormAnimationFrames)  -- infinite list of frames
  return $ do
    putMVar shouldStop True
    readMVar stopped ; return ()

  where
    _showWorm shouldStop stopped prevLen (worm : nextWorms) = do  -- pick first frame off the list and draw it
      willStop <- not <$> isEmptyMVar shouldStop
      when willStop $ do
        hPutStr stderr $ "\r" ++ replicate prevLen ' ' ++ "\r\n"  -- TODO clear line with terminfo
        putMVar stopped True
      unless willStop $ do
        progressChunks <- mapM formatProgress progress
        let lineToDraw = worm ++ (intercalate " " $ map (("("++) . (++")")) $ filter (not.null) progressChunks)
        let lengthDiff = max 0 (prevLen - length lineToDraw)
        hPutStr stderr $ "\r" ++ lineToDraw ++ replicate (lengthDiff+2) ' ' ++ replicate lengthDiff '\b'
        threadDelay $ wormFrameDelayMillis*1000
        _showWorm shouldStop stopped (length lineToDraw) nextWorms  -- draw next (and in turn, all remaining) frames


-- | An example of basic usage.
main = do
  progress <- newMVar "loading..."  -- set up progress variable to report to worm-drawing thread
  stopWorm <- wormProgress [rawProgress progress]
  forM_ [0..300] $ \n -> do  -- simulate long process
    let paddedPercent = reverse $ take 3 $ (reverse . show . floor $ n/3.0) ++ repeat ' '
    swapMVar progress $ "loading (" ++ paddedPercent ++ "%)..."
    threadDelay $ 80*1000
  stopWorm
