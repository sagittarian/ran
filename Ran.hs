module Ran where

-- import Control.Applicative
import Control.Monad.State

ia :: Int
ia = 16807

im :: Int
im = 2147483647

type Seed = Int

-- The next seed in the series
nextSeed :: Seed -> Seed
nextSeed seed = (ia * seed) `mod` im

-- A pseudorandom number
ran0 :: State Seed Seed
-- ran0 = state $ join (,) . nextSeed
ran0 = modify nextSeed >> get

-- A pseudorandom number between 0 and 1
ran01 :: State Seed Float
ran01 = ran0 >>= \val ->
  return $ fromIntegral val / fromIntegral im

-- A state representing advancing the seed n times
ranx :: Int -> State Seed Float
ranx 1 = ran01
ranx n = ran01 >> ranx (n-1)

-- An infinite list of random numbers, given a seed
ranList :: Seed -> [Float]
ranList seed =
  let (start, next) = runState ran01 seed
  in start : ranList next

-- other implementations:
-- ranList seed = start : rest
--   where
--     (start, next) = runState ran01 seed
--     rest = ranList next
-- ranList seed = evalState ran01 seed : ranList (nextSeed seed)
-- ranList seed = map applySeed stateList
--    where
--      stateList = iterate (>> ran01) ran01
--      applySeed = fst . ($ seed) . runState

-- not sure how to get this one to work
-- ranList :: Seed -> [Float]
-- ranList seed = (:) <$> ran01 <*> return (ranList (nextSeed seed))

