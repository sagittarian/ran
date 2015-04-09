module Ran where

-- import Control.Applicative
import Control.Monad.State

ia :: Int
ia = 16807

im :: Int
im = 2147483647

type Seed = Int

-- Calculate the next seed in the series
nextSeed :: Seed -> Seed
nextSeed seed = (ia * seed) `mod` im

-- A pseudorandom number
ran0 :: State Seed Seed
-- ran0 = state $ join (,) . nextSeed
ran0 = modify nextSeed >> get

-- A pseudorandom number from [0,1)
ran01 :: State Seed Float
ran01 = ran0 >>= \val ->
  return $ fromIntegral val / fromIntegral im

-- A state representing advancing the seed n times
-- (ignoring the values generated)
ranx :: Int -> State Seed Float
ranx 1 = ran01
ranx n = ran01 >> ranx (n-1)

-- An infinite list of random items, given a State
ranList :: State Seed a -> Seed -> [a]
ranList s = evalState $ sequence (repeat s)

-- infinite list from [0,1), given a seed
ranList01 :: Seed -> [Float]
ranList01 = ranList ran01

-- State representing taking a random element between lower and upper
-- of an Enum (including both endpoints)
ranRange :: (Enum a) => a -> a -> State Seed a
ranRange lower upper = do
  ran <- ran01
  let rangeIdx = floor (fromIntegral range * ran + 0.5)
      result = toEnum $ rangeIdx + lowerIdx
  return result
  where
    lowerIdx = fromEnum lower
    upperIdx = fromEnum upper
    range = upperIdx - lowerIdx

-- State representing choosing a random item from a list
ranChoice :: [a] -> State Seed a
ranChoice xs = do
  ran <- ran01
  let idx = floor (ran * fromIntegral (length xs))
  return $ xs !! idx

-- ranShuffle -- XXX should be interesting

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
-- ranList seed =
--   let (start, next) = runState ran01 seed
--   in start : ranList next



-- not sure how to get this one to work
-- ranList :: Seed -> [Float]
-- ranList seed = (:) <$> ran01 <*> return (ranList (nextSeed seed))

