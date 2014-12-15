{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Control.Monad
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving Show

nRoll :: Int -> Rand StdGen [DieValue]
nRoll n = replicateM n die >>= \ds -> return (sortBy (flip compare) ds)

pairRoll :: Int -> Int -> Rand StdGen [(DieValue, DieValue)]
pairRoll a b = mas >>= \as ->
               mbs >>= \bs ->
               return (zip as bs)
    where
        mas = nRoll a
        mbs = nRoll b

judge :: Rand StdGen [(DieValue, DieValue)] -> Rand StdGen [Bool]
judge mprs = mprs >>= \prs -> mapM (\(a, b) -> return (a > b)) prs

countBool :: [Bool] -> (Int, Int)
countBool = foldr (\x (a1, a2)-> if x then (a1+1, a2) else (a1, a2+1)) (0,0)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield ats dfs) = mn >>= \(na, nd) -> return (Battlefield (ats - nd) (dfs - na))
    where
        mn = judge (pairRoll ats dfs) >>= \js -> return (countBool js)
