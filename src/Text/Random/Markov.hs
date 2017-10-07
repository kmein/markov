module Text.Random.Markov where

import Control.Arrow (first)

import Data.Char (isUpper)
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.Random (RandomGen, randomR, newStdGen)

type TextWord = T.Text

type TextCorpus = V.Vector TextWord

type TextCache = M.Map (TextWord, TextWord) (M.Map TextWord Int)

weightedChoice :: (RandomGen g) => g -> M.Map a Int -> (a, g)
weightedChoice g = choice g . weighted
    where
      weighted = V.concatMap (uncurry $ flip V.replicate) . V.fromList . M.toList
      choice g xs = (xs !) `first` randomR (0, V.length xs - 1) g

startWords :: TextCorpus -> TextCorpus
startWords = V.filter (isUpper . T.head)

createStatistics :: TextCorpus -> TextCache
createStatistics = V.foldl' insertTriple M.empty . triplets
    where
      insertTriple acc t =
          case t of
            Just (w1, w2, w3) ->
                M.insertWith (M.unionWith (+)) (w1, w2) (M.singleton w3 1) acc
            Nothing -> acc
      triplets wrds =
          if size < 3
          then V.empty
          else fmap (sequenceTriple . mkTriplet) (V.enumFromN 0 (size - 2))
          where
            size = V.length wrds
            mkTriplet i = (wrds !? i, wrds !? (i+1), wrds !? (i+2))
            sequenceTriple t =
                case t of
                  (Just x, Just y, Just z) -> Just (x, y, z)
                  _ -> Nothing

generateMarkovText :: (RandomGen g) => TextCorpus -> g -> Int -> T.Text
generateMarkovText wrds gen size =
    T.dropWhileEnd (/= '.') $ T.unwords $ V.toList $ generate seedWord nextWord gen' size
    where
      (seedStartWord, gen') = choice gen $ startWords wrds
      seed = fromMaybe 0 $ V.elemIndex seedStartWord wrds
      seedWord = wrds ! seed
      nextWord = wrds ! (seed + 1)
      generate = go V.empty
          where
            go acc w1 w2 g n =
                case n of
                  0 -> acc `V.snoc` w2
                  _ ->
                      maybe (acc `V.snoc` w2)
                          (\(w', g') -> go (acc `V.snoc` w1) w2 w' g' (n - 1)) wg
                where
                  wg = weightedChoice g <$> M.lookup (w1, w2) cache
                  cache = createStatistics wrds

textGen len file =
    (generateMarkovText . V.fromList . T.words)
    <$> T.readFile file
    <*> newStdGen
    <*> pure len
