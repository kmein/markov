module Text.Random.Markov where

import Control.Arrow (first)

import Data.Char (isUpper)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M ((!), Map, empty, insertWith)
import qualified Data.Text as T (Text, head, unwords, words)
import qualified Data.Text.IO as T (readFile)

import System.Random (RandomGen, randomR, newStdGen)

type TextWord = T.Text

type TextCorpus = Vector TextWord

type TextCache = M.Map (TextWord, TextWord) TextCorpus

choice :: (RandomGen g) => g -> Vector a -> (a, g)
choice g xs = (xs !) `first` randomR (0, V.length xs - 1) g

startWords :: TextCorpus -> TextCorpus
startWords = V.filter (isUpper . T.head)

triplets :: TextCorpus -> Vector (TextWord, TextWord, TextWord)
triplets wrds =
    if size < 3
       then V.empty
       else flip fmap (V.fromList [0 .. (size - 3)]) $ \i ->
           (wrds ! i, wrds ! (i+1), wrds ! (i+2))
    where
      size = V.length wrds

createStatistics :: TextCorpus -> TextCache
createStatistics =
    V.foldl' (\acc (w1, w2, w3) -> M.insertWith (V.++) (w1, w2) (V.singleton w3) acc)
        M.empty . triplets

generateMarkovText :: (RandomGen g) => TextCorpus -> g -> Int -> T.Text
generateMarkovText wrds gen size =
    T.unwords $ V.toList $ generate seedWord nextWord gen' size
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
                  _ -> go (acc `V.snoc` w1) w2 w' g' (n - 1)
                where
                  (w', g') = choice g (cache M.! (w1, w2))
                  cache = createStatistics wrds

textGen :: Int -> FilePath -> IO T.Text
textGen len file =
    (generateMarkovText . V.fromList . T.words)
    <$> T.readFile file
    <*> newStdGen
    <*> pure len
