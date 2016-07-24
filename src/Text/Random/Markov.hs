module Text.Random.Markov where

import Control.Arrow (first)
import Data.Char (isUpper)
import Data.Vector
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M ((!), Map, empty, insertWith)
import qualified Data.Text as T (Text, head, unwords, words)
import qualified Data.Text.IO as T (readFile)
import Prelude hiding ((++), filter, length, map)
import System.Random (RandomGen, randomR, newStdGen)

type TextWord = T.Text
type TextText = Vector TextWord
type TextCache = M.Map (TextWord, TextWord) TextText

startWords :: TextText -> TextText
startWords = filter (isUpper . T.head)

triplets :: TextText -> Vector (TextWord, TextWord, TextWord)
triplets wrds =
    if size < 3
       then empty
       else flip map (fromList [0 .. (size - 3)]) $ \i ->
           (wrds ! i, wrds ! (i+1), wrds ! (i+2))
    where
      size = length wrds

createStatistics :: TextText -> TextCache
createStatistics =
    foldl' (\acc (w1, w2, w3) -> M.insertWith (++) (w1, w2) (singleton w3) acc)
        M.empty . triplets

generateMarkovText :: (RandomGen g) => TextText -> g -> Int -> T.Text
generateMarkovText wrds gen size =
    T.unwords $ toList $ generate seedWord nextWord gen' size
    where
      (seedStartWord, gen') = choice gen $ startWords wrds
      seed = fromMaybe 0 $ elemIndex seedStartWord wrds

      seedWord = wrds ! seed
      nextWord = wrds ! (seed + 1)

      choice g xs = (xs !) `first` randomR (0, length xs - 1) g

      generate = go empty
          where
            go acc w1 w2 g n =
                case n of
                  0 -> acc `snoc` w2
                  _ -> go (acc `snoc` w1) w2 w' g' (n - 1)
                where
                  (w', g') = choice g (cache M.! (w1, w2))
                  cache = createStatistics wrds

textGen :: Int -> FilePath -> IO T.Text
textGen len file =
    generateMarkovText
    <$> (fromList <$> T.words <$> T.readFile file)
    <*> newStdGen
    <*> pure len
