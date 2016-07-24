module Main where

import Data.Maybe (fromMaybe)
import Options.Applicative
import Text.Random.Markov (textGen)
import qualified Data.Text.IO as T (putStrLn)

main :: IO ()
main =
    do (file, len) <- execParser $
           info (helper <*> markovOptions)
               (fullDesc <> progDesc "Generate a random text based on a corpus.")
       T.putStrLn =<< textGen (fromMaybe 1000 len) file
    where
      markovOptions =
          (,)
          <$> strArgument (metavar "PATH" <> help "File containing the corpus")
          <*> optional (option auto (short 'l' <> long "length" <> metavar "LENGTH" <> help "Length of the text (default 1000)"))
