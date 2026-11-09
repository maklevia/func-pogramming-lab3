module Text.Analysis
  ( ukrainianAlphabet
  , countLetter
  , alphabeticalKey
  , sortWordsByLetter
  , uniqueWords
  , DocumentStats (..)
  , documentStats
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord, toLower)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import Prelude hiding (Word)

import Text.Types

-- | Ukrainian alphabet used for deterministic alphabetical ordering.
ukrainianAlphabet :: String
ukrainianAlphabet = "абвгґдеєжзиіїйклмнопрстуфхцчшщьюя"

ukrainianRanks :: [(Char, Int)]
ukrainianRanks = zip ukrainianAlphabet [0 ..]

-- | Count occurrences of a letter. With @caseSensitive = False@ both the
-- letter and the symbols are lowercased before comparison.
countLetter :: Bool -> Char -> Word -> Int
countLetter caseSensitive letter =
  length . filter matches . wordSymbols
  where
    target = normalize letter
    matches (Symbol character) = normalize character == target
    normalize = if caseSensitive then id else toLower

-- | Sort key that respects the Ukrainian alphabet instead of raw code points.
-- Latin letters go after Cyrillic ones, digits after letters.
alphabeticalKey :: Word -> [Int]
alphabeticalKey = map symbolRank . map toLower . wordText

symbolRank :: Char -> Int
symbolRank character =
  case lookup character ukrainianRanks of
    Just rank -> rank
    Nothing
      | isAsciiLower character -> 1000 + ord character
      | isAsciiUpper character -> 1000 + ord (toLower character)
      | isDigit character -> 2000 + ord character
      | otherwise -> 3000 + ord character

-- | Sort words by ascending number of the chosen letter. Words with equal
-- counts are ordered alphabetically. The final raw-text tie-breaker only makes
-- the order deterministic for words that differ solely in case.
sortWordsByLetter :: Bool -> Char -> [Word] -> [Word]
sortWordsByLetter caseSensitive letter =
  sortBy (comparing sortKey)
  where
    sortKey word =
      ( countLetter caseSensitive letter word
      , alphabeticalKey word
      , wordText word
      )

-- | Keep the first occurrence of every distinct word. Comparison ignores case,
-- so @Ада@ and @ада@ are treated as the same word.
uniqueWords :: [Word] -> [Word]
uniqueWords = go []
  where
    go _ [] = []
    go seen (word : rest)
      | key word `elem` seen = go seen rest
      | otherwise = word : go (key word : seen) rest

    key = map toLower . wordText

data DocumentStats = DocumentStats
  { statsSentences :: Int
  , statsWords :: Int
  , statsUniqueWords :: Int
  , statsPunctuation :: Int
  }
  deriving (Eq, Show)

documentStats :: Document -> DocumentStats
documentStats document =
  DocumentStats
    { statsSentences = length (documentSentences document)
    , statsWords = length wordsInDocument
    , statsUniqueWords = length (group (sort (map uniqueKey wordsInDocument)))
    , statsPunctuation = length (documentPunctuation document)
    }
  where
    wordsInDocument = documentWords document
    uniqueKey = map toLower . wordText
