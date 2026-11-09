module Text.Types
  ( Symbol (..)
  , SymbolKind (..)
  , classifySymbol
  , isWordStart
  , Word (..)
  , mkWord
  , wordText
  , wordLength
  , countSymbol
  , PunctuationKind (..)
  , Punctuation (..)
  , mkPunctuation
  , punctuationText
  , isTerminalPunctuation
  , isClosingPunctuation
  , isOpeningPunctuation
  , Token (..)
  , Sentence (..)
  , Document (..)
  , sentenceWords
  , sentencePunctuation
  , documentWords
  , documentTokens
  , documentPunctuation
  ) where

import Data.Char (isDigit, isLetter, isPunctuation, isSpace)
import Prelude hiding (Word)

-- | One character of the source text.
--
-- Keeping a dedicated type makes the distinction between symbols that can
-- belong to a word and symbols that form punctuation explicit.
newtype Symbol = Symbol
  { symbolChar :: Char
  }
  deriving (Eq, Ord, Show)

-- | Classification used by the tokenizer.
data SymbolKind
  = LetterSymbol
  | DigitSymbol
  | ApostropheSymbol
  | UnderscoreSymbol
  | SeparatorSymbol
  | PunctuationSymbol
  | OtherSymbol
  deriving (Eq, Ord, Show)

classifySymbol :: Symbol -> SymbolKind
classifySymbol (Symbol character)
  | isLetter character = LetterSymbol
  | isDigit character = DigitSymbol
  | isApostrophe character = ApostropheSymbol
  | character == '_' = UnderscoreSymbol
  | isSpace character = SeparatorSymbol
  | isPunctuation character = PunctuationSymbol
  | otherwise = OtherSymbol

-- | First symbol of a word or identifier.
isWordStart :: Symbol -> Bool
isWordStart symbol = case classifySymbol symbol of
  LetterSymbol -> True
  DigitSymbol -> True
  UnderscoreSymbol -> True
  _ -> False

-- | A word is a non-empty sequence of symbols. Whitespace never belongs to it.
newtype Word = Word
  { wordSymbols :: [Symbol]
  }
  deriving (Eq, Ord, Show)

mkWord :: [Symbol] -> Word
mkWord = Word

wordText :: Word -> String
wordText (Word symbols) = map symbolChar symbols

wordLength :: Word -> Int
wordLength (Word symbols) = length symbols

-- | Count exact symbol occurrences. Case folding is a separate policy and is
-- implemented in 'Text.Analysis'.
countSymbol :: Symbol -> Word -> Int
countSymbol target (Word symbols) = length (filter (== target) symbols)

-- | A punctuation mark together with its grammatical role.
data PunctuationKind
  = Comma
  | Period
  | QuestionMark
  | ExclamationMark
  | Semicolon
  | Colon
  | Hyphen
  | Dash
  | EllipsisMark
  | OpeningQuote
  | ClosingQuote
  | OpeningBracket
  | ClosingBracket
  | OtherPunctuation
  deriving (Eq, Ord, Show)

data Punctuation = Punctuation
  { punctuationSymbol :: Symbol
  , punctuationKind :: PunctuationKind
  }
  deriving (Eq, Ord, Show)

mkPunctuation :: Symbol -> Punctuation
mkPunctuation symbol = Punctuation symbol (classifyPunctuation (symbolChar symbol))

punctuationText :: Punctuation -> String
punctuationText = pure . symbolChar . punctuationSymbol

-- | Period, question mark, exclamation mark and ellipsis end a sentence.
isTerminalPunctuation :: Punctuation -> Bool
isTerminalPunctuation punctuation = case punctuationKind punctuation of
  Period -> True
  QuestionMark -> True
  ExclamationMark -> True
  EllipsisMark -> True
  _ -> False

isClosingPunctuation :: Punctuation -> Bool
isClosingPunctuation punctuation = case punctuationKind punctuation of
  ClosingQuote -> True
  ClosingBracket -> True
  _ -> False

isOpeningPunctuation :: Punctuation -> Bool
isOpeningPunctuation punctuation = case punctuationKind punctuation of
  OpeningQuote -> True
  OpeningBracket -> True
  _ -> False

-- | A word or a punctuation mark in the original order.
data Token
  = WordToken Word
  | PunctuationToken Punctuation
  deriving (Eq, Show)

-- | A sentence keeps its tokens intact. The flag records whether the sentence
-- ended with a terminal punctuation mark, which is useful for statistics.
data Sentence = Sentence
  { sentenceTokens :: [Token]
  , sentenceTerminated :: Bool
  }
  deriving (Eq, Show)

newtype Document = Document
  { documentSentences :: [Sentence]
  }
  deriving (Eq, Show)

sentenceWords :: Sentence -> [Word]
sentenceWords = foldr collect [] . sentenceTokens
  where
    collect (WordToken word) result = word : result
    collect (PunctuationToken _) result = result

sentencePunctuation :: Sentence -> [Punctuation]
sentencePunctuation = foldr collect [] . sentenceTokens
  where
    collect (PunctuationToken punctuation) result = punctuation : result
    collect (WordToken _) result = result

documentWords :: Document -> [Word]
documentWords = concatMap sentenceWords . documentSentences

documentTokens :: Document -> [Token]
documentTokens = concatMap sentenceTokens . documentSentences

documentPunctuation :: Document -> [Punctuation]
documentPunctuation = concatMap sentencePunctuation . documentSentences

isApostrophe :: Char -> Bool
isApostrophe character = character == '\'' || character == '\8217'

classifyPunctuation :: Char -> PunctuationKind
classifyPunctuation character = case character of
  ',' -> Comma
  '.' -> Period
  '?' -> QuestionMark
  '!' -> ExclamationMark
  ';' -> Semicolon
  ':' -> Colon
  '-' -> Hyphen
  '\8211' -> Dash
  '\8212' -> Dash
  '\8230' -> EllipsisMark
  '«' -> OpeningQuote
  '„' -> OpeningQuote
  '“' -> OpeningQuote
  '‘' -> OpeningQuote
  '»' -> ClosingQuote
  '”' -> ClosingQuote
  '’' -> ClosingQuote
  '(' -> OpeningBracket
  '[' -> OpeningBracket
  '{' -> OpeningBracket
  ')' -> ClosingBracket
  ']' -> ClosingBracket
  '}' -> ClosingBracket
  _ -> OtherPunctuation
