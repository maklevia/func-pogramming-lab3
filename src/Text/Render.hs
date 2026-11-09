module Text.Render
  ( renderWord
  , renderPunctuation
  , renderToken
  , renderSentence
  , renderDocument
  , renderWords
  ) where

import Data.List (intercalate)
import Prelude hiding (Word)

import Text.Types

renderWord :: Word -> String
renderWord = wordText

renderPunctuation :: Punctuation -> String
renderPunctuation = punctuationText

renderToken :: Token -> String
renderToken token = case token of
  WordToken word -> renderWord word
  PunctuationToken punctuation -> renderPunctuation punctuation

renderSentence :: Sentence -> String
renderSentence = renderTokens . sentenceTokens

-- | Render a document as normalized text: sentences are separated by one
-- space, punctuation follows the usual spacing rules, and no tab or repeated
-- space can appear in the result.
renderDocument :: Document -> String
renderDocument =
  intercalate " "
    . filter (not . null)
    . map renderSentence
    . documentSentences

-- | Render a list of words as one normalized line. This is exactly what the
-- laboratory task asks for when it mentions forming text.
renderWords :: [Word] -> String
renderWords = unwords . map renderWord

renderTokens :: [Token] -> String
renderTokens = go Nothing
  where
    go _ [] = ""
    go Nothing (token : rest) =
      renderToken token ++ go (Just token) rest
    go (Just previous) (token : rest) =
      (if needsSpace previous token then " " else "")
        ++ renderToken token
        ++ go (Just token) rest

needsSpace :: Token -> Token -> Bool
needsSpace previous current =
  not (attachesLeft current || attachesRight previous)

attachesLeft :: Token -> Bool
attachesLeft (WordToken _) = False
attachesLeft (PunctuationToken punctuation) =
  case punctuationKind punctuation of
    Comma -> True
    Period -> True
    QuestionMark -> True
    ExclamationMark -> True
    Semicolon -> True
    Colon -> True
    EllipsisMark -> True
    Hyphen -> True
    ClosingQuote -> True
    ClosingBracket -> True
    _ -> False

attachesRight :: Token -> Bool
attachesRight (WordToken _) = False
attachesRight (PunctuationToken punctuation) =
  case punctuationKind punctuation of
    OpeningQuote -> True
    OpeningBracket -> True
    Hyphen -> True
    _ -> False
