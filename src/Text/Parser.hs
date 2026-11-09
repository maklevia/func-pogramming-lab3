module Text.Parser
  ( parseDocument
  , tokenize
  , splitSentences
  ) where

import Data.Char (isSpace)

import Text.Normalize (normalizeWhitespace, stripBom)
import Text.Types

-- | Normalize whitespace, split the text into tokens and group tokens into
-- sentences. Parsing is total: any input produces a (possibly empty) document.
parseDocument :: String -> Document
parseDocument =
  Document
    . splitSentences
    . tokenize
    . normalizeWhitespace
    . stripBom

-- | Split normalized text into words and single punctuation marks.
--
-- A word starts with a letter, a digit or an underscore and may continue with
-- letters, digits, underscores and apostrophes. This keeps Ukrainian forms
-- such as @м'ята@ and Haskell identifiers such as @foldl'@ in one piece.
tokenize :: String -> [Token]
tokenize [] = []
tokenize input@(character : rest)
  | isSpace character = tokenize rest
  | isWordStart (Symbol character) =
      let (wordCharacters, remaining) = takeWordCharacters input
      in WordToken (mkWord (map Symbol wordCharacters)) : tokenize remaining
  | take 3 input == "..." =
      PunctuationToken (mkPunctuation (Symbol '\8230')) : tokenize (drop 3 input)
  | otherwise =
      PunctuationToken (mkPunctuation (Symbol character)) : tokenize rest

-- | Group tokens into sentences. A terminal punctuation mark closes the
-- current sentence; closing quotes and brackets that follow it stay inside the
-- same sentence.
splitSentences :: [Token] -> [Sentence]
splitSentences = go []
  where
    go [] [] = []
    go accumulated [] = [Sentence (reverse accumulated) False]
    go accumulated (token : rest) =
      case token of
        PunctuationToken punctuation
          | isTerminalPunctuation punctuation ->
              let (trailing, remaining) = span isSentenceTrailing rest
                  sentenceTokens' = reverse accumulated ++ (token : trailing)
              in Sentence sentenceTokens' True : go [] remaining
        _ -> go (token : accumulated) rest

    isSentenceTrailing (PunctuationToken punctuation) =
      isClosingPunctuation punctuation || isTerminalPunctuation punctuation
    isSentenceTrailing (WordToken _) = False

takeWordCharacters :: String -> (String, String)
takeWordCharacters input@(character : _)
  | not (isWordStart (Symbol character)) = ([], input)
  | otherwise = go input
  where
    go [] = ([], [])
    go (current : rest)
      | isWordStart (Symbol current) =
          let (wordRest, remaining) = go rest
          in (current : wordRest, remaining)
      | isApostrophe current =
          let (wordRest, remaining) = go rest
          in (current : wordRest, remaining)
      | otherwise = ([], current : rest)

    isApostrophe symbol = symbol == '\'' || symbol == '\8217'
takeWordCharacters [] = ([], [])
