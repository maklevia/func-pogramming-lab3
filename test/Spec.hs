module Main (main) where

import Data.Either (isLeft)
import Prelude hiding (Word)
import System.Exit (exitFailure)

import CLI
import Text.Analysis
import Text.Normalize
import Text.Parser
import Text.Render
import Text.Types

main :: IO ()
main = do
  check "normalization collapses tabs and repeated spaces" $
    normalizeWhitespace "  Привіт\t\tсвіт  \n\n  програмування  "
      == "Привіт світ програмування"

  check "normalization trims an empty text" $
    normalizeWhitespace " \t\r\n " == ""

  check "BOM is removed before parsing" $
    map wordText (documentWords (parseDocument "\65279Привіт."))
      == ["Привіт"]

  check "empty input produces an empty document" $
    parseDocument " \t\n" == Document []

  check "parser splits words and punctuation" $
    parseDocument "Привіт, світ!"
      == Document
        [ Sentence
            [ WordToken (word "Привіт")
            , PunctuationToken (mkPunctuation (Symbol ','))
            , WordToken (word "світ")
            , PunctuationToken (mkPunctuation (Symbol '!'))
            ]
            True
        ]

  check "parser keeps apostrophes inside words" $
    map wordText (documentWords (parseDocument "м'ята та foldl'"))
      == ["м'ята", "та", "foldl'"]

  check "parser keeps a hyphenated word together" $
    map wordText (documentWords (parseDocument "будь-який текст"))
      == ["будь-який", "текст"]

  check "parser recognises an ellipsis as one punctuation mark" $
    map punctuationKind (documentPunctuation (parseDocument "Чекай... далі."))
      == [EllipsisMark, Period]

  check "parser groups tokens into terminated sentences" $
    map sentenceTerminated (documentSentences (parseDocument "Один. Два! Три?"))
      == [True, True, True]

  check "parser keeps closing quotes in the sentence" $
    map (length . sentenceTokens) (documentSentences (parseDocument "«Так!» Далі."))
      == [4, 2]

  check "parser leaves the last sentence unterminated when needed" $
    map sentenceTerminated (documentSentences (parseDocument "Текст без крапки"))
      == [False]

  check "render normalizes text and punctuation spacing" $
    renderDocument (parseDocument "  Привіт,\t\tсвіт!\n\nHaskell   —   мова.  ")
      == "Привіт, світ! Haskell — мова."

  check "render keeps brackets tight" $
    renderDocument (parseDocument "Функція (f x) працює.")
      == "Функція (f x) працює."

  check "render keeps quotes tight" $
    renderDocument (parseDocument "«Слово» — це слово.")
      == "«Слово» — це слово."

  check "render keeps hyphenated words together" $
    renderDocument (parseDocument "будь-який") == "будь-який"

  check "renderWords joins words with single spaces" $
    renderWords [word "це", word "велика", word "мова"]
      == "це велика мова"

  check "countLetter ignores case by default" $
    countLetter False 'а' (word "Ада") == 2

  check "countLetter respects case when requested" $
    countLetter True 'а' (word "Ада") == 1
      && countLetter True 'А' (word "Ада") == 1

  check "sortWordsByLetter orders by count and then alphabetically" $
    map wordText (sortWordsByLetter False 'а' (map word ["ада", "мова", "це", "велика", "мова"]))
      == ["це", "велика", "мова", "мова", "ада"]

  check "sortWordsByLetter uses the Ukrainian alphabet" $
    map wordText
      ( sortWordsByLetter
          False
          'щ'
          (map word ["яблуко", "їжак", "ірис", "игра", "ґанок", "абзац"])
      )
      == ["абзац", "ґанок", "игра", "ірис", "їжак", "яблуко"]

  check "sortWordsByLetter puts Latin words after Cyrillic ones" $
    map wordText (sortWordsByLetter False 'щ' (map word ["zeta", "яблуко", "apple"]))
      == ["яблуко", "apple", "zeta"]

  check "uniqueWords ignores case and keeps the first form" $
    map wordText (uniqueWords (map word ["Ада", "ада", "мова"]))
      == ["Ада", "мова"]

  check "documentStats counts sentences, words and punctuation" $
    documentStats (parseDocument "Ада — мова. Ада!")
      == DocumentStats
        { statsSentences = 2
        , statsWords = 3
        , statsUniqueWords = 2
        , statsPunctuation = 3
        }

  check "documentStats handles an empty document" $
    documentStats (parseDocument "") == DocumentStats 0 0 0 0

  check "CLI defaults to letter а and standard output" $
    parseCommand [] == Right (Run defaultOptions)

  check "CLI parses all flags and a file" $
    parseCommand ["--unique", "--case-sensitive", "--normalized", "--text", "--stats", "--letter=ґ", "examples/textbook.txt"]
      == Right
        ( Run
            defaultOptions
              { optionLetter = 'ґ'
              , optionUnique = True
              , optionCaseSensitive = True
              , optionShowNormalized = True
              , optionTextOnly = True
              , optionShowStats = True
              , optionFile = Just "examples/textbook.txt"
              }
        )

  check "CLI supports the short letter option" $
    parseCommand ["-l", "о", "examples/simple.txt"]
      == Right
        ( Run
            defaultOptions
              { optionLetter = 'о'
              , optionFile = Just "examples/simple.txt"
              }
        )

  check "CLI treats a dash as stdin" $
    parseCommand ["-"] == Right (Run defaultOptions)

  check "CLI supports the option terminator" $
    parseCommand ["--", "examples/simple.txt"]
      == Right (Run defaultOptions {optionFile = Just "examples/simple.txt"})

  check "CLI rejects a missing letter value" $
    isLeft (parseCommand ["--letter"])

  check "CLI rejects a multi-character letter" $
    isLeft (parseCommand ["--letter=аб"])

  check "CLI rejects a non-letter value" $
    isLeft (parseCommand ["--letter=1"])

  check "CLI rejects an unknown long option" $
    isLeft (parseCommand ["--nope"])

  check "CLI rejects an unknown short option" $
    isLeft (parseCommand ["-x"])

  check "CLI rejects more than one file" $
    isLeft (parseCommand ["a.txt", "b.txt"])

  check "CLI shows help" $
    parseCommand ["--help"] == Right ShowHelp

  putStrLn "Усі тести пройшли."

word :: String -> Word
word = mkWord . map Symbol

check :: String -> Bool -> IO ()
check label ok =
  if ok
    then putStrLn ("PASS " ++ label)
    else do
      putStrLn ("FAIL " ++ label)
      exitFailure
