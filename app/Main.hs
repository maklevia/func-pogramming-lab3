module Main (main) where

import Control.Exception (IOException, SomeException, catch, evaluate, try)
import Control.Monad (forM_, when)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO
  ( hGetContents
  , hPutStrLn
  , hSetEncoding
  , stderr
  , stdin
  , stdout
  , utf8
  , withFile
  , IOMode (ReadMode)
  )
import Text.Printf (printf)
import Prelude hiding (Word)

import CLI
import Text.Analysis
import Text.Parser (parseDocument)
import Text.Render (renderDocument, renderWords)
import Text.Types

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  exitCode <- case parseCommand args of
    Left message -> do
      hPutStrLn stderr ("Помилка: " ++ message)
      hPutStrLn stderr ""
      hPutStrLn stderr usageText
      pure (ExitFailure 1)
    Right ShowHelp -> do
      putStrLn usageText
      pure ExitSuccess
    Right (Run options) -> run options `catch` handleUnexpected
  exitWith exitCode
  where
    handleUnexpected :: SomeException -> IO ExitCode
    handleUnexpected exception = do
      hPutStrLn stderr ("Непередбачена помилка: " ++ show exception)
      pure (ExitFailure 1)

run :: Options -> IO ExitCode
run options = do
  inputResult <- readInput (optionFile options)
  case inputResult of
    Left message -> do
      hPutStrLn stderr ("Помилка: " ++ message)
      pure (ExitFailure 1)
    Right rawText ->
      let document = parseDocument rawText
          allWords = documentWords document
      in if null allWords
           then do
             hPutStrLn stderr "Помилка: у тексті не знайдено жодного слова."
             pure (ExitFailure 1)
           else do
             let outputWords =
                   if optionUnique options
                     then uniqueWords allWords
                     else allWords
                 sortedWords =
                   sortWordsByLetter
                     (optionCaseSensitive options)
                     (optionLetter options)
                     outputWords

             printHeader options document allWords outputWords

             when (optionShowNormalized options) $ do
               putStrLn ""
               putStrLn "Нормалізований текст:"
               putStrLn (renderDocument document)

             when (optionShowStats options) $ do
               putStrLn ""
               printStats document

             putStrLn ""
             if optionTextOnly options
               then do
                 putStrLn "Відсортований текст:"
                 putStrLn (renderWords sortedWords)
               else printTable options sortedWords
             pure ExitSuccess

readInput :: Maybe FilePath -> IO (Either String String)
readInput Nothing = Right <$> getContents
readInput (Just path) = do
  result <- try (readFileUtf8 path) :: IO (Either IOException String)
  pure $ case result of
    Left exception ->
      Left ("не вдалося прочитати файл «" ++ path ++ "»: " ++ show exception)
    Right content -> Right content

readFileUtf8 :: FilePath -> IO String
readFileUtf8 path =
  withFile path ReadMode $ \handle -> do
    hSetEncoding handle utf8
    content <- hGetContents handle
    _ <- evaluate (length content)
    pure content

printHeader :: Options -> Document -> [Word] -> [Word] -> IO ()
printHeader options document allWords outputWords = do
  putStrLn ("Джерело: " ++ sourceDescription)
  putStrLn
    ( "Літера: «"
        ++ [optionLetter options]
        ++ "» ("
        ++ comparisonDescription
        ++ "); входжень у тексті: "
        ++ show totalOccurrences
    )
  putStrLn
    ( "Слів: "
        ++ show (length allWords)
        ++ " (різних: "
        ++ show uniqueCount
        ++ "); у виводі: "
        ++ show (length outputWords)
        ++ " ("
        ++ outputDescription
        ++ ")"
    )
  where
    sourceDescription = case optionFile options of
      Just path -> path
      Nothing -> "stdin"
    comparisonDescription =
      if optionCaseSensitive options
        then "з урахуванням регістру"
        else "без урахування регістру"
    totalOccurrences =
      sum
        [ countLetter
            (optionCaseSensitive options)
            (optionLetter options)
            word
        | word <- allWords
        ]
    uniqueCount = statsUniqueWords (documentStats document)
    outputDescription =
      if optionUnique options then "лише різні слова" else "усі входження"

printStats :: Document -> IO ()
printStats document = do
  let stats = documentStats document
  putStrLn "Статистика:"
  putStrLn ("  речень: " ++ show (statsSentences stats))
  putStrLn
    ( "  слів: "
        ++ show (statsWords stats)
        ++ " (різних: "
        ++ show (statsUniqueWords stats)
        ++ ")"
    )
  putStrLn ("  розділових знаків: " ++ show (statsPunctuation stats))
  putStrLn
    ( "  символів у нормалізованому тексті: "
        ++ show (length (renderDocument document))
    )

printTable :: Options -> [Word] -> IO ()
printTable options sortedWords = do
  putStrLn
    ( "Відсортовані слова (за кількістю «"
        ++ [optionLetter options]
        ++ "» за зростанням, потім за абеткою):"
    )
  putStrLn ""
  let width = max 5 (min 36 (maximum (0 : map (length . wordText) sortedWords)))
  putStrLn ("   №  " ++ padRight width "Слово" ++ "  К-ть")
  putStrLn ("----  " ++ replicate width '-' ++ "  ----")
  forM_ (zip [1 :: Int ..] sortedWords) $ \(index, word) ->
    printf
      "%4d  %s  %4d\n"
      index
      (padRight width (wordText word))
      ( countLetter
          (optionCaseSensitive options)
          (optionLetter options)
          word
      )

padRight :: Int -> String -> String
padRight width text = text ++ replicate (max 0 (width - length text)) ' '
