module CLI
  ( Command (..)
  , Options (..)
  , defaultOptions
  , parseCommand
  , usageText
  ) where

import Data.Char (isLetter)
import Data.List (stripPrefix)

data Options = Options
  { optionLetter :: Char
  , optionUnique :: Bool
  , optionCaseSensitive :: Bool
  , optionShowNormalized :: Bool
  , optionTextOnly :: Bool
  , optionShowStats :: Bool
  , optionFile :: Maybe FilePath
  }
  deriving (Eq, Show)

data Command
  = ShowHelp
  | Run Options
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optionLetter = 'а'
    , optionUnique = False
    , optionCaseSensitive = False
    , optionShowNormalized = False
    , optionTextOnly = False
    , optionShowStats = False
    , optionFile = Nothing
    }

parseCommand :: [String] -> Either String Command
parseCommand args
  | any isHelp args = Right ShowHelp
  | otherwise = Run <$> go defaultOptions args
  where
    isHelp arg = arg == "--help" || arg == "-h"

go :: Options -> [String] -> Either String Options
go options [] = Right options
go options (arg : rest)
  | arg == "--unique" = go options {optionUnique = True} rest
  | arg == "--case-sensitive" = go options {optionCaseSensitive = True} rest
  | arg == "--normalized" = go options {optionShowNormalized = True} rest
  | arg == "--text" = go options {optionTextOnly = True} rest
  | arg == "--stats" = go options {optionShowStats = True} rest
  | arg == "--letter" || arg == "-l" = do
      (value, remaining) <- needValue arg rest
      letter <- parseLetter value
      go options {optionLetter = letter} remaining
  | Just value <- stripPrefix "--letter=" arg = do
      letter <- parseLetter value
      go options {optionLetter = letter} rest
  | arg == "--" = addFiles options rest
  | arg == "-" = go options rest
  | take 1 arg == "-" = Left ("невідома опція «" ++ arg ++ "»")
  | otherwise = do
      updated <- addFile options arg
      go updated rest

addFiles :: Options -> [String] -> Either String Options
addFiles options [] = Right options
addFiles options (path : rest) = do
  updated <- addFile options path
  addFiles updated rest

addFile :: Options -> FilePath -> Either String Options
addFile options path =
  case optionFile options of
    Just _ -> Left "можна вказати лише один вхідний файл"
    Nothing -> Right options {optionFile = Just path}

needValue :: String -> [String] -> Either String (String, [String])
needValue name [] = Left ("опція " ++ name ++ " потребує значення")
needValue _ (value : rest) = Right (value, rest)

parseLetter :: String -> Either String Char
parseLetter token =
  case token of
    [letter] | isLetter letter -> Right letter
    _ -> Left ("--letter має бути однією літерою, отримано «" ++ token ++ "»")

usageText :: String
usageText =
  unlines
    [ "Використання: textbook-word-sorter [ОПЦІЇ] [ФАЙЛ]"
    , ""
    , "Читає текст підручника з програмування, нормалізує пробіли та сортує"
    , "слова за зростанням кількості заданої літери. Слова з однаковою"
    , "кількістю впорядковуються за українською абеткою."
    , ""
    , "Опції:"
    , "  -l, --letter ЛІТЕРА   літера для підрахунку, типово «а»"
    , "      --unique          показати кожне різне слово лише раз"
    , "      --case-sensitive  розрізняти великі й малі літери"
    , "      --normalized      показати текст після нормалізації пробілів"
    , "      --text            вивести лише відсортований текст"
    , "      --stats           показати статистику тексту"
    , "  -h, --help            показати цю довідку"
    , ""
    , "Якщо файл не вказано, текст читається зі stdin. У режимі за"
    , "замовчуванням виводиться таблиця слів із кількістю заданої літери;"
    , "для машинної обробки використовуйте --text."
    , ""
    , "Приклади:"
    , "  textbook-word-sorter examples/textbook.txt"
    , "  textbook-word-sorter --letter о --unique --text examples/textbook.txt"
    , "  cat examples/simple.txt | textbook-word-sorter -l а --text"
    ]
