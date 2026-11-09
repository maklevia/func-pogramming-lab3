module Text.Normalize
  ( normalizeWhitespace
  , stripBom
  ) where

-- | Replace every run of whitespace with one ordinary space and trim the
-- result. Tabs, repeated spaces and line breaks are all covered.
normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

-- | Drop a UTF-8 byte-order mark if a reader has left it in the string.
stripBom :: String -> String
stripBom ('\65279' : rest) = rest
stripBom input = input
