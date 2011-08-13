module Data.SelfML (
  Tree(Node, Terminal),
  Forest,
  readSML,
  showSML,
  prettyPrintSML) where

import Control.Applicative
import Data.Char
import Data.List
import Text.Parsec hiding (many, (<|>), spaces)

-- | Like Data.Tree.Tree, but provides a singleton case. In essence, a conc list.
data Tree a = Node a (Forest a) | Terminal a
            deriving (Eq, Read, Show)

type Forest a = [Tree a]

-- | Reads SelfML out of a string.
readSML :: String -> Either ParseError (Forest String)
readSML = parse (smlForest <* eof) ""

-- | Serializes into a SelfML string.
showSML :: Forest String -> String
showSML [Node x []]  = "(" ++ showSMLString x ++ ")"
showSML [Node x ts]  = concat [ '(' : showSMLString x
                              , ' ' : showSML ts
                              , ")" ]
showSML [Terminal s] = showSMLString s
showSML xs           = intercalate " " $ map (showSML . singleton) xs

-- | Serializes into a SelfML string with indentation to make it more human-readable.
prettyPrintSML :: Forest String -> String
prettyPrintSML [Node x []]  = "(" ++ showSMLString x ++ ")"
prettyPrintSML [Node x ts]  = concat [ '(' : showSMLString x
                                     , concatMap (\(Terminal s) -> ' ' : showSMLString s) as
                                     , if null bs
                                          then ""
                                          else '\n' : (indent 4 $ intercalate "\n"
                                                                $ map (prettyPrintSML . singleton) bs)
                                     , ")" ]
  where (as,bs) = span isTerminal ts
prettyPrintSML [Terminal s] = showSMLString s
prettyPrintSML xs           = intercalate "\n\n" $ map (prettyPrintSML . singleton) xs

-- | Formats a string in SelfML syntax.
showSMLString :: String -> String
showSMLString s
  | null s = "[]"
  | any ((||) <$> (`elem` "[](){}#`") <*> isSpace) s =
      case parse (smlBracketed <* eof) "" ("[" ++ s ++ "]") of
        Left  _ -> "`" ++ concatMap (\c -> case c of { '`' -> "``"; _ -> [c] }) s ++ "`"
        Right _ -> "[" ++ s ++ "]"
  | otherwise = s

--- parser ---

smlForest :: Parsec [Char] () (Forest String)
smlTree   :: Parsec [Char] () (Tree   String)

smlForest       = spaces (smlTree `sepEndBy` whitespace)
smlTree         = Terminal <$> spaces smlString <|>
                  parens (Node <$> spaces smlString <*> smlForest)
smlString       = smlBacktick <|> smlBracketed <|> smlVerbatim <?> "a string"
smlVerbatim     = many1 $ satisfyNone [flip elem "[](){}#`", isSpace]
smlBracketed    = brackets $ concat <$> many (try smlBracketed <|> many1 (noneOf "[]"))
smlBacktick     = backticks $ many1 (try (string "``") *> pure '`' <|> noneOf "`")
smlLineComment  = char '#' *> many (noneOf "\n") *> (char '\n' *> pure () <|> eof)
smlComment      = braceHash $ many1 (notFollowedBy (string "#}") *>
                                     (try smlComment <|>
                                      anyChar *> pure ())) *> pure ()

parens    p = char   '('  *> p <* char   ')'
brackets  p = char   '['  *> p <* char   ']'
braceHash p = string "{#" *> p <* string "#}"
backticks p = char   '`'  *> p <* char   '`'

spaces   p = whitespace *> p <* whitespace
whitespace = many (smlLineComment <|> smlComment <|> satisfy isSpace *> pure ())

satisfyNone fs = satisfy $ \ c -> not $ any ($ c) fs

--- various utility functions for trees ---

instance Functor Tree where
  fmap f (Node x ts)  = Node (f x) (map (fmap f) ts)
  fmap f (Terminal x) = Terminal (f x)

-- helpers --

singleton x = [x]

indent n = (replicate n ' ' ++) . concatMap (\c -> case c of
                                                '\n' -> '\n' : replicate n ' '
                                                _    -> [c])

isTerminal (Terminal _) = True
isTerminal _            = False
isNode                  = not . isTerminal