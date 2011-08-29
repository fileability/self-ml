module Data.SelfML (
  Tree(Node, Terminal),
  Forest,
  children,
  withChildren,
  value,
  withValue,
  isTerminal,
  isNode,
  readSML,
  showSML,
  showSMLString,
  prettyPrintSML,
  SelfPath(..),
  Query(..),
  filterSelfPath,
  mapSelfPath,
  parseSelfPath,
  (<//>),
  (<$/>),
  transformSML) where

import Control.Applicative
import Data.Char
import Data.List
import Data.Monoid
import Text.Parsec hiding (many, (<|>), spaces, optional)

-- | Like Data.Tree.Tree, but provides a singleton case. In essence, a conc list.
data Tree a = Node a (Forest a) | Terminal a
            deriving (Eq, Read, Show)

type Forest a = [Tree a]

-- | Reads SelfML out of a string.
readSML :: String -> Either ParseError (Forest String)
readSML = parse (smlForest smlString <* eof) ""

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

--- SelfPath-related ---

data Query = Named String
           | Equal (Tree String)
           | Child
           | At    Integer
           | AtEq  Integer (Tree String)
           | Range Integer Integer
           | Not   Query
           deriving (Eq, Read, Show)

data SelfPath = Recursive  Query    SelfPath
              | Direct     Query    SelfPath
              | Lookahead  SelfPath SelfPath
              | NLookahead SelfPath SelfPath
              | MEither    SelfPath SelfPath
              | Anything
              deriving (Eq, Read, Show)

instance Monoid SelfPath where
  mempty                      = Anything
  Recursive  q  sp `mappend` b = Recursive   q               (sp `mappend` b)
  Direct     q  sp `mappend` b = Direct      q               (sp `mappend` b)
  Lookahead  sa sb `mappend` b = Lookahead   sa              (sb `mappend` b)
  NLookahead sa sb `mappend` b = NLookahead  sa              (sb `mappend` b)
  MEither    sa sb `mappend` b = MEither    (sa `mappend` b) (sb `mappend` b)
  Anything         `mappend` b = b

(<//>) :: Forest String                     -- ^ Forest to match against
       -> String                            -- ^ SelfPath to compile
       -> Either ParseError (Forest String) -- ^ Result of the filter operation
f <//> sp = case parseSelfPath sp of
              Left err       -> Left err
              Right Anything -> Right f
              Right sp'      -> Right (filterSelfPath sp' (Node "" f))

(<$/>) :: (Tree String -> Tree String)      -- ^ Map function
       -> String                            -- ^ SelfPath to compile
       -> Forest String                     -- ^ Forest to match & map over
       -> Either ParseError (Forest String) -- ^ Result of the map operation
f <$/> sp = \ fo -> cs . ($ Node "" fo) . ($ f) . mapSelfPath <$> parseSelfPath sp
  where cs (Node "" ts) = ts
        cs n@_          = [n]

transformSML :: (Tree String -> Tree String) -- ^ Map function
             -> String                       -- ^ SelfPath to compile
             -> String                       -- ^ Serialized SelfML text to transform
             -> Either ParseError String     -- ^ Pretty-printed transformation if successful
transformSML f sp i = prettyPrintSML <$> (readSML i >>= f <$/> sp)

filterSelfPath :: SelfPath -> Tree String -> Forest String
filterSelfPath (Recursive  q  sp) t = deepQuery    q t >>= filterSelfPath sp
filterSelfPath (Direct     q  sp) t = shallowQuery q t >>= filterSelfPath sp
filterSelfPath (Lookahead  ie sp) t = if null (filterSelfPath ie t)
                                         then []
                                         else filterSelfPath sp t
filterSelfPath (NLookahead ie sp) t = if null (filterSelfPath ie t)
                                         then filterSelfPath sp t
                                         else []
filterSelfPath (MEither    sa sb) t = filterSelfPath sa t ++ filterSelfPath sb t
filterSelfPath Anything           t = [t]

mapSelfPath :: SelfPath -> (Tree String -> Tree String) -> Tree String -> Tree String
mapSelfPath (Recursive  q  sp) f t = deepMapQuery    q (mapSelfPath sp f) t
mapSelfPath (Direct     q  sp) f t = shallowMapQuery q (mapSelfPath sp f) t
mapSelfPath (Lookahead  ie sp) f t = if null (filterSelfPath ie t)
                                        then t
                                        else mapSelfPath sp f t
mapSelfPath (NLookahead ie sp) f t = if null (filterSelfPath ie t)
                                        then mapSelfPath sp f t
                                        else t
mapSelfPath (MEither    sa sb) f t = mapSelfPath sb f (mapSelfPath sa f t)
mapSelfPath Anything           f t = f t

parseSelfPath :: String -> Either ParseError SelfPath
parseSelfPath = parse (spSpaces spNode <* eof) ""

--- parser ---

smlForest :: Parsec [Char] () a -> Parsec [Char] () (Forest a)
smlTree   :: Parsec [Char] () a -> Parsec [Char] () (Tree   a)

smlForest     v = spaces (smlTree v `sepEndBy` whitespace)
smlTree       v = Terminal <$> v <|>
                  parens (Node <$> v <*> smlForest v)
smlString       = smlBacktick <|> smlBracketed <|> smlVerbatim <?> "a string"
smlVerbatim     = many1 $ satisfyNone [flip elem "[](){}#`", isSpace]
smlBracketed    = brackets $ concat <$> many (("["++) . (++"]") <$> try smlBracketed <|> many1 (noneOf "[]"))
smlBacktick     = backticks $ many (try (string "``" *> pure '`') <|> noneOf "`")
smlLineComment  = char '#' *> many (noneOf "\n") *> (char '\n' *> pure () <|> eof)
smlComment      = braceHash $ many (notFollowedBy (string "#}") *>
                                    (try smlComment <|>
                                     anyChar *> pure ())) *> pure ()

parens    p = char   '('  *> p <* char   ')'
brackets  p = char   '['  *> p <* char   ']'
braceHash p = string "{#" *> p <* string "#}"
backticks p = char   '`'  *> p <* char   '`'

spaces   p = whitespace *> p <* whitespace
whitespace = many (smlLineComment <|> smlComment <|> satisfy isSpace *> pure ()) *> pure ()

satisfyNone fs = satisfy $ \ c -> not $ any ($ c) fs

--- SelfPath parser ---

spNode :: Parsec [Char] () SelfPath

spNode = try (MEither <$> spNodeI <*> (spSpaces (char ',') *> spNode)) <|> spNodeI

spNodeI = try (lah       <$> many (char '!') <*> parens (spNode)   <*> sn)
      <|>      Direct    <$> (char '>' *> spWhitespace *> spQuery) <*> sn
      <|>                     char '$' *> spWhitespace *> spNode
      <|>      Recursive <$> spQuery                               <*> sn
  where sn = try (spWhitespace *> spNodeI) <|> pure Anything
        lah excl ie sp =
          (if even (length excl) then Lookahead else NLookahead) ie sp

spQuery = spNot
      <|> spEqual
      <|> try spAtEq
      <|> spAtRange
      <|> spChild
      <|> spNamed

spNot        = Not   <$> (char '!' *> spQuery)
spEqual      = Equal <$> (char '=' *> smlTree spString)
spAtEq       = AtEq  <$> (char '#' *> spNum) <*> (char '=' *> smlTree spString)
spAtRange    = fd    <$> (char '#' *> spNum) <*> optional (char '-' *> spNum)
  where fd n = maybe (At n) (Range n)
spChild      = char '*' *> pure Child
spNamed      = Named <$> spString

spWhitespace = many (smlComment <|> satisfy isSpace *> pure ())
spSpaces p   = spWhitespace *> p <* spWhitespace

spNum      = read <$> many1 (oneOf ['0'..'9'])
spString   = smlBacktick <|> smlBracketed <|> spVerbatim <?> "a SelfML string"
spVerbatim = many1 $ satisfyNone [flip elem "[](){}#`,=!*<>$", isSpace]

--- various utility functions for trees ---

instance Functor Tree where
  fmap f (Node x ts)  = Node (f x) (map (fmap f) ts)
  fmap f (Terminal x) = Terminal (f x)

-- | Returns the value (left side) of a node.
value :: Tree a -> a
value (Node x _)   = x
value (Terminal x) = x

-- | Returns the children of a node. If the node is 'Terminal', then it returns '[]'.
children :: Tree a -> Forest a
children (Node _ ts) = ts
children _           = []

-- | Modifies the children of a node. If the node is 'Terminal', nothing is changed.
withChildren :: (Forest a -> Forest a) -> Tree a -> Tree a
withChildren f (Node x ts)  = Node x (f ts)
withChildren f (Terminal x) = Terminal x

-- | Modifies the value (left side) of a node.
withValue :: (a -> a) -> Tree a -> Tree a
withValue f (Node x ts)  = Node (f x) ts
withValue f (Terminal x) = Terminal (f x)

--- SelfPath Query Magic ---

-- | Recursively matches a query through a tree.
deepQuery :: Query -> Tree String -> Forest String
deepQuery q n = shallowQuery q n ++ (children n >>= deepQuery q)

-- | Matches a query on the top level of a tree.
shallowQuery :: Query -> Tree String -> Forest String
shallowQuery      (Named s)    (Node _ ts)  = filter f ts
  where f (Node x _) = x == s
        f _          = False
shallowQuery (Not (Named s))   (Node _ ts)  = filter f ts
  where f (Node x _) = x /= s
        f _          = False
shallowQuery      (Equal t)    (Node _ ts)  = filter (== t)    ts
shallowQuery (Not (Equal t))   (Node _ ts)  = filter (/= t)    ts
shallowQuery      (At    i)    n            = filterWithIndex (const . (== i))                (nodeToList n)
shallowQuery (Not (At    i))   n            = filterWithIndex (const . (/= i))                (nodeToList n)
shallowQuery      (AtEq  i e)  n            = filterWithIndex (\ i' e' -> i' == i && e' == e) (nodeToList n)
shallowQuery (Not (AtEq  i e)) n            = filterWithIndex (\ i' e' -> i' == i && e' /= e) (nodeToList n)
shallowQuery      (Range a b)  n            = filterWithIndex (const . andF [(>= a),(<= b)])  (nodeToList n)
shallowQuery (Not (Range a b)) n            = filterWithIndex (const . andF [(<  a),(>  b)])  (nodeToList n)
shallowQuery      Child        (Node _ ts)  = ts
shallowQuery (Not Child)       _            = []
shallowQuery (Not (Not q))     t            = shallowQuery q t
shallowQuery _         _                    = []

-- | Recursively maps a query through a tree.
deepMapQuery :: Query -> (Tree String -> Tree String) -> Tree String -> Tree String
deepMapQuery q f = withChildren (map $ deepMapQuery q f) . shallowMapQuery q f

-- | Maps a query through the top level of a tree.
shallowMapQuery :: Query -> (Tree String -> Tree String) -> Tree String -> Tree String
shallowMapQuery      (Named s)    f t = withChildren (mapIf (andF [isNode, (== s).value]) f) t
shallowMapQuery (Not (Named s))   f t = withChildren (mapIf (andF [isNode, (/= s).value]) f) t
shallowMapQuery      (Equal e)    f t = withChildren (mapIf (== e) f) t
shallowMapQuery (Not (Equal e))   f t = withChildren (mapIf (/= e) f) t
shallowMapQuery      (At    0)    f t = withValue    (value.f.Terminal) t
shallowMapQuery (Not (At    0))   f t = withChildren (map f) t
shallowMapQuery      (At    i)    f t = withChildren (mapIf' (== i-1) f) t
shallowMapQuery (Not (At    i))   f t = withChildren (mapIf' (/= i-1) f) t
shallowMapQuery      (AtEq  0 e)  f t = withValue    (value . applyIf (== e) f . Terminal) t
shallowMapQuery (Not (AtEq  0 e)) f t = withValue    (value . applyIf (/= e) f . Terminal) t
shallowMapQuery      (AtEq  i e)  f t = withChildren (mapIf' (== i-1) (applyIf (== e) f)) t
shallowMapQuery (Not (AtEq  i e)) f t = withChildren (mapIf' (== i-1) (applyIf (/= e) f)) t
shallowMapQuery      (Range a b)  f t = withChildren (mapIf' (andF [(>= a),(<= b)]) f . (Terminal (value t) :)) t
shallowMapQuery (Not (Range a b)) f t = withChildren (mapIf' (orF  [(<  a),(>  b)]) f . (Terminal (value t) :)) t
shallowMapQuery      Child        f t = withChildren (map f) t
shallowMapQuery (Not Child)       f t = t
shallowMapQuery (Not (Not q))     f t = shallowMapQuery q f t

--- helpers ---

singleton x = [x]

indent n = (replicate n ' ' ++) . concatMap (\c -> case c of
                                                '\n' -> '\n' : replicate n ' '
                                                _    -> [c])

-- | 'True' if the node is 'Terminal'.
isTerminal (Terminal _) = True
isTerminal _            = False

-- | 'True' if the node is 'Node'.
isNode                  = not . isTerminal

-- | Applies the function to the argument if and only if the predicate
--   matches the argument, otherwise simply returns the argument.
applyIf p f x | p x       = f x
              | otherwise = x

-- | Applies the function to the second argument if and only if the
--   predicate matches the first argument, otherwise simply returns
--   the second argument.
applyIf' p f a b | p a       = f b
                 | otherwise = b

mapIf  p f = map          (applyIf  p f)
mapIf' p f = mapWithIndex (applyIf' p f)

-- | Like 'map', but accumulates the index while mapping and applies
--   that to the function as well.
mapWithIndex f xs = snd (mapAccumL (\ i x -> (i + 1, f i x)) 0 xs)

-- | Like 'mapWithIndex', but for 'filter'.
filterWithIndex p = reverse . snd . foldl (\ (i,l) x -> (i + 1, if p i x then x : l else l)) (0,[])

-- | Returns 'True' if the argument matches all of the predicates in the list.
andF fs = and . flip map fs . flip ($)

-- | Returns 'True' if the argument matches any of the predicates in the list.
orF  fs = or  . flip map fs . flip ($)

-- | Returns the value of the node and any children. Treats an empty
--   'Node' and a 'Terminal' the same way, so it's not lossless.
nodeToList n = Terminal (value n) : children n