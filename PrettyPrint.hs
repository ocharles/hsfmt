module PrettyPrint where

import Prelude hiding ((<$>))
import Data.Text.Lazy (Text)
import Data.String (IsString(..))
import qualified Data.Text.Lazy as T
import Data.Monoid
import Data.Text.Lazy.Builder (Builder, fromLazyText, singleton, toLazyText)
import Data.Int (Int64)

data Necessity = Essential | Cosmetic

infixr 5 </>,<//>,<$>,<$$>
infixr 6 <+>,<++>

data Doc
  = Empty
  | Char !Char
  | Line
  | Text Int64
         Builder
  | Union Doc
          Doc
  | Cat Doc
        Doc
  | Spaces Int64
  | Column (Int64 -> Doc)
  | Nesting (Int64 -> Doc)
  | Nest !Int64
         Doc
  | Expanded Doc Doc

instance Monoid Doc where
  mempty = Empty
  mappend = Cat

instance IsString Doc where
  fromString = string . T.pack

empty :: Doc
empty = Empty

line :: Doc
line = Expanded Line space

char      :: Char -> Doc
char '\n' = line
char  c   = Char c

text :: Text -> Doc
text s
  | T.null s = Empty
  | otherwise =
    Text (T.length s)
         (fromLazyText s)

string :: T.Text -> Doc
string str =
  case T.uncons str of
    Nothing -> empty
    Just ('\n',str') -> line <> string str'
    _ ->
      case (T.span (/= '\n') str) of
        (xs,ys) -> text xs <> string ys

data SimpleDoc
  = SEmpty
  | SChar Char
          SimpleDoc
  | SText !Int64
          Builder
          SimpleDoc
  | SLine !Int64
          SimpleDoc

renderSmart :: Int64 -> Doc -> SimpleDoc
renderSmart = renderFits nicestR

data Docs
  = Nil
  | Cons {-# UNPACK #-} !Int64
         Doc
         Docs

renderFits :: (Int64 -> Int64 -> Int64 -> SimpleDoc -> SimpleDoc -> SimpleDoc)
           -> Int64
           -> Doc
           -> SimpleDoc
renderFits nicest w x = best 0 0 (Cons 0 x Nil)
  where
        -- best :: n = indentation of current line
        --         k = current column
        --        (ie. (k >= n) && (k - n == count of inserted characters)
        best _ _ Nil = SEmpty
        best n k (Cons i d ds) =
          case d of
            Empty -> best n k ds
            Char c ->
              let k' = k + 1
              in seq k' $ SChar c (best n k' ds)
            Text l s ->
              let k' = k + l
              in seq k' $ SText l s (best n k' ds)
            Line -> SLine i (best i i ds)
            Cat x y -> best n k (Cons i x (Cons i y ds))
            Nest j x ->
              let i' = i + j
              in seq i' (best n k (Cons i' x ds))
            Union x y ->
              nicest n
                     k
                     w
                     (best n k $ Cons i x ds)
                     (best n k $ Cons i y ds)
            Column f -> best n k (Cons i (f k) ds)
            Nesting f -> best n k (Cons i (f i) ds)
            Spaces l ->
              let k' = k + l
              in seq k' $
                 SText l
                       (indentation l)
                       (best n k' ds)
            Expanded l _ -> best n k (Cons i l ds)

-- @nicestR@ compares the initial lines of the two documents that are nested at
-- least as deep as the current nesting level. If the initial lines of both
-- documents fit within the page width, the document that takes fewer lines is
-- prefered, with preference toward the first.
nicestR :: Int64 -> Int64 -> Int64 -> SimpleDoc -> SimpleDoc -> SimpleDoc
nicestR n k p x' y =
  if fits (min n k) wid x' <= fits (min n k) wid y then x' else y
  where wid = p - k
        inf = 1.0/0 :: Double
        -- @fitsR@ has a little more lookahead: assuming that nesting roughly
        -- corresponds to syntactic depth, @fitsR@ checks that not only the
        -- current line fits, but the entire syntactic structure being formatted
        -- at this level of indentation fits. If we were to remove the second
        -- case for @SLine@, we would check that not only the current structure
        -- fits, but also the rest of the document, which would be slightly more
        -- intelligent but would have exponential runtime (and is prohibitively
        -- expensive in practice).
        -- m = minimum nesting level to fit in
        -- w = the width in which to fit the first line
        fits _ w _           | w < 0     = inf
        fits _ _ SEmpty                  = 0
        fits m w (SChar _ x)             = fits m (w - 1) x
        fits m w (SText l _ x)           = fits m (w - l) x
        fits m _ (SLine i x) | m < i     = 1 + fits m (p - i) x
                             | otherwise = 0

displayB :: SimpleDoc -> Builder
displayB SEmpty        = mempty
displayB (SChar c x)   = c `consB` displayB x
displayB (SText _ s x) = s <> displayB x
displayB (SLine i x)   = '\n' `consB` (indentation i <> displayB x)

consB :: Char -> Builder -> Builder
c `consB` b = singleton c `mappend` b

indentation :: Int64 -> Builder
indentation l = mconcat (replicate (fromIntegral l) (singleton ' '))

group   :: Doc -> Doc
group x = Union (flatten x) x

expanded :: Doc -> Doc -> Doc
expanded = Expanded

flatten                :: Doc -> Doc
flatten (Expanded _ r) = flatten r
flatten (Cat x y)   = Cat (flatten x) (flatten y)
flatten (Nest i x)  = Nest i (flatten x)
flatten Line        = Empty
flatten (Union x _) = flatten x
flatten (Column f)  = Column (flatten . f)
flatten (Nesting f) = Nesting (flatten . f)
flatten other          = other

displayT = toLazyText . displayB

hang     :: Int -> Doc -> Doc
hang i d = align (nest i d)

align   :: Doc -> Doc
align d = column (\k ->
                   nesting (\i -> nest (k - i) d))

nest         :: Int -> Doc -> Doc
nest _ Empty = Empty
nest i x     = Nest (fromIntegral i) x

column   :: (Int -> Doc) -> Doc
column f = Column (f . fromIntegral)

nesting   :: (Int -> Doc) -> Doc
nesting f = Nesting (f . fromIntegral)

(<+>) :: Doc -> Doc -> Doc
Empty <+> y     = y
x     <+> Empty = x
x     <+> y     = x <> space <> y

(</>) :: Doc -> Doc -> Doc
(</>) = splitWithBreak False

(<//>) :: Doc -> Doc -> Doc
(<//>) = splitWithBreak True

splitWithBreak               :: Bool -> Doc -> Doc -> Doc
splitWithBreak _ Empty b     = b
splitWithBreak _ a     Empty = a
splitWithBreak f a     b     = a <> group (mkLine f) <> b

mkLine False = line
mkLine True = linebreak

linebreak = expanded line empty

-- | The document @(x \<$\> y)@ concatenates document @x@ and @y@ with
--   a 'line' in between. (infixr 5)
(<$>) :: Doc -> Doc -> Doc
(<$>) = splitWithLine False

-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@
--   with a 'linebreak' in between. (infixr 5)
(<$$>) :: Doc -> Doc -> Doc
(<$$>) = splitWithLine True

splitWithLine               :: Bool -> Doc -> Doc -> Doc
splitWithLine _ Empty b     = b
splitWithLine _ a     Empty = a
splitWithLine f a     b     = a <> mkLine f <> b

vsep :: [Doc] -> Doc
vsep = fold (<$>)

fold      :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold _ [] = empty
fold f ds = foldr1 f ds

equals = char '='

indent         :: Int -> Doc -> Doc
indent _ Empty = Empty
indent i d     = hang i (spaced i <> d)

spaced   :: Int -> Doc
spaced l = Spaces l'
  where
    l' = fromIntegral l

hsep :: [Doc] -> Doc
hsep = fold (<+>)

softbreak :: Doc
softbreak = group linebreak

-- | The document @spacebreak@ behaves like 'space' when rendered normally
-- but like 'empty' when using 'renderCompact' or 'renderOneLine'.
spacebreak :: Doc
spacebreak = Spaces 1

-- | Document @(squotes x)@ encloses document @x@ with single quotes
--   \"'\".
squotes :: Doc -> Doc
squotes = enclose squote squote

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
--   '\"'.
dquotes :: Doc -> Doc
dquotes = enclose dquote dquote

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
--   \"}\".
braces :: Doc -> Doc
braces = enclose lbrace rbrace

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
--   and \")\".
parens :: Doc -> Doc
parens = enclose lparen rparen

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
--   \"\>\".
angles :: Doc -> Doc
angles = enclose langle rangle

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
--   \"[\" and \"]\".
brackets :: Doc -> Doc
brackets = enclose lbracket rbracket

-- | The document @(enclose l r x)@ encloses document @x@ between
--   documents @l@ and @r@ using @(\<\>)@.
--
--   > enclose l r x = l <> x <> r
enclose       :: Doc -> Doc -> Doc -> Doc
enclose l r x = l <> x <> r

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: Doc
lparen = char '('

-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: Doc
rparen = char ')'

-- | The document @langle@ contains a left angle, \"\<\".
langle :: Doc
langle = char '<'

-- | The document @rangle@ contains a right angle, \">\".
rangle :: Doc
rangle = char '>'

-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: Doc
lbrace = char '{'

-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: Doc
rbrace = char '}'

-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: Doc
lbracket = char '['

-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: Doc
rbracket = char ']'

-- | The document @squote@ contains a single quote, \"'\".
squote :: Doc
squote = char '\''

-- | The document @dquote@ contains a double quote, '\"'.
dquote :: Doc
dquote = char '"'

-- | The document @semi@ contains a semi colon, \";\".
semi :: Doc
semi = char ';'

-- | The document @colon@ contains a colon, \":\".
colon :: Doc
colon = char ':'

-- | The document @comma@ contains a comma, \",\".
comma :: Doc
comma = char ','

-- | The document @space@ contains a single space, \" \".
--
--   > x <+> y = x <> space <> y
space :: Doc
space = char ' '

-- | The document @dot@ contains a single dot, \".\".
dot :: Doc
dot = char '.'

-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: Doc
backslash = char '\\'

-----------------------------------------------------------
-- list, tupled and semiBraces pretty print a list of
-- documents either horizontally or vertically aligned.
-----------------------------------------------------------


-- | The document @(list xs)@ comma separates the documents @xs@ and
--   encloses them in square brackets. The documents are rendered
--   horizontally if that fits the page. Otherwise they are aligned
--   vertically. All comma separators are put in front of the
--   elements.
list :: [Doc] -> Doc
list = encloseSep lbracket rbracket comma

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
--   encloses them in parenthesis. The documents are rendered
--   horizontally if that fits the page. Otherwise they are aligned
--   vertically. All comma separators are put in front of the
--   elements.
tupled :: [Doc] -> Doc
tupled = encloseSep lparen rparen comma

-- | The document @(semiBraces xs)@ separates the documents @xs@ with
--   semi colons and encloses them in braces. The documents are
--   rendered horizontally if that fits the page. Otherwise they are
--   aligned vertically. All semi colons are put in front of the
--   elements.
semiBraces :: [Doc] -> Doc
semiBraces = encloseSep lbrace rbrace semi

-- | The document @(encloseSep l r sep xs)@ concatenates the documents
--   @xs@ separated by @sep@ and encloses the resulting document by
--   @l@ and @r@. The documents are rendered horizontally if that fits
--   the page. Otherwise they are aligned vertically. All separators
--   are put in front of the elements. For example, the combinator
--   'list' can be defined with @encloseSep@:
--
--   > list xs = encloseSep lbracket rbracket comma xs
--   > test = text "list" <+> (list (map int [10,200,3000]))
--
--   Which is laid out with a page width of 20 as:
--
--   @
--   list [10,200,3000]
--   @
--
--   But when the page width is 15, it is laid out as:
--
--   @
--   list [10
--        ,200
--        ,3000]
--   @
encloseSep :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep left right sp ds
  = case ds of
      []  -> left <> right
      [d] -> left <> d <> right
      _   -> align (cat (zipWith (<>) (left : repeat sp) ds) <> right)

-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------


-- | @(punctuate p xs)@ concatenates all documents in @xs@ with
--   document @p@ except for the last document.
--
--   > someText = map text ["words","in","a","tuple"]
--   > test = parens (align (cat (punctuate comma someText)))
--
--   This is laid out on a page width of 20 as:
--
--   @
--   (words,in,a,tuple)
--   @
--
--   But when the page width is 15, it is laid out as:
--
--   @
--   (words,
--    in,
--    a,
--    tuple)
--   @
--
--   (If you want put the commas in front of their elements instead of
--   at the end, you should use 'tupled' or, in general, 'encloseSep'.)
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

-- | The document @(cat xs)@ concatenates all documents @xs@ either
--   horizontally with @(\<\>)@, if it fits the page, or vertically
--   with @(\<$$\>)@.
--
--   > cat xs = group (vcat xs)
cat :: [Doc] -> Doc
cat = group . vcat

vcat :: [Doc] -> Doc
vcat = fold (<$$>)

-- | The document @(sep xs)@ concatenates all documents @xs@ either
--   horizontally with @(\<+\>)@, if it fits the page, or vertically
--   with @(\<$\>)@.
--
--   > sep xs = group (vsep xs)
sep :: [Doc] -> Doc
sep = group . vsep

-- | The document @(fillSep xs)@ concatenates documents @xs@
--   horizontally with @(\<+\>)@ as long as its fits the page, then
--   inserts a @line@ and continues doing that for all documents in
--   @xs@.
--
--   > fillSep xs = foldr (</>) empty xs
fillSep :: [Doc] -> Doc
fillSep = fold (</>)

-- | The document @(fillCat xs)@ concatenates documents @xs@
--   horizontally with @(\<\>)@ as long as its fits the page, then
--   inserts a @linebreak@ and continues doing that for all documents
--   in @xs@.
--
--   > fillCat xs = foldr (<//>) empty xs
fillCat :: [Doc] -> Doc
fillCat = fold (<//>)

-- | The document @(hcat xs)@ concatenates all documents @xs@
--   horizontally with @(\<\>)@.
hcat :: [Doc] -> Doc
hcat = fold (<>)

-- | The document @(x \<++\> y)@ concatenates document @x@ and @y@ with
--   a 'spacebreak' in between.  (infixr 6)
(<++>) :: Doc -> Doc -> Doc
Empty <++> y     = y
x     <++> Empty = x
x     <++> y     = x <> spacebreak <> y


-- | The document @softline@ behaves like 'space' if the resulting
--   output fits the page, otherwise it behaves like 'line'.
--
--   > softline = group line
softline :: Doc
softline = group line

-- | The document @(integer i)@ shows the literal integer @i@ using
--   'text'.
integer   :: Integer -> Doc
integer i = text' i

text' :: (Show a) => a -> Doc
text' = text . T.pack . show
