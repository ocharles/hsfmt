module PrettyPrint.Lifted (module PrettyPrint.Lifted, Doc, displayT, renderSmart) where

import qualified Data.Monoid as Monoid
import Control.Applicative
import qualified Control.Applicative as Ap
import Data.Text.Lazy (Text)
import PrettyPrint (Doc, displayT, renderSmart)
import qualified PrettyPrint as PP


infixr 5 </>,<//>,<$>,<$$>
infixr 6 <+>,<++>

-----------------------------------------------------------

-- | The document @(list xs)@ comma separates the documents @xs@ and
--   encloses them in square brackets. The documents are rendered
--   horizontally if that fits the page. Otherwise they are aligned
--   vertically. All comma separators are put in front of the
--   elements.
list :: (Functor m) => m [Doc] -> m Doc
list = fmap PP.list

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
--   encloses them in parenthesis. The documents are rendered
--   horizontally if that fits the page. Otherwise they are aligned
--   vertically. All comma separators are put in front of the
--   elements.
tupled :: (Functor m) => m [Doc] -> m Doc
tupled = fmap PP.tupled

-- | The document @(semiBraces xs)@ separates the documents @xs@ with
--   semi colons and encloses them in braces. The documents are
--   rendered horizontally if that fits the page. Otherwise they are
--   aligned vertically. All semi colons are put in front of the
--   elements.
semiBraces :: (Functor m) => m [Doc] -> m Doc
semiBraces = fmap PP.semiBraces

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
encloseSep :: (Applicative m) => m Doc -> m Doc -> m Doc -> m [Doc] -> m Doc
encloseSep = liftA4 PP.encloseSep

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
punctuate :: (Monad m) => m Doc -> m [Doc] -> m [Doc]
punctuate = liftA2 PP.punctuate

-- | The document @(sep xs)@ concatenates all documents @xs@ either
--   horizontally with @(\<+\>)@, if it fits the page, or vertically
--   with @(\<$\>)@.
--
--   > sep xs = group (vsep xs)
sep :: (Monad m) => m [Doc] -> m Doc
sep = fmap PP.sep

-- | The document @(fillSep xs)@ concatenates documents @xs@
--   horizontally with @(\<+\>)@ as long as its fits the page, then
--   inserts a @line@ and continues doing that for all documents in
--   @xs@.
--
--   > fillSep xs = foldr (</>) empty xs
fillSep :: (Monad m) => m [Doc] -> m Doc
fillSep = fmap PP.fillSep

-- | The document @(hsep xs)@ concatenates all documents @xs@
--   horizontally with @(\<+\>)@.
hsep :: (Monad m) => m [Doc] -> m Doc
hsep = fmap PP.hsep

-- | The document @(vsep xs)@ concatenates all documents @xs@
--   vertically with @(\<$\>)@. If a 'group' undoes the line breaks
--   inserted by @vsep@, all documents are separated with a space.
--
--   > someText = map text (words ("text to lay out"))
--   >
--   > test = text "some" <+> vsep someText
--
--   This is laid out as:
--
--   @
--   some text
--   to
--   lay
--   out
--   @
--
--   The 'align' combinator can be used to align the documents under
--   their first element
--
--   > test = text "some" <+> align (vsep someText)
--
--   Which is printed as:
--
--   @
--   some text
--        to
--        lay
--        out
--   @
vsep :: (Monad m) => m [Doc] -> m Doc
vsep = fmap PP.vsep

-- | The document @(cat xs)@ concatenates all documents @xs@ either
--   horizontally with @(\<\>)@, if it fits the page, or vertically
--   with @(\<$$\>)@.
--
--   > cat xs = group (vcat xs)
cat :: (Monad m) => m [Doc] -> m Doc
cat = fmap PP.cat

-- | The document @(fillCat xs)@ concatenates documents @xs@
--   horizontally with @(\<\>)@ as long as its fits the page, then
--   inserts a @linebreak@ and continues doing that for all documents
--   in @xs@.
--
--   > fillCat xs = foldr (<//>) empty xs
fillCat :: (Monad m) => m [Doc] -> m Doc
fillCat = fmap PP.fillCat

-- | The document @(hcat xs)@ concatenates all documents @xs@
--   horizontally with @(\<\>)@.
hcat :: (Monad m) => m [Doc] -> m Doc
hcat = fmap PP.hcat

-- | The document @(vcat xs)@ concatenates all documents @xs@
--   vertically with @(\<$$\>)@. If a 'group' undoes the line breaks
--   inserted by @vcat@, all documents are directly concatenated.
vcat :: (Monad m) => m [Doc] -> m Doc
vcat = fmap PP.vcat

-- | The document @(x \<\> y)@ concatenates document @x@ and document
--   @y@. It is an associative operation having 'empty' as a left and
--   right unit.  (infixr 6)
(<>) :: (Monad m) => m Doc -> m Doc -> m Doc
(<>) = liftA2 (Monoid.<>)

-- | The document @(x \<+\> y)@ concatenates document @x@ and @y@ with
--   a 'space' in between.  (infixr 6)
(<+>) :: (Monad m) => m Doc -> m Doc -> m Doc
(<+>) = liftA2 (PP.<+>)

-- | The document @(x \<++\> y)@ concatenates document @x@ and @y@ with
--   a 'spacebreak' in between.  (infixr 6)
(<++>) :: (Monad m) => m Doc -> m Doc -> m Doc
(<++>) = liftA2 (PP.<++>)

-- | The document @(x \<\/\> y)@ concatenates document @x@ and @y@
--   with a 'softline' in between. This effectively puts @x@ and @y@
--   either next to each other (with a @space@ in between) or
--   underneath each other. (infixr 5)
(</>) :: (Monad m) => m Doc -> m Doc -> m Doc
(</>) = liftA2 (PP.</>)

-- | The document @(x \<\/\/\> y)@ concatenates document @x@ and @y@
--   with a 'softbreak' in between. This effectively puts @x@ and @y@
--   either right next to each other or underneath each other. (infixr
--   5)
(<//>) :: (Monad m) => m Doc -> m Doc -> m Doc
(<//>) = liftA2 (PP.<//>)

-- | The document @(x \<$\> y)@ concatenates document @x@ and @y@ with
--   a 'line' in between. (infixr 5)
(<$>) :: (Monad m) => m Doc -> m Doc -> m Doc
(<$>) = liftA2 (PP.<$>)

-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@
--   with a 'linebreak' in between. (infixr 5)
(<$$>) :: (Monad m) => m Doc -> m Doc -> m Doc
(<$$>) = liftA2 (PP.<$$>)

-- | The document @softline@ behaves like 'space' if the resulting
--   output fits the page, otherwise it behaves like 'line'.
softline :: (Monad m) => m Doc
softline = return PP.softline

-- | The document @softbreak@ behaves like 'empty' if the resulting
--   output fits the page, otherwise it behaves like 'line'.
softbreak :: (Monad m) => m Doc
softbreak = return PP.softbreak

-- | The document @spacebreak@ behaves like 'space' when rendered normally
-- but like 'empty' when using 'renderCompact' or 'renderOneLine'.
spacebreak :: (Monad m) => m Doc
spacebreak = return PP.spacebreak

-- | Document @(squotes x)@ encloses document @x@ with single quotes
--   \"'\".
squotes :: (Monad m) => m Doc -> m Doc
squotes = fmap PP.squotes

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
--   '\"'.
dquotes :: (Monad m) => m Doc -> m Doc
dquotes = fmap PP.dquotes

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
--   \"}\".
braces :: (Monad m) => m Doc -> m Doc
braces = fmap PP.braces

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
--   and \")\".
parens :: (Monad m) => m Doc -> m Doc
parens = fmap PP.parens

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
--   \"\>\".
angles :: (Monad m) => m Doc -> m Doc
angles = fmap PP.angles

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
--   \"[\" and \"]\".
brackets :: (Monad m) => m Doc -> m Doc
brackets = fmap PP.brackets

-- | The document @(enclose l r x)@ encloses document @x@ between
--   documents @l@ and @r@ using @(\<\>)@.
--
--   > enclose l r x = l <> x <> r
enclose :: (Monad m) => m Doc -> m Doc -> m Doc -> m Doc
enclose = liftA3 PP.enclose

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: (Monad m) => m Doc
lparen = return PP.lparen

-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: (Monad m) => m Doc
rparen = return PP.rparen

-- | The document @langle@ contains a left angle, \"\<\".
langle :: (Monad m) => m Doc
langle = return PP.langle

-- | The document @rangle@ contains a right angle, \">\".
rangle :: (Monad m) => m Doc
rangle = return PP.rangle

-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: (Monad m) => m Doc
lbrace = return PP.lbrace

-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: (Monad m) => m Doc
rbrace = return PP.rbrace

-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: (Monad m) => m Doc
lbracket = return PP.lbracket

-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: (Monad m) => m Doc
rbracket = return PP.rbracket

-- | The document @squote@ contains a single quote, \"'\".
squote :: (Monad m) => m Doc
squote = return PP.squote

-- | The document @dquote@ contains a double quote, '\"'.
dquote :: (Monad m) => m Doc
dquote = return PP.dquote

-- | The document @semi@ contains a semi colon, \";\".
semi :: (Monad m) => m Doc
semi = return PP.semi

-- | The document @colon@ contains a colon, \":\".
colon :: (Monad m) => m Doc
colon = return PP.colon

-- | The document @comma@ contains a comma, \",\".
comma :: (Monad m) => m Doc
comma = return PP.comma

-- | The document @space@ contains a single space, \" \".
--
-- > x <+> y = x <> space <> y
space :: (Monad m) => m Doc
space = return PP.space

-- | The document @dot@ contains a single dot, \".\".
dot :: (Monad m) => m Doc
dot = return PP.dot

-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: (Monad m) => m Doc
backslash = return PP.backslash

-- | The document @equals@ contains an equal sign, \"=\".
equals :: (Monad m) => m Doc
equals = return PP.equals

liftA4 f a b c d = f Ap.<$> a <*> b <*> c <*> d


-- | The @group@ combinator is used to specify alternative
--   layouts. The document @(group x)@ undoes all line breaks in
--   document @x@. The resulting line is added to the current line if
--   that fits the page. Otherwise, the document @x@ is rendered
--   without any changes.
group :: (Functor m) => m Doc -> m Doc
group = fmap PP.group

-- | The document @(string s)@ concatenates all characters in @s@
--   using @line@ for newline characters and @char@ for all other
--   characters. It is used instead of 'text' whenever the text
--   contains newline characters.
string :: (Applicative m) => Text -> m Doc
string = pure . PP.string

-- | The document @(indent i x)@ indents document @x@ with @i@ spaces.
--
--   > test = indent 4 (fillSep (map text
--   >         (words "the indent combinator indents these words !")))
--
--   Which lays out with a page width of 20 as:
--
--   @
--       the indent
--       combinator
--       indents these
--       words !
--   @
indent :: (Functor m) => Int -> m Doc -> m Doc
indent = fmap . PP.indent

-- | The hang combinator implements hanging indentation. The document
--   @(hang i x)@ renders document @x@ with a nesting level set to the
--   current column plus @i@. The following example uses hanging
--   indentation for some text:
--
--   > test = hang 4 (fillSep (map text
--   >         (words "the hang combinator indents these words !")))
--
--   Which lays out on a page with a width of 20 characters as:
--
--   @
--   the hang combinator
--       indents these
--       words !
--   @
--
--   The @hang@ combinator is implemented as:
--
--   > hang i x = align (nest i x)
hang :: (Functor m) => Int -> m Doc -> m Doc
hang = fmap . PP.hang

-- | The empty document is, indeed, empty. Although @empty@ has no
--   content, it does have a \'height\' of 1 and behaves exactly like
--   @(text \"\")@ (and is therefore not a unit of @\<$\>@).
empty :: (Monad m) => m Doc
empty = return PP.empty

-- | The document @(char c)@ contains the literal character @c@. The
--   character shouldn't be a newline (@'\n'@), the function 'line'
--   should be used for line breaks.
char :: (Monad m) => Char -> m Doc
char = return . PP.char

-- | The document @(text s)@ contains the literal string @s@. The
--   string shouldn't contain any newline (@'\n'@) characters. If the
--   string contains newline characters, the function 'string' should
--   be used.
text :: (Monad m) => Text -> m Doc
text = return . PP.text

-- | The @line@ document advances to the next line and indents to the
--   current nesting level. Document @line@ behaves like @(text \"
--   \")@ if the line break is undone by 'group' or if rendered with
--   'renderOneLine'.
line :: (Monad m) => m Doc
line = return PP.line

-- | The @linebreak@ document advances to the next line and indents to
--   the current nesting level. Document @linebreak@ behaves like
--   'empty' if the line break is undone by 'group'.
linebreak :: (Monad m) => m Doc
linebreak = return PP.linebreak

-- | The document @(nest i x)@ renders document @x@ with the current
--   indentation level increased by @i@ (See also 'hang', 'align' and
--   'indent').
--
--   > nest 2 (text "hello" <$> text "world") <$> text "!"
--
--   outputs as:
--
--   @
--   hello
--     world
--   !
--   @
nest :: (Monad m) => Int -> m Doc -> m Doc
nest = fmap . PP.nest

-- | The document @(align x)@ renders document @x@ with the nesting
--   level set to the current column. It is used for example to
--   implement 'hang'.
--
--   As an example, we will put a document right above another one,
--   regardless of the current nesting level:
--
--   > x $$ y = align (x <$> y)
--
--   > test = text "hi" <+> (text "nice" $$ text "world")
--
--   which will be laid out as:
--
--   @
--   hi nice
--      world
--   @
align :: (Monad m) => m Doc -> m Doc
align = fmap PP.align

-- | The document @(integer i)@ shows the literal integer @i@ using
--   'text'.
integer :: (Monad m) => Integer -> m Doc
integer = return . PP.integer

expanded :: Applicative m => m Doc -> m Doc -> m Doc
expanded = liftA2 PP.expanded
