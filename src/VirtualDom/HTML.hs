{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VirtualDom.HTML where

import Control.Applicative
import Control.Lens hiding (children, coerce)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Strict
import Data.Coerce
import Data.FMList
import Data.Monoid
import Data.String
import GHCJS.Foreign
import GHCJS.Types
import System.IO.Unsafe
import VirtualDom.Prim

newtype HTML m =
  HTML (m (FMList Node))

instance (Applicative m) => Monoid (HTML m) where
  mempty = HTML (pure mempty)
  HTML l `mappend` HTML r =
    HTML (liftA2 mappend l r)

instance (Applicative m) => IsString (HTML m) where
  fromString s = HTML (pure (pure (fromString s)))

runHTML :: (Functor m,Monoid a) => HTML m -> (Node -> a) -> m a
runHTML (HTML m) run =
  fmap (\f -> unFM f run) m

runHTMLInto :: Functor m
             => Node -> HTML m -> m Node
runHTMLInto el html =
  fmap (into el)
       (runHTML html pure)
  where
  into :: Node -> [Node] -> Node
  into el xs =
    el & _HTMLElement %~
    (children .~
     unsafePerformIO (toArray (coerce xs)))

into :: Applicative m
     => HTML m -> HTML m -> HTML m
into (HTML outer) (HTML inner) =
  HTML ((\o i ->
            let xs = toList i
            in fmap (\h ->
                       h & _HTMLElement %~
                       (children .~
                        unsafePerformIO (toArray (coerce xs))))
                    o) <$>
         outer <*>
         inner)

with :: Applicative m => HTML m -> State HTMLElement () -> HTML m -> HTML m
with (HTML html) s =
  into (HTML (fmap (fmap (over _HTMLElement (execState s))) html))

embed :: Functor m => m (HTML Identity) -> HTML m
embed =
  HTML .
  fmap (\x ->
          case x of
            HTML (Identity l) -> l)

--------------------------------------------------------------------------------
class Term arg result | result -> arg where
  term :: JSString
       -> arg
       -> result

instance (Applicative m, f ~ HTML m,r ~ ()) => Term (StateT HTMLElement Identity r) (f -> HTML m) where
  term name =
    with (HTML (pure (pure (emptyElement name))))

instance Applicative m => Term (HTML m) (HTML m) where
  term name =
    into (HTML (pure (pure (emptyElement name))))

--------------------------------------------------------------------------------

a_ :: (Term arg result) => arg -> result
a_ = term "a"

abbr_ :: (Term arg result) => arg -> result
abbr_ = term "abbr"

address_ :: (Term arg result) => arg -> result
address_ = term "address"

area_ :: (Term arg result) => arg -> result
area_ = term "area"

article_ :: (Term arg result) => arg -> result
article_ = term "article"

aside_ :: (Term arg result) => arg -> result
aside_ = term "aside"

audio_ :: (Term arg result) => arg -> result
audio_ = term "audio"

b_ :: (Term arg result) => arg -> result
b_ = term "b"

base_ :: (Term arg result) => arg -> result
base_ = term "base"

bdi_ :: (Term arg result) => arg -> result
bdi_ = term "bdi"

bdo_ :: (Term arg result) => arg -> result
bdo_ = term "bdo"

blockquote_ :: (Term arg result) => arg -> result
blockquote_ = term "blockquote"

body_ :: (Term arg result) => arg -> result
body_ = term "body"

br_ :: (Term arg result) => arg -> result
br_ = term "br"

button_ :: (Term arg result) => arg -> result
button_ = term "button"

canvas_ :: (Term arg result) => arg -> result
canvas_ = term "canvas"

caption_ :: (Term arg result) => arg -> result
caption_ = term "caption"

cite_ :: (Term arg result) => arg -> result
cite_ = term "cite"

code_ :: (Term arg result) => arg -> result
code_ = term "code"

col_ :: (Term arg result) => arg -> result
col_ = term "col"

colgroup_ :: (Term arg result) => arg -> result
colgroup_ = term "colgroup"

data_ :: (Term arg result) => arg -> result
data_ = term "data"

datalist_ :: (Term arg result) => arg -> result
datalist_ = term "datalist"

dd_ :: (Term arg result) => arg -> result
dd_ = term "dd"

del_ :: (Term arg result) => arg -> result
del_ = term "del"

dfn_ :: (Term arg result) => arg -> result
dfn_ = term "dfn"

div_ :: (Term arg result) => arg -> result
div_ = term "div"

dl_ :: (Term arg result) => arg -> result
dl_ = term "dl"

dt_ :: (Term arg result) => arg -> result
dt_ = term "dt"

em_ :: (Term arg result) => arg -> result
em_ = term "em"

embed_ :: (Term arg result) => arg -> result
embed_ = term "embed"

fieldset_ :: (Term arg result) => arg -> result
fieldset_ = term "fieldset"

figcaption_ :: (Term arg result) => arg -> result
figcaption_ = term "figcaption"

figure_ :: (Term arg result) => arg -> result
figure_ = term "figure"

footer_ :: (Term arg result) => arg -> result
footer_ = term "footer"

form_ :: (Term arg result) => arg -> result
form_ = term "form"

h1_ :: (Term arg result) => arg -> result
h1_ = term "h1"

h2_ :: (Term arg result) => arg -> result
h2_ = term "h2"

h3_ :: (Term arg result) => arg -> result
h3_ = term "h3"

h4_ :: (Term arg result) => arg -> result
h4_ = term "h4"

h5_ :: (Term arg result) => arg -> result
h5_ = term "h5"

h6_ :: (Term arg result) => arg -> result
h6_ = term "h6"

head_ :: (Term arg result) => arg -> result
head_ = term "head"

header_ :: (Term arg result) => arg -> result
header_ = term "header"

hr_ :: (Term arg result) => arg -> result
hr_ = term "hr"

html_ :: (Term arg result) => arg -> result
html_ = term "html"

i_ :: (Term arg result) => arg -> result
i_ = term "i"

iframe_ :: (Term arg result) => arg -> result
iframe_ = term "iframe"

img_ :: (Term arg result) => arg -> result
img_ = term "img"

input_ :: (Term arg result) => arg -> result
input_ = term "input"

ins_ :: (Term arg result) => arg -> result
ins_ = term "ins"

kbd_ :: (Term arg result) => arg -> result
kbd_ = term "kbd"

keygen_ :: (Term arg result) => arg -> result
keygen_ = term "keygen"

label_ :: (Term arg result) => arg -> result
label_ = term "label"

legend_ :: (Term arg result) => arg -> result
legend_ = term "legend"

li_ :: (Term arg result) => arg -> result
li_ = term "li"

link_ :: (Term arg result) => arg -> result
link_ = term "link"

main_ :: (Term arg result) => arg -> result
main_ = term "main"

map_ :: (Term arg result) => arg -> result
map_ = term "map"

mark_ :: (Term arg result) => arg -> result
mark_ = term "mark"

meta_ :: (Term arg result) => arg -> result
meta_ = term "meta"

meter_ :: (Term arg result) => arg -> result
meter_ = term "meter"

nav_ :: (Term arg result) => arg -> result
nav_ = term "nav"

noscript_ :: (Term arg result) => arg -> result
noscript_ = term "noscript"

object_ :: (Term arg result) => arg -> result
object_ = term "object"

ol_ :: (Term arg result) => arg -> result
ol_ = term "ol"

optgroup_ :: (Term arg result) => arg -> result
optgroup_ = term "optgroup"

option_ :: (Term arg result) => arg -> result
option_ = term "option"

output_ :: (Term arg result) => arg -> result
output_ = term "output"

p_ :: (Term arg result) => arg -> result
p_ = term "p"

param_ :: (Term arg result) => arg -> result
param_ = term "param"

pre_ :: (Term arg result) => arg -> result
pre_ = term "pre"

progress_ :: (Term arg result) => arg -> result
progress_ = term "progress"

q_ :: (Term arg result) => arg -> result
q_ = term "q"

rb_ :: (Term arg result) => arg -> result
rb_ = term "rb"

rp_ :: (Term arg result) => arg -> result
rp_ = term "rp"

rt_ :: (Term arg result) => arg -> result
rt_ = term "rt"

rtc_ :: (Term arg result) => arg -> result
rtc_ = term "rtc"

ruby_ :: (Term arg result) => arg -> result
ruby_ = term "ruby"

s_ :: (Term arg result) => arg -> result
s_ = term "s"

samp_ :: (Term arg result) => arg -> result
samp_ = term "samp"

script_ :: (Term arg result) => arg -> result
script_ = term "script"

section_ :: (Term arg result) => arg -> result
section_ = term "section"

select_ :: (Term arg result) => arg -> result
select_ = term "select"

small_ :: (Term arg result) => arg -> result
small_ = term "small"

source_ :: (Term arg result) => arg -> result
source_ = term "source"

span_ :: (Term arg result) => arg -> result
span_ = term "span"

strong_ :: (Term arg result) => arg -> result
strong_ = term "strong"

style_ :: (Term arg result) => arg -> result
style_ = term "style"

sub_ :: (Term arg result) => arg -> result
sub_ = term "sub"

sup_ :: (Term arg result) => arg -> result
sup_ = term "sup"

table_ :: (Term arg result) => arg -> result
table_ = term "table"

tbody_ :: (Term arg result) => arg -> result
tbody_ = term "tbody"

td_ :: (Term arg result) => arg -> result
td_ = term "td"

template_ :: (Term arg result) => arg -> result
template_ = term "template"

textarea_ :: (Term arg result) => arg -> result
textarea_ = term "textarea"

tfoot_ :: (Term arg result) => arg -> result
tfoot_ = term "tfoot"

th_ :: (Term arg result) => arg -> result
th_ = term "th"

thead_ :: (Term arg result) => arg -> result
thead_ = term "thead"

time_ :: (Term arg result) => arg -> result
time_ = term "time"

title_ :: (Term arg result) => arg -> result
title_ = term "title"

tr_ :: (Term arg result) => arg -> result
tr_ = term "tr"

track_ :: (Term arg result) => arg -> result
track_ = term "track"

u_ :: (Term arg result) => arg -> result
u_ = term "u"

ul_ :: (Term arg result) => arg -> result
ul_ = term "ul"

var_ :: (Term arg result) => arg -> result
var_ = term "var"

video_ :: (Term arg result) => arg -> result
video_ = term "video"

wbr_ :: (Term arg result) => arg -> result
wbr_ = term "wbr"
