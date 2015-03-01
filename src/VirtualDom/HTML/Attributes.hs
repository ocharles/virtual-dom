{-# LANGUAGE OverloadedStrings #-}

module VirtualDom.HTML.Attributes where

import Control.Lens
import GHCJS.Foreign
import GHCJS.Types
import VirtualDom.Prim

abbr_ :: Traversal' HTMLElement (Maybe JSString)
abbr_ = attributes . at "abbr"

accept_ :: Traversal' HTMLElement (Maybe JSString)
accept_ = attributes . at "accept"

acceptCharset_ :: Traversal' HTMLElement (Maybe JSString)
acceptCharset_ = attributes . at "accept-charset"

accessKey_ :: Traversal' HTMLElement (Maybe JSString)
accessKey_ = attributes . at "accesskey"

action_ :: Traversal' HTMLElement (Maybe JSString)
action_ = attributes . at "action"

alt_ :: Traversal' HTMLElement (Maybe JSString)
alt_ = attributes . at "alt"

async_ :: Traversal' HTMLElement (Maybe JSString)
async_ = attributes . at "async"

autocomplete_ :: Traversal' HTMLElement (Maybe JSString)
autocomplete_ = attributes . at "autocomplete"

autofocus_ :: Traversal' HTMLElement (Maybe JSString)
autofocus_ = attributes . at "autofocus"

autoplay_ :: Traversal' HTMLElement (Maybe JSString)
autoplay_ = attributes . at "autoplay"

challenge_ :: Traversal' HTMLElement (Maybe JSString)
challenge_ = attributes . at "challenge"

charset_ :: Traversal' HTMLElement (Maybe JSString)
charset_ = attributes . at "charset"

checked_ :: Traversal' HTMLElement (Maybe JSString)
checked_ = attributes . at "checked"

cite_ :: Traversal' HTMLElement (Maybe JSString)
cite_ = attributes . at "cite"

class_ :: Traversal' HTMLElement (Maybe JSString)
class_ = attributes . at "class"

classes :: Traversal' HTMLElement [JSString]
classes =
  class_ .
  anon "" (== "") .
  iso (map toJSString . words . fromJSString)
      (toJSString . unwords . map fromJSString)
  where isEmptyStr = (== ("" :: String))

cols_ :: Traversal' HTMLElement (Maybe JSString)
cols_ = attributes . at "cols"

colspan_ :: Traversal' HTMLElement (Maybe JSString)
colspan_ = attributes . at "colspan"

command_ :: Traversal' HTMLElement (Maybe JSString)
command_ = attributes . at "command"

content_ :: Traversal' HTMLElement (Maybe JSString)
content_ = attributes . at "content"

contentEditable_ :: Traversal' HTMLElement (Maybe JSString)
contentEditable_ = attributes . at "contenteditable"

contextmenu_ :: Traversal' HTMLElement (Maybe JSString)
contextmenu_ = attributes . at "contextmenu"

controls_ :: Traversal' HTMLElement (Maybe JSString)
controls_ = attributes . at "controls"

coords_ :: Traversal' HTMLElement (Maybe JSString)
coords_ = attributes . at "coords"

crossOrigin_ :: Traversal' HTMLElement (Maybe JSString)
crossOrigin_ = attributes . at "crossorigin"

data_ :: Traversal' HTMLElement (Maybe JSString)
data_ = attributes . at "data"

datetime_ :: Traversal' HTMLElement (Maybe JSString)
datetime_ = attributes . at "datetime"

default_ :: Traversal' HTMLElement (Maybe JSString)
default_ = attributes . at "default"

defer_ :: Traversal' HTMLElement (Maybe JSString)
defer_ = attributes . at "defer"

dir_ :: Traversal' HTMLElement (Maybe JSString)
dir_ = attributes . at "dir"

dirName_ :: Traversal' HTMLElement (Maybe JSString)
dirName_ = attributes . at "dirname"

disabled_ :: Traversal' HTMLElement (Maybe JSString)
disabled_ = attributes . at "disabled"

draggable_ :: Traversal' HTMLElement (Maybe JSString)
draggable_ = attributes . at "draggable"

dropzone_ :: Traversal' HTMLElement (Maybe JSString)
dropzone_ = attributes . at "dropzone"

encType_ :: Traversal' HTMLElement (Maybe JSString)
encType_ = attributes . at "enctype"

for_ :: Traversal' HTMLElement (Maybe JSString)
for_ = attributes . at "for"

form_ :: Traversal' HTMLElement (Maybe JSString)
form_ = attributes . at "form"

formAction_ :: Traversal' HTMLElement (Maybe JSString)
formAction_ = attributes . at "formaction"

formEncType_ :: Traversal' HTMLElement (Maybe JSString)
formEncType_ = attributes . at "formenctype"

formMethod_ :: Traversal' HTMLElement (Maybe JSString)
formMethod_ = attributes . at "formmethod"

formNoValidate_ :: Traversal' HTMLElement (Maybe JSString)
formNoValidate_ = attributes . at "formnovalidate"

formTarget_ :: Traversal' HTMLElement (Maybe JSString)
formTarget_ = attributes . at "formtarget"

headers_ :: Traversal' HTMLElement (Maybe JSString)
headers_ = attributes . at "headers"

height_ :: Traversal' HTMLElement (Maybe JSString)
height_ = attributes . at "height"

hidden_ :: Traversal' HTMLElement (Maybe JSString)
hidden_ = attributes . at "hidden"

high_ :: Traversal' HTMLElement (Maybe JSString)
high_ = attributes . at "high"

href_ :: Traversal' HTMLElement (Maybe JSString)
href_ = attributes . at "href"

hrefLang_ :: Traversal' HTMLElement (Maybe JSString)
hrefLang_ = attributes . at "hreflang"

httpEquiv_ :: Traversal' HTMLElement (Maybe JSString)
httpEquiv_ = attributes . at "http-equiv"

icon_ :: Traversal' HTMLElement (Maybe JSString)
icon_ = attributes . at "icon"

id_ :: Traversal' HTMLElement (Maybe JSString)
id_ = attributes . at "id"

inert_ :: Traversal' HTMLElement (Maybe JSString)
inert_ = attributes . at "inert"

inputMode_ :: Traversal' HTMLElement (Maybe JSString)
inputMode_ = attributes . at "inputmode"

isMap_ :: Traversal' HTMLElement (Maybe JSString)
isMap_ = attributes . at "ismap"

itemId_ :: Traversal' HTMLElement (Maybe JSString)
itemId_ = attributes . at "itemid"

itemProp_ :: Traversal' HTMLElement (Maybe JSString)
itemProp_ = attributes . at "itemprop"

itemRef_ :: Traversal' HTMLElement (Maybe JSString)
itemRef_ = attributes . at "itemref"

itemScope_ :: Traversal' HTMLElement (Maybe JSString)
itemScope_ = attributes . at "itemscope"

itemType_ :: Traversal' HTMLElement (Maybe JSString)
itemType_ = attributes . at "itemtype"

keyType_ :: Traversal' HTMLElement (Maybe JSString)
keyType_ = attributes . at "keytype"

kind_ :: Traversal' HTMLElement (Maybe JSString)
kind_ = attributes . at "kind"

label_ :: Traversal' HTMLElement (Maybe JSString)
label_ = attributes . at "label"

lang_ :: Traversal' HTMLElement (Maybe JSString)
lang_ = attributes . at "lang"

list_ :: Traversal' HTMLElement (Maybe JSString)
list_ = attributes . at "list"

loop_ :: Traversal' HTMLElement (Maybe JSString)
loop_ = attributes . at "loop"

low_ :: Traversal' HTMLElement (Maybe JSString)
low_ = attributes . at "low"

manifest_ :: Traversal' HTMLElement (Maybe JSString)
manifest_ = attributes . at "manifest"

max_ :: Traversal' HTMLElement (Maybe JSString)
max_ = attributes . at "max"

maxLength_ :: Traversal' HTMLElement (Maybe JSString)
maxLength_ = attributes . at "maxlength"

media_ :: Traversal' HTMLElement (Maybe JSString)
media_ = attributes . at "media"

mediaGroup_ :: Traversal' HTMLElement (Maybe JSString)
mediaGroup_ = attributes . at "mediagroup"

method_ :: Traversal' HTMLElement (Maybe JSString)
method_ = attributes . at "method"

min_ :: Traversal' HTMLElement (Maybe JSString)
min_ = attributes . at "min"

multiple_ :: Traversal' HTMLElement (Maybe JSString)
multiple_ = attributes . at "multiple"

muted_ :: Traversal' HTMLElement (Maybe JSString)
muted_ = attributes . at "muted"

name_ :: Traversal' HTMLElement (Maybe JSString)
name_ = attributes . at "name"

noValidate_ :: Traversal' HTMLElement (Maybe JSString)
noValidate_ = attributes . at "novalidate"

open_ :: Traversal' HTMLElement (Maybe JSString)
open_ = attributes . at "open"

optimum_ :: Traversal' HTMLElement (Maybe JSString)
optimum_ = attributes . at "optimum"

option_ :: Traversal' HTMLElement (Maybe JSString)
option_ = attributes . at "option"

pattern_ :: Traversal' HTMLElement (Maybe JSString)
pattern_ = attributes . at "pattern"

ping_ :: Traversal' HTMLElement (Maybe JSString)
ping_ = attributes . at "ping"

placeholder_ :: Traversal' HTMLElement (Maybe JSString)
placeholder_ = attributes . at "placeholder"

poster_ :: Traversal' HTMLElement (Maybe JSString)
poster_ = attributes . at "poster"

preload_ :: Traversal' HTMLElement (Maybe JSString)
preload_ = attributes . at "preload"

pubDate_ :: Traversal' HTMLElement (Maybe JSString)
pubDate_ = attributes . at "pubdate"

radioGroup_ :: Traversal' HTMLElement (Maybe JSString)
radioGroup_ = attributes . at "radiogroup"

readonly_ :: Traversal' HTMLElement (Maybe JSString)
readonly_ = attributes . at "readonly"

rel_ :: Traversal' HTMLElement (Maybe JSString)
rel_ = attributes . at "rel"

required_ :: Traversal' HTMLElement (Maybe JSString)
required_ = attributes . at "required"

reversed_ :: Traversal' HTMLElement (Maybe JSString)
reversed_ = attributes . at "reversed"

role_ :: Traversal' HTMLElement (Maybe JSString)
role_ = attributes . at "role"

rows_ :: Traversal' HTMLElement (Maybe JSString)
rows_ = attributes . at "rows"

rowSpan_ :: Traversal' HTMLElement (Maybe JSString)
rowSpan_ = attributes . at "rowspan"

sandbox_ :: Traversal' HTMLElement (Maybe JSString)
sandbox_ = attributes . at "sandbox"

scope_ :: Traversal' HTMLElement (Maybe JSString)
scope_ = attributes . at "scope"

scoped_ :: Traversal' HTMLElement (Maybe JSString)
scoped_ = attributes . at "scoped"

seamless_ :: Traversal' HTMLElement (Maybe JSString)
seamless_ = attributes . at "seamless"

selected_ :: Traversal' HTMLElement (Maybe JSString)
selected_ = attributes . at "selected"

shape_ :: Traversal' HTMLElement (Maybe JSString)
shape_ = attributes . at "shape"

size_ :: Traversal' HTMLElement (Maybe JSString)
size_ = attributes . at "size"

sizes_ :: Traversal' HTMLElement (Maybe JSString)
sizes_ = attributes . at "sizes"

span_ :: Traversal' HTMLElement (Maybe JSString)
span_ = attributes . at "span"

spellcheck_ :: Traversal' HTMLElement (Maybe JSString)
spellcheck_ = attributes . at "spellcheck"

src_ :: Traversal' HTMLElement (Maybe JSString)
src_ = attributes . at "src"

srcDoc_ :: Traversal' HTMLElement (Maybe JSString)
srcDoc_ = attributes . at "srcdoc"

srcLang_ :: Traversal' HTMLElement (Maybe JSString)
srcLang_ = attributes . at "srclang"

srcSet_ :: Traversal' HTMLElement (Maybe JSString)
srcSet_ = attributes . at "srcset"

start_ :: Traversal' HTMLElement (Maybe JSString)
start_ = attributes . at "start"

step_ :: Traversal' HTMLElement (Maybe JSString)
step_ = attributes . at "step"

style_ :: Traversal' HTMLElement (Maybe JSString)
style_ = attributes . at "style"

tabIndex_ :: Traversal' HTMLElement (Maybe JSString)
tabIndex_ = attributes . at "tabindex"

target_ :: Traversal' HTMLElement (Maybe JSString)
target_ = attributes . at "target"

title_ :: Traversal' HTMLElement (Maybe JSString)
title_ = attributes . at "title"

translate_ :: Traversal' HTMLElement (Maybe JSString)
translate_ = attributes . at "translate"

type_ :: Traversal' HTMLElement (Maybe JSString)
type_ = attributes . at "type"

typeMustMatch_ :: Traversal' HTMLElement (Maybe JSString)
typeMustMatch_ = attributes . at "typemustmatch"

useMap_ :: Traversal' HTMLElement (Maybe JSString)
useMap_ = attributes . at "usemap"

value_ :: Traversal' HTMLElement (Maybe JSString)
value_ = attributes . at "value"

width_ :: Traversal' HTMLElement (Maybe JSString)
width_ = attributes . at "width"

wrap_ :: Traversal' HTMLElement (Maybe JSString)
wrap_ = attributes . at "wrap"
