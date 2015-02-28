{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module VirtualDom.Prim where

import Control.Lens
import Control.Monad.State
import System.IO.Unsafe
import Data.String (IsString(fromString))
import GHCJS.Foreign
import GHCJS.Types
import qualified Data.Immutable as Immutable

-- | An opaque data-type representing a foreign @VNode@ object.
data VNode

-- | A fragment of HTML - either an element with attributes, or a piece of text.
newtype HTML = HTML (JSRef VNode)

foreign import javascript safe
  "new VText($1)" ffiNewVText :: JSString -> JSRef VNode

-- | Construct a 'HTML' text node.
text :: ToJSString t => t -> HTML
text = HTML . ffiNewVText . toJSString

foreign import javascript safe
  "new VNode($1)"
  ffiNewVNode :: JSString -> JSRef VNode

-- | Construct a new 'HTML' fragment consisting of a given element, with no
-- child content or attributes.
emptyElement :: ToJSString t => t -> HTML
emptyElement = HTML . ffiNewVNode . toJSString

instance IsString HTML where
  fromString = text

-- | Witness that a fragment of 'HTML' is pointing to a VNode.
newtype HTMLElement = HTMLElement (JSRef VNode)

foreign import javascript safe
  "$r = $1.type"
  ffiGetVNodeType :: JSRef VNode -> JSString
                      
-- A zero-or-one traversal into a 'HTML' fragment to determine if it is an
-- element or not.
_HTMLElement :: Traversal' HTML HTMLElement
_HTMLElement f (HTML vNode)
  | ffiGetVNodeType vNode == "VirtualNode" =
    fmap (\(HTMLElement vNode') -> HTML vNode')
         (f (HTMLElement vNode))
  | otherwise = pure (HTML vNode)

foreign import javascript safe
  "Immutable.Map($1.properties)"
  ffiVNodeGetProperties :: JSRef VNode -> Immutable.Map

foreign import javascript safe
  "new VNode($1.tagName, $2.toJS(), $1.children, $1.key, $1.namespace)"
  ffiVNodeSetProperties :: JSRef VNode -> Immutable.Map -> JSRef VNode

properties :: Lens' HTMLElement Immutable.Map
properties f (HTMLElement vNode) =
  fmap (HTMLElement . ffiVNodeSetProperties vNode)
       (f (ffiVNodeGetProperties vNode))
       
foreign import javascript safe
  "Immutable.Map($1.properties.attributes)"
  ffiVNodeGetAttributes :: JSRef VNode -> Immutable.Map

foreign import javascript safe
  "new VNode($1.tagName, Immutable.Map($1.properties).set('attributes', $2).toJS(), $1.children, $1.key)"
  ffiVNodeSetAttributes :: JSRef VNode -> Immutable.Map -> JSRef VNode

attributes :: Lens' HTMLElement Immutable.Map
attributes f (HTMLElement vNode) =
  fmap (HTMLElement . ffiVNodeSetAttributes vNode)
       (f (ffiVNodeGetAttributes vNode))

foreign import javascript safe
  "$1.children"
  ffiGetVNodeChildren :: JSRef VNode -> JSArray (JSRef VNode)

foreign import javascript safe
  "new VNode($1.tagName, $1.properties, $2, $1.key, $1.namespace)"
  ffiSetVNodeChildren :: JSRef VNode -> JSArray (JSRef VNode) -> JSRef VNode

children :: Lens' HTMLElement (JSArray (JSRef VNode))
children f (HTMLElement vNode) =
  fmap (HTMLElement . ffiSetVNodeChildren vNode)
       (f (ffiGetVNodeChildren vNode))

foreign import javascript safe
  "$1.key"
  ffiGetVNodeKey :: JSRef VNode -> JSString

foreign import javascript safe
  "new VNode($1.tagName, $1.properties, $1.children, $2, $1.namespace)"
  ffiSetVNodeKey :: JSRef VNode -> JSString -> JSRef VNode

key :: Lens' HTMLElement JSString
key f (HTMLElement vNode) =
  fmap (HTMLElement . ffiSetVNodeKey vNode)
       (f (ffiGetVNodeKey vNode))

foreign import javascript safe
  "$1.namespace"
  ffiGetVNodeNamespace :: JSRef VNode -> JSString

foreign import javascript safe
  "new VNode($1.tagName, $1.properties, $1.children, $1.children, $2)"
  ffiSetVNodeNamespace :: JSRef VNode -> JSString -> JSRef VNode

namespace :: Lens' HTMLElement JSString
namespace f (HTMLElement vNode) =
  fmap (HTMLElement . ffiSetVNodeKey vNode)
       (f (ffiGetVNodeKey vNode))

foreign import javascript safe
  "new VNode($1.tagName, Immutable.Map($1.properties).set('ev-' + $2, evHook($3)).toJS(), $1.children, $1.key, $1.namespace)"
  ffiSetVNodeEvent :: JSRef VNode -> JSString -> JSRef a -> JSRef VNode
  
on :: MonadState HTMLElement m => JSString -> IO () -> m ()
on ev f =
  modify (\(HTMLElement vnode) ->
            HTMLElement
              (ffiSetVNodeEvent
                 vnode
                 ev
                 (unsafePerformIO
                    (syncCallback1 AlwaysRetain
                                   True
                                   (const f)))))
