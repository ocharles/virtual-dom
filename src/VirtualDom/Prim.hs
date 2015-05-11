{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module VirtualDom.Prim where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.String (IsString(fromString))
import GHCJS.DOM.Event
import GHCJS.Foreign
import GHCJS.Types
import System.IO.Unsafe
import qualified Data.Immutable as Immutable
import qualified GHCJS.DOM.HTMLElement as DOM

-- | An opaque data-type representing a foreign @VNode@ object.
data VNode

-- | A HTML node - either an element with attributes, or a piece of text.
newtype Node =
  Node (JSRef VNode)

foreign import javascript safe
  "new virtualdom.VText($1)" ffiNewVText :: JSString -> JSRef VNode

-- | Construct a 'HTML' text node.
text :: ToJSString t => t -> Node
text = Node . ffiNewVText . toJSString

foreign import javascript safe
  "new virtualdom.VNode($1)"
  ffiNewVNode :: JSString -> JSRef VNode

-- | Construct a new 'HTML' element consisting of a given element, with no
-- child content or attributes.
emptyElement :: JSString -> Node
emptyElement = Node . ffiNewVNode

-- | Strings are considered HTML nodes by converting them to text nodes.
instance IsString Node where
  fromString = text

-- | Witness that a 'HTML' node is pointing to a VNode.
newtype HTMLElement = HTMLElement (JSRef VNode)

foreign import javascript safe
  "$r = $1.type"
  ffiGetVNodeType :: JSRef VNode -> JSString

-- A zero-or-one traversal into a 'HTML' fragment to determine if it is an
-- element or not.
_HTMLElement :: Traversal' Node HTMLElement
_HTMLElement f (Node vNode)
  | fromJSString (ffiGetVNodeType vNode) == ("VirtualNode" :: String) =
    fmap (\(HTMLElement vNode') -> Node vNode')
         (f (HTMLElement vNode))
  | otherwise = pure (Node vNode)

foreign import javascript safe
  "Immutable.Map($1.properties)"
  ffiVNodeGetProperties :: JSRef VNode -> Immutable.Map

foreign import javascript safe
  "new virtualdom.VNode($1.tagName, $2.toJS(), $1.children, $1.key, $1.namespace)"
  ffiVNodeSetProperties :: JSRef VNode -> Immutable.Map -> JSRef VNode

properties :: Lens' HTMLElement Immutable.Map
properties f (HTMLElement vNode) =
  fmap (HTMLElement . ffiVNodeSetProperties vNode)
       (f (ffiVNodeGetProperties vNode))

foreign import javascript safe
  "Immutable.Map($1.properties.attributes)"
  ffiVNodeGetAttributes :: JSRef VNode -> Immutable.Map

foreign import javascript safe
  "new virtualdom.VNode($1.tagName, Immutable.Map($1.properties).set('attributes', $2).toJS(), $1.children, $1.key)"
  ffiVNodeSetAttributes :: JSRef VNode -> Immutable.Map -> JSRef VNode

attributes :: Lens' HTMLElement Immutable.Map
attributes f (HTMLElement vNode) =
  fmap (HTMLElement . ffiVNodeSetAttributes vNode)
       (f (ffiVNodeGetAttributes vNode))

foreign import javascript safe
  "$1.children"
  ffiGetVNodeChildren :: JSRef VNode -> JSArray (JSRef VNode)

foreign import javascript safe
  "new virtualdom.VNode($1.tagName, $1.properties, $2, $1.key, $1.namespace)"
  ffiSetVNodeChildren :: JSRef VNode -> JSArray (JSRef VNode) -> JSRef VNode

children :: Lens' HTMLElement (JSArray (JSRef VNode))
children f (HTMLElement vNode) =
  fmap (HTMLElement . ffiSetVNodeChildren vNode)
       (f (ffiGetVNodeChildren vNode))

foreign import javascript safe
  "$1.key"
  ffiGetVNodeKey :: JSRef VNode -> JSString

foreign import javascript safe
  "new virtualdom.VNode($1.tagName, $1.properties, $1.children, $2, $1.namespace)"
  ffiSetVNodeKey :: JSRef VNode -> JSString -> JSRef VNode

key :: Lens' HTMLElement JSString
key f (HTMLElement vNode) =
  fmap (HTMLElement . ffiSetVNodeKey vNode)
       (f (ffiGetVNodeKey vNode))

foreign import javascript safe
  "$1.namespace"
  ffiGetVNodeNamespace :: JSRef VNode -> JSString

foreign import javascript safe
  "new virtualdom.VNode($1.tagName, $1.properties, $1.children, $1.children, $2)"
  ffiSetVNodeNamespace :: JSRef VNode -> JSString -> JSRef VNode

namespace :: Lens' HTMLElement JSString
namespace f (HTMLElement vNode) =
  fmap (HTMLElement . ffiSetVNodeKey vNode)
       (f (ffiGetVNodeKey vNode))

foreign import javascript safe
  "new virtualdom.VNode($1.tagName, Immutable.Map($1.properties).set('ev-' + $2, evHook($3)).toJS(), $1.children, $1.key, $1.namespace)"
  ffiSetVNodeEvent :: JSRef VNode -> JSString -> JSFun (JSRef Event -> IO ()) -> JSRef VNode

on :: MonadState HTMLElement m => JSString -> (JSRef Event -> IO ()) -> m ()
on ev f =
  modify (\(HTMLElement vnode) ->
            HTMLElement
              (ffiSetVNodeEvent vnode
                                ev
                                (unsafePerformIO (syncCallback1 AlwaysRetain True f))))

foreign import javascript safe
  "new virtualdom.VNode($1.tagName, Immutable.Map($1.properties).set('hook-' + $2, Object.create({ hook: $3 })).toJS(), $1.children, $1.key, $1.namespace)"
  ffiRegisterVNodeHook :: JSRef VNode -> JSString -> JSFun (JSRef DOM.HTMLElement -> JSString -> IO ()) -> JSRef VNode

registerHook :: MonadState HTMLElement m
             => JSString -> (JSRef DOM.HTMLElement -> JSString -> IO ()) -> m ()
registerHook hookName f =
  modify (\(HTMLElement vnode) ->
            HTMLElement
              (ffiRegisterVNodeHook vnode
                                    hookName
                                    (unsafePerformIO (syncCallback2 AlwaysRetain True f))))
