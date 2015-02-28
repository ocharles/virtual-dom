module VirtualDom
       (module VirtualDom.HTML, with, into, renderTo, bodyContainer,
        newTopLevelContainer, on, DOMDelegator, initDomDelegator)
       where

import Control.Lens hiding (coerce, children)
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.IORef
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.Foreign
import GHCJS.Types
import System.IO.Unsafe
import VirtualDom.HTML
import VirtualDom.Prim

--------------------------------------------------------------------------------
with :: HTML -> State HTMLElement () -> [HTML] -> HTML
with el f xs =
  el & _HTMLElement %~
  (execState f .
   (children .~
    unsafePerformIO (toArray (coerce xs))))

into :: HTML -> [HTML] -> HTML
into el xs =
  el & _HTMLElement %~
  (children .~
   unsafePerformIO (toArray (coerce xs)))

--------------------------------------------------------------------------------
data DOMDelegator

foreign import javascript unsafe
  "var dd = DOMDelegator(); dd.listenTo('mouseenter'); dd.listenTo('mouseleave'); $r = dd;"
  initDomDelegator :: IO (JSRef DOMDelegator)

--------------------------------------------------------------------------------
-- An element in the DOM that we can render virtualdom elements to
data VNodePresentation = VNodePresentation (IORef HTML) (IORef Element)

foreign import javascript unsafe
  "vdom($1)"
  ffiVDom :: HTML -> IO Element

data Diff
foreign import javascript unsafe
  "$r = window.virtualDom.diff($1, $2)"
  ffiVirtualDomDiff :: HTML -> HTML -> IO (JSRef Diff)

foreign import javascript unsafe
  "window.virtualDom.patch($1, $2)"
  ffiVirtualDomPatch :: Element -> JSRef Diff -> IO Element

-- Render our internal HTML tree representation into a VNode. We first
-- convert our HTML into a VTree, and then diff this against the container
-- and apply the resulting updates.
renderTo :: VNodePresentation -> HTML -> IO ()
renderTo (VNodePresentation ioref el) e =
  do oldVnode <- readIORef ioref
     patches <- ffiVirtualDomDiff oldVnode e
     el' <- readIORef el
     writeIORef el =<<
       ffiVirtualDomPatch el' patches
     writeIORef ioref e

newTopLevelContainer :: IO VNodePresentation
newTopLevelContainer =
  do initialVNode <- return div_
     currentVNode <- newIORef initialVNode
     el <- ffiVDom initialVNode
     Just doc <- currentDocument
     Just bodyNode <- documentGetBody doc
     _ <-
       nodeAppendChild bodyNode
                       (Just el)
     currentElement <- newIORef el
     return (VNodePresentation currentVNode currentElement)

bodyContainer :: IO VNodePresentation
bodyContainer =
  do initialVNode <- return div_
     currentVNode <- newIORef initialVNode
     Just doc <- currentDocument
     Just bodyNode <- documentGetBody doc
     currentElement <-
       newIORef (toElement bodyNode)
     return (VNodePresentation currentVNode currentElement)
