{-# LANGUAGE OverloadedStrings #-}

import VirtualDom
import VirtualDom.Prim

main :: IO ()
main =
  do body <- bodyContainer
     initDomDelegator
     renderTo body =<<
       runHTMLInto
         (emptyElement "div")
         (body_ (on "click" (const (putStrLn "Ouch")))
                (h1_ "Heading"))
