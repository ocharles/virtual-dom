{-# LANGUAGE OverloadedStrings #-}

import VirtualDom

main :: IO ()
main =
  do body <- bodyContainer
     initDomDelegator
     renderTo body
              (with body_
                    (on "click" (const (putStrLn "Ouch")))
                    [into h1_ ["Heading"]])
