{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
  where


import Prelude hiding (init)
import qualified Data.List as List
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Text.PrettyPrint.ANSI.Leijen ((<>))
import Text.Read (readMaybe)

import qualified Elm.Version as V
import Terminal
import Terminal.Helpers

import qualified Make




-- MAIN


main :: IO ()
main =
  Terminal.app intro outro
    [ make ]

intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        ["Hi,","thank","you","for","trying","out"
        ,P.green "Elm"
        ,P.green (P.text (V.toChars V.compiler)) <> "."
        ,"I hope you like it!"
        ]
    , ""
    , P.black "-------------------------------------------------------------------------------"
    , P.black "I highly recommend working through <https://guide.elm-lang.org> to get started."
    , P.black "It teaches many important concepts, including how to use `elm` in the terminal."
    , P.black "-------------------------------------------------------------------------------"
    ]


outro :: P.Doc
outro =
  P.fillSep $ map P.text $ words $
    "Be sure to ask on the Elm slack if you run into trouble! Folks are friendly and\
    \ happy to help out. They hang out there because it is fun, so be kind to get the\
    \ best results!"


-- MAKE


make :: Terminal.Command
make =
  let
    details =
      "The `make` command compiles Elm code into JS or HTML:"

    example =
      stack
        [ reflow
            "For example:"
        , P.indent 4 $ P.green "elm make src/Main.elm"
        , reflow
            "This tries to compile an Elm file named src/Main.elm, generating an index.html\
            \ file if possible."
        ]

    makeFlags =
      flags Make.Flags      
        |-- flag "output" Make.output "Specify the name of the resulting JS file. For example --output=assets/elm.js to generate the JS at assets/elm.js or --output=/dev/null to generate no output at all!"
        |-- flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!"
        |-- onOff "optimize" "Turn on optimizations to make code smaller and faster. For example, the compiler renames record fields to be as short as possible and unboxes values to reduce allocation."

  in
  Terminal.Command "make" Uncommon details example (zeroOrMore elmFile) makeFlags Make.run




-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
