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
  Terminal.app intro outro make

intro :: P.Doc
intro =
  P.vcat
    [ P.fillSep
        ["Hi,","thank","you","for","trying","out"
        ,P.green "elm2node"
        ,P.green (P.text (V.toChars V.compiler)) <> "."
        ,"I hope you like it!"
        ]
    , ""
    , P.black "-------------------------------------------------------------------------------"
    , P.black "This is a modified elm compiler to produce a Node.js module exposing pure"
    , P.black "functions."
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
      "Compiles Elm code into Node.js module:"

    example =
      stack
        [ reflow
            "For example:"
        , P.indent 4 $ P.green "elm2node src/Main.elm"
        , reflow
            "This tries to compile an Elm file named src/Main.elm, generating an elm.js\
            \ Node.js module if possible. This Node.js module will expose the same values\
            \ exposed in the `Main.elm` file. Some restrictions:"
        , P.indent 4 $ P.vcat
          [ reflow "* only \"static\" values or functions with one argument can be exposed"
          , P.text "* the only accepted types in exposed values are:"
          , P.indent 4 $
              P.hcat $
                List.intersperse (P.text ", ") $
                  List.map P.green
                    ["Int", "Float", "String", "Maybe", "Records", "List", "Array", "Json.Value"]
            
          , reflow "* exposed user defined types are silently ignored."
          ]
        ]

    makeFlags =
      flags Make.Flags      
        |-- flag "output" Make.output "Specify the name of the resulting JS file. For example --output=assets/elm.js to generate the JS at assets/elm.js or --output=/dev/null to generate no output at all!"
        |-- flag "report" Make.reportType "You can say --report=json to get error messages as JSON. This is only really useful if you are an editor plugin. Humans should avoid it!"
        |-- onOff "optimize" "Turn on optimizations to make code smaller and faster. For example, the compiler renames record fields to be as short as possible and unboxes values to reduce allocation."

  in
  Terminal.Command "make" Uncommon details example (oneOrMore elmFile) makeFlags Make.run




-- HELPERS


stack :: [P.Doc] -> P.Doc
stack docs =
  P.vcat $ List.intersperse "" docs


reflow :: String -> P.Doc
reflow string =
  P.fillSep $ map P.text $ words string
