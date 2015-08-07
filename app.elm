module App where 

import Signal exposing ((<~))

import Html exposing  (..)
import Html.Attributes as Attri exposing (..)
import Html.Events exposing (..)

import Bootstrap.Html exposing (..)

import Task exposing (Task)

import SvgParser exposing (..) 

import Parser exposing (..)

import Debug

import Regex exposing (..)

main : Signal Html
main = view <~ model 


model : Signal String
model = Signal.foldp update "" query.signal


update : String -> String -> String
update action state = 
               svgToElm action

view : String -> Html
view result =
   div [] [ 
          container_ [ div [class "page-header" ] [h1 [] [text "svg -> elm-svg Function"]]  ]
          , textField result
                       
          ]

fontStyle = [("font-size", "5ex")]

textField :  String -> Html
textField result = 
  row_
    [ colMd_ 2 2 2 [ div [] []]
    , colMd_ 8 8 8 [
                  row_ 
                       [ 
                        colMd_ 12 12 12   [ textarea  [ Attri.style myStyle
                                                      , Attri.value sample
                                                      , on "input" targetValue (Signal.message query.address) ] 
                                                      [] 
                                          , div [class "text-center"] [ div [style fontStyle] [text "↓"] ]
                                          , textarea [Attri.style myStyle , Attri.value result ] [] 
                                          ] 
                       ] 
                   ] 
    , colMd_ 2 2 2 []
    ]


sample : String
sample = 
  """<svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
     viewBox="0 0" enable-background="new 0 0" xml:space="preserve">
<path fill="#E0DA10" d="M8.5,8.2c0,0-4.2,0.5-7,0.8C1.3,9,1,9.2,1,9.5C0.9,9.7,1,10,1.1,10.2c2.1,1.9,5.2,4.7,5.2,4.7c0,0-0.9,4.1-1.4,6.9c0,0.2,0,0.5,0.3,0.7c0.2,0.2,0.5,0.2,0.7,0C8.3,21.1,12,19,12,19s3.7,2.1,6.1,3.5c0.2,0.1,0.5,0.1,0.7,0c0.2-0.2,0.3-0.4,0.3-0.7c-0.6-2.8-1.4-6.9-1.4-6.9s3.1-2.8,5.2-4.7C23,10,23.1,9.7,23,9.5S22.7,9,22.5,9c-2.8-0.3-7-0.8-7-0.8s-1.7-3.8-2.9-6.4c-0.1-0.2-0.3-0.4-0.6-0.4c-0.3,0-0.5,0.2-0.6,0.4C10.2,4.4,8.5,8.2,8.5,8.2z"/>
</svg>"""

myStyle : List (String,String)
myStyle =
    [ ("width", "100%")
    , ("height", "250px") ]

query : Signal.Mailbox String
query =
  Signal.mailbox ""

port firstCase : Task a ()
port firstCase = Signal.send query.address sample

svgToElm : String -> String
svgToElm s = 
  let  string =replace All (regex "\\n") (\_ -> "") s
  in  case parse svgToElmParser string of 
                    Err s -> "not svg?"
                    Ok s -> if s == "" then "not svg?" else s

