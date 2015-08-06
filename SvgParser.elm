module SvgParser (svgToElmParser) where


import Parser exposing (..)
import Parser.Char exposing (..)
import String 
import List 
import Regex exposing (..)
import Char 

import Maybe exposing(Maybe(..))

type SVGAttr = SVGAttr (String,String)

svgToElmParser : Parser String
svgToElmParser = 
  spaces                     
  `Parser.andThen` \_      -> many xmlDecl
  `Parser.andThen` \ignore -> spaces
  `Parser.andThen` \_      -> many tag
  `Parser.andThen` \tags   -> spaces
  `Parser.andThen` \_      ->
    let all = filterJoin " , " tags
    in succeed <| all 

xmlDecl :Parser String
xmlDecl =  
  token "<?xml" 
  `Parser.andThen` \x -> many (noneOf "?>") 
  `Parser.andThen` \decl -> token "?>"
  `Parser.andThen` \_ -> succeed ""


tag : Parser (Maybe String)
tag =   
    (token "<" ) 
    `Parser.andThen` \_ -> spaces 
    `Parser.andThen` \_ -> many (noneOf " <>/")
    `Parser.andThen` \name' ->
       let name = String.fromList name'
       in spaces
    `Parser.andThen` \_ -> many attribute
    `Parser.andThen` \attributePart -> spaces
    `Parser.andThen` \_ -> (token ">" <|> token "/>") 
    `Parser.andThen` \close ->
       if String.length close == 2 
        then succeed <| Just <| jenerateOneLineTag name attributePart
        else many elementBody 
            `Parser.andThen` \elements -> spaces
            `Parser.andThen` \_ -> endTag name
            `Parser.andThen` \_ -> spaces
            `Parser.andThen` \_ ->
            succeed <| Just <| jenerateElementTag name  attributePart elements

jenerateOneLineTag : String -> List SVGAttr -> String
jenerateOneLineTag name attributePart = 
    name ++" [ " ++ (filterJoin " , " <| generateAttribute attributePart) ++ " ]" ++" []"

jenerateElementTag : String -> List SVGAttr -> List (Maybe String) ->String
jenerateElementTag name attributePart elements =
    name ++ " [ "++ (filterJoin " , " <| generateAttribute attributePart) ++ " ]" ++ " [ " ++ (filterJoin " , " elements) ++ " ]"


attribute : Parser (SVGAttr)
attribute =
    ((many (noneOf ":-= />")) <|> attriName) 
    `Parser.andThen` \name' ->
     let name = String.fromList name'
     in token "=\""
    `Parser.andThen` \_ -> many (noneOf "\"")
    `Parser.andThen` \value -> symbol '"'
    `Parser.andThen` \_ -> spaces
    `Parser.andThen` \_ ->
    succeed <| SVGAttr ( name , (String.fromList value) )

-- "abc-def" -> "abcDef"
upparName : List Char -> List Char -> List Char
upparName  frontPart backPart  =  
            let up = case List.head backPart of 
                    Just head -> Char.toUpper head 
                    Nothing -> ' '
                upped = case List.tail backPart of
                    Just list -> up :: list 
                    Nothing -> []
            in  List.append frontPart upped

attriName : Parser ( List Char )
attriName = 
            many (noneOf "-:= />")
            `Parser.andThen` \frontPart -> (symbol ':' <|> symbol '-') 
            `Parser.andThen` \key -> many (noneOf ":= />")
            `Parser.andThen` \backPart -> 
            succeed <| upparName frontPart backPart



generateAttribute : List SVGAttr -> List ( Maybe String)
generateAttribute ls = 
            let isIgnoreCase name = name == "xmlns" || name == "xmlnsXlink" --２つは除外
                f x =
                 case x of 
                   SVGAttr (a ,b) ->if isIgnoreCase a 
                                            then Nothing 
                                            else Just <| a ++ " "++ "\"" ++ b ++"\"" 
            in List.map f ls 

elementBody : Parser (Maybe String)
elementBody = 
    spaces *> recursively (\() -> tag) <|> bodyText 

bodyText : Parser (Maybe String)
bodyText = 
        (some <| noneOf "><" )
        `Parser.andThen` \x ->
        let tex = String.fromList x
        in if tex == "\n" then succeed Nothing else succeed Nothing

filterJoin : String -> List (Maybe String) -> String
filterJoin s list =
            let  isJust x =  case x of 
                                  Just str -> True
                                  Nothing -> False
                 justs = List.filter isJust list
                 toNomal jstr = case jstr of 
                                  Just x -> x 
                                  Nothing -> ""
                 result = List.map toNomal justs
            in String.join s  result

noneOf : String -> Parser Char
noneOf list = 
    satisfy (\c -> charTest c list)

charTest : Char -> String -> Bool
charTest c s = 
    let f target a = a /= target
    in List.all (f c) (String.toList s)

endTag : String -> Parser String
endTag str = 
    token "</" *> token str <* symbol '>'


spaces : Parser (List Char)
spaces = 
    many <| satisfy (\s -> isSpace s)

isSpace : Char -> Bool
isSpace c =
        let 
            target = String.fromChar c 
        in contains (regex "[\\s\\n\\t\\f\\v]") target

