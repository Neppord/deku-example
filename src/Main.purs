module Main where

import Prelude

import Control.Alt ((<|>))
import Deku.Attribute ((!:=))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Deku.DOM.Attr.Height (Height(Height)) as D
import Deku.DOM.Attr.Width (Width(Width)) as D
import Deku.DOM.Attr.X (X(X)) as D
import Deku.DOM.Attr.Y (Y(Y)) as D
import Deku.DOM.Elt.Rect (rect) as D
import Deku.DOM.Elt.Svg (svg) as D
import Deku.Do as Deku
import Deku.DOM.Attr.Fill (Fill(Fill)) as D
import FRP.Event.Keyboard (down) as Keyborad
import Data.Maybe (Maybe(..))
import Data.Compactable (compact)
import Data.Tuple.Nested ((/\))
import Deku.Control ((<#~>))
import Deku.Core (Nut)
import Data.Tuple (Tuple)
import FRP.Event.Time (interval)
import FRP.Event.Class (fold, (*|>))


size :: Int

size = 25

data Direction = Up | Down | Left | Right

key_to_direction :: String -> Maybe Direction
key_to_direction string = case string of 
    "ArrowUp" -> Just Up
    "ArrowDown" -> Just Down
    "ArrowLeft" -> Just Left
    "ArrowRight" -> Just Right
    _ -> Nothing

point :: Direction -> Tuple Int Int
point d = case d of 
    Up -> 0 /\ -1
    Down -> 0 /\ 1
    Left -> -1 /\ 0
    Right -> 1 /\ 0

player :: Tuple Int Int -> Nut
player (x /\ y) = D.rect
     ( D.X !:= show ((x + 1) * size)
         <|> D.Y !:= show ((y + 1) * size)
         <|> D.Width !:= show size
         <|> D.Height !:= show size
         <|> D.Fill !:= "green"
     )
     []

main :: Effect Unit
main = runInBody Deku.do
  let direction = Keyborad.down 
        <#> key_to_direction  
        # compact
      tick = interval 500
      position = fold add (5 /\ 5) (tick *|> (pure Right <|> direction) <#> point)  
  D.svg (D.Width !:= "100vw" <|> D.Height !:= "100vh")
    [ position <#~> player
    ]