module Canvas(drawTest)
       where

import Graphic.Canvas as TCanvas
import Test.HUnit

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

drawTest = TCanvas.draw drawBlack 

drawBlack :: (Point -> Color)
drawBlack (a, b)  
  | a == b = Color 0 0 0                          
  | otherwise = Color 65535 65535 65535


