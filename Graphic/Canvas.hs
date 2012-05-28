{-

Easy Drawing - for easy drawing with haskell for absolute beginners
Copyright (C) 2012 Benjamin Weißenfels <b.pixeldrama@gmail.com>

This program is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

-- | Canvas provides a very easy way to draw something.
module Graphic.Canvas(draw, Gtk.Point(..), Gtk.Color(..)) where

import Graphics.UI.Gtk as Gtk
import Graphics.Rendering.Cairo as Cairo

-- | Needs a higher order function, wich gets a point. The point
-- represents a point in a cartesian coordinate system
draw ::
  (Point -> Color) -> IO()
draw f = do

  initGUI

  -- generate new Window
  window <- windowNew
  set window [windowTitle := "Easy Drawing",
              windowDefaultWidth := 400,
              windowDefaultHeight := 400]

  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 400 400)
  containerAdd window canvas

  onExpose canvas (\event -> do
                      drawin <- widgetGetDrawWindow canvas
                      dimensions <- widgetSizeRequest canvas
                      renderWithDrawable drawin (call f dimensions)
                      return True)

  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
  widgetShowAll window -- can draw only on visible Windows

  onDestroy window mainQuit
  mainGUI

call :: (Point -> Color) -> Requisition -> Render()
call f dimensions = do
   mapM_ (Graphic.Canvas.drawPoint f) pixels
    where
      x = getX dimensions
      y = getY dimensions
      pixels = [(a, b) | a <- [0..x], b <- [0..y]]
      getX,getY :: Requisition -> Int
      getX (Requisition x _) = x
      getY (Requisition _ y) = y

drawPoint :: (Point -> Color) -> Point -> Render()
drawPoint f p = do
  setSourceColor $ f p
  rectangle (fromIntegral $ fst p) (fromIntegral $ snd p) 1 1
  fill
  return ()