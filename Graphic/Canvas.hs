module Graphic.Canvas(draw) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

draw :: (Point -> Color) -> IO()
draw f = do

  initGUI

  -- generate new Window
  window <- windowNew
  set window [windowTitle := "Easy Drawing",
              windowDefaultWidth := 450,
              windowDefaultHeight := 450]

  canvas <- drawingAreaNew
  canvas `onSizeRequest` return (Requisition 450 450)
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