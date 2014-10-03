import Graphics.UI.GLUT
import Data.IORef
import MeanCurvature

import LA

main :: IO()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Curvature"
  windowSize $= Size 800 600
  curve <- newIORef $ Curve [ (LA.Point (sin(2*pi*k/64) :: GLfloat) ((cos(2*pi*k/64)) :: GLfloat)) | k <- [1..64] ]
  displayCallback $= display curve
  idleCallback $= Just (idle curve)
  mainLoop

color3f :: GLfloat -> GLfloat -> GLfloat -> IO()
color3f r g b = color $ Color3 r g b

vertex2f :: GLfloat -> GLfloat -> IO()
vertex2f x y = vertex $ Vertex3 x y 0


display :: IORef Curve -> DisplayCallback
display curve = do
  clear [ ColorBuffer ]
  c <- get curve
  let (Curve myLine) = c in
    renderPrimitive LineLoop $
      mapM_ (\(LA.Point x y) -> do
        color3f 1 0 0
        vertex2f x y) myLine
  swapBuffers

idle :: IORef Curve -> IdleCallback
idle curve = do
  modifyIORef' curve step
  postRedisplay Nothing
