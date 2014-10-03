import Graphics.UI.GLUT

import MeanCurvature

main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Curvature"
    windowSize $= Size 800 600
    displayCallback $= display
    mainLoop

myLine :: [(GLfloat, GLfloat)]
myLine = [ (sin (2*pi*k/64), cos (2*pi*k/64)) | k <- [1..64] ]

color3f :: GLfloat -> GLfloat -> GLfloat -> IO()
color3f r g b = color $ Color3 r g b

vertex2f :: GLfloat -> GLfloat -> IO()
vertex2f x y = vertex $ Vertex3 x y 0


display :: DisplayCallback
display = do
    clear [ ColorBuffer ]
    renderPrimitive LineLoop $
        mapM_ (\(x, y) -> do
            color3f 1 0 0
            vertex2f x y) myLine
    flush
