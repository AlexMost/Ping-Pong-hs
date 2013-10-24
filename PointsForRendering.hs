module PointsForRendering where
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL


displayAt (x, y) displayMe = do
    translate$Vector3 x y (0::GLfloat)
    displayMe
    loadIdentity


displayPoints points primitiveShape = do
   renderAs primitiveShape points
   flush


renderAs figure ps = renderPrimitive figure $ makeVertexes ps


makeVertexes :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
makeVertexes = mapM_ (\(x, y, z) -> vertex$Vertex3 x y z)