module Circle where
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import PointsForRendering

circlePoints radius number
  = [let alpha = twoPi * i /number
     in  (radius*(sin alpha) ,radius * (cos alpha), 0)
    |i <- [1,2..number]]
   where
     twoPi = 2*pi


circle radius = circlePoints radius 100

fillCircle r = displayPoints (circle r) Polygon