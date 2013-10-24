import Graphics.Rendering.OpenGL
import Data.IORef
import Graphics.UI.GLUT as GLUT
import PointsForRendering
import Circle
import Rectangle

_LEFT   = -1
_RIGHT  =  1
_TOP    =  1
_BOTTOM = -1

paddleWidth  = 0.07
paddleHeight = 0.2
ballRadius   = 0.035

_INITIAL_WIDTH :: GLsizei
_INITIAL_WIDTH = 400

_INITIAL_HEIGHT :: GLsizei
_INITIAL_HEIGHT = 200

_INITIAL_BALL_DIR = 0.002
_INITIAL_PADDLE_DIR = 0.005


data Ball = Ball (GLfloat, GLfloat) GLfloat GLfloat


type Paddle = (GLfloat, GLfloat, GLfloat)


data Game = Game { ball :: Ball
                 , leftP, rightP :: Paddle
                 , points :: (Int, Int)
                 , moveFactor :: GLfloat
                 }

displayPaddle (x, y, _) = preservingMatrix$do
    translate$Vector3 (paddleWidth/2) (paddleHeight/2) 0
    displayAt (x, y)$myRect paddleWidth paddleHeight


initGame = Game {ball=Ball (-0.8, 0.3) _INITIAL_BALL_DIR _INITIAL_BALL_DIR
                , leftP=(_LEFT+paddleWidth, _BOTTOM, 0)
                , rightP=(_RIGHT-2*paddleWidth, _BOTTOM, 0)
                , points=(0, 0)
                , moveFactor=1
                }


main = do
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow progName
    game <- newIORef initGame
    --windowSize $= Size _INITIAL_WIDTH _INITIAL_HEIGHT
    displayCallback $= display game
    --idleCallback $= Just (idle game)
    --keyboardMouseCallback $= Just (keyboard game)
    --reshapeCallback $= Just (reshape game)
    mainLoop


display game = do
    clear [ColorBuffer]
    g <- get game
    let (Ball pos xDir yDir) = ball g
    -- a ball is a circle
    displayAt pos $ fillCircle ballRadius
    displayPaddle $ leftP g
    displayPaddle $ rightP g
    swapBuffers

