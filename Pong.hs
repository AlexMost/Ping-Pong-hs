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
    displayCallback $= display game
    idleCallback $= Just (idle game)
    keyboardMouseCallback $= Just (keyboard game)
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

idle game = do
    g <- get game
    let fac = moveFactor g
    game $= g{ball   = moveBall g
             ,leftP  = movePaddle (leftP g) fac
             ,rightP = movePaddle (rightP g) fac
             }
    postRedisplay Nothing


moveBall g = Ball (x+factor*newXDir, y+factor*newYDir) newXDir newYDir
    where
        newXDir
            |    x-ballRadius <= x1 + paddleWidth
              && y+ballRadius >= yl
              && y            <= yl+paddleHeight
              = -xDir

            |    x <= _LEFT-ballRadius = 0
            |    x + ballRadius >= xr
              && y + ballRadius >= yr
              && y              <=yr+paddleHeight
              = -xDir
            |    x >= _RIGHT+ballRadius = 0
            | otherwise = xDir

        newYDir
            | y > _TOP-ballRadius || y < _BOTTOM+ballRadius = -yDir
            | newXDir == 0 = 0
            | otherwise = yDir

        (Ball (x,y) xDir yDir) = ball g
        factor = moveFactor g
        (x1,yl,_) = leftP g
        (xr,yr,_) = rightP g


movePaddle (x, y, dir) factor =
    let y1 = y + factor*dir
        newY = min (_TOP-paddleHeight) $ max _BOTTOM y1
    in (x, newY, dir)

keyboard game (Char 'a') upDown _ _ = do
    g <- get game
    let (x, y, _) = leftP g
    game $= g{leftP=(x, y, paddleDir upDown)}
keyboard game (Char 'l') upDown _ _ = do
    g <- get game
    let (x,y,_) = rightP g
    game $= g{rightP=(x,y,paddleDir upDown)}
keyboard game (Char 'r') Down _ _ = do
    g <- get game
    let Ball (x,y) xD yD = ball g
    let xDir
            | x <= _LEFT + 3*paddleWidth = _INITIAL_BALL_DIR
            | x >= _RIGHT-3*paddleWidth = - _INITIAL_BALL_DIR
            | otherwise = xD
    if (xD == 0)
        then game $= g{ball=Ball (x+4*xDir, y) xDir _INITIAL_BALL_DIR}
        else return()
keyboard _ _ _ _ _ = return()

paddleDir Down = _INITIAL_PADDLE_DIR
paddleDir Up   = _INITIAL_PADDLE_DIR

