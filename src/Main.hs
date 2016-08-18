import Prelude hiding ((.))
import Control.Category ((.))
import Graphics.Shine
import Graphics.Shine.FRP.Varying hiding (time)
import Graphics.Shine.Picture
import Graphics.Shine.Image
import Control.Varying.Core
import Web.KeyCode
import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import Data.Fixed (mod')

-- Some constants
thrust = 500
drag = 0.99
angular_thrust = 10
angular_drag = 0.8

map_width = 1024 :: Float
map_height = 768 :: Float


-- Type definitions
type Vel = (Float, Float)
type Pos = (Float, Float)
type Angle = Float
type Spin = Float
type TimeDelta = Float

data Entity = Entity { vel :: Vel, pos :: Pos, angle :: Angle, spin :: Spin }

type GameInput = [Key]

-- Move an entity without control
updateEntity :: Entity -> TimeDelta -> Entity
updateEntity e t = e {pos = newPos, angle = newAngle, vel = (vx * drag, vy * drag), spin = newSpin}
  where newPos   = ((px + vx * t) `mod'` map_width, (py + vy * t) `mod'` map_height)
        newAngle = oldAngle + oldSpin * t
        (px, py) = pos e
        (vx, vy) = vel e
        oldAngle = angle e
        oldSpin  = spin e
        newSpin  = oldSpin * angular_drag

-- Add control to the ship entity
controlShip :: Entity -> TimeDelta -> GameInput -> Entity
controlShip e t i = e {vel = newVel, spin = newSpin}
  where newVel = (vx + ctlx, vy + ctly)
        newSpin = spin e + (ctrlSign (elem ArrowRight i, elem ArrowLeft i)) * 5 * angular_thrust * t
        ctrlSign (True, False) = 1
        ctrlSign (False, True) = -1
        ctrlSign _             = 0
        (vx, vy)               = vel e
        (tx, ty)               = if elem ArrowUp i then (thrust*t, 0) else (0,0) -- thrust or no thrust
        ctlx                   = tx * cos (angle e) - ty * sin (angle e)
        ctly                   = tx * sin (angle e) + ty * cos (angle e)

-- ship is updated by controlling first and updating position next
updateShip :: Entity -> (TimeDelta, GameInput) -> Entity
updateShip s (t, i) = flip updateEntity t $ controlShip s t i

-- begin state of ship
shipStart :: Entity        
shipStart = Entity {vel = (15, 10)
                   ,pos = (200, 200)
                   ,angle = 0
                   ,spin = 0}

-- the ShineInput Entity of the ship
ship :: Var ShineInput Entity
ship = accumulate updateShip shipStart . ((,) <$> timeDeltaNumeric <*> keysDown)

renderShip :: ImageData -> Var ShineInput Picture
renderShip img = fmap f ship
  where f s  = Translate x y $ Rotate (angle s) $ Rotate (pi/2)  $ Colored (Color 0 0 255 1) $ shipPic
               where (x, y)  = pos s
        shipPic = Image Original img

worldRender :: ImageData -> ImageData -> Var ShineInput Picture
worldRender bgrImage shipImage  =
  done (Image (Stretched (map_width*2) (map_height*2)) bgrImage)
  <> renderShip shipImage

main :: IO ()
main = runWebGUI $ \ webView -> do
  ctx <- fixedSizeCanvas webView (floor map_width) (floor map_height)
  Just doc <- webViewGetDomDocument webView
  shipImage <- makeImage "http://wahtera.fi/lambdaroids/conjunctionship.png"
  bgrImage <- makeImage "http://wahtera.fi/lambdaroids/A_galactic_gathering.jpg"
  playVarying ctx doc 30 (worldRender bgrImage shipImage)

