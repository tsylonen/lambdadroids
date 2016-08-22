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

-- Game related constants
thrust = 500
drag = 0.99
angular_thrust = 50
angular_drag = 0.8

map_width = 1024 :: Float
map_height = 768 :: Float

-- Some operations on 1x2 vectors
type Vec = (Float, Float)

(<+>) :: Vec -> Vec -> Vec
(a1, a2) <+> (b1, b2) = (a1+b1, a2+b2)

rotate :: Float -> Vec -> Vec
rotate α (a1, a2) = (a1 * cos α - a2 * sin α
                    ,a1 * sin α + a2 * cos α)

scale :: Float -> Vec -> Vec
scale s (a1, a2) = (s * a1, s * a2)

modVec :: Vec -> Vec -> Vec
modVec (a1, a2) (m1, m2) = (a1 `mod'` m1, a2 `mod'` m2)

-- Type definitions
type Vel = Vec
type Pos = Vec
type Angle = Float
type Spin = Float
type TimeDelta = Float

data Entity = Entity {vel :: Vel, pos :: Pos, angle :: Angle, spin :: Spin}
            deriving (Eq, Show)
data Action = Thrust | TurnLeft | TurnRight | Shoot | NoAction
            deriving (Eq, Show)
data WorldState = WorldState {worldShip :: Entity, worldAsteroids :: [Entity], worldBullets :: [Entity]}
                deriving (Show)

keyToAction :: Key -> Action
keyToAction ArrowLeft  = TurnLeft
keyToAction ArrowRight = TurnRight
keyToAction ArrowUp    = Thrust
keyToAction Space      = Shoot
keyToAction _          = NoAction

-- Apply velocities to entity
updateEntity :: TimeDelta -> Entity -> Entity
updateEntity tΔ e = e {pos = newPos, angle = newAngle, vel = newVel, spin = newSpin}
  where newPos   = (flip modVec) (map_width, map_height) $ (pos e) <+> (scale tΔ (vel e))
        newAngle = (angle e) + (spin e) * tΔ
        newSpin  = (spin e) * angular_drag
        newVel = scale drag (vel e)

-- Add control to the ship entity
controlShip :: TimeDelta -> [Action] -> Entity -> Entity
controlShip tΔ input e = e {vel = newVel, spin = newSpin}
  where newVel    = (vel e) <+> thrustVec
        newSpin   = (spin e + ctrlSign * angular_thrust * tΔ)
        ctrlSign  = case (elem TurnLeft input, elem TurnRight input) of
                     (True, False) -> -1
                     (False, True) ->  1
                     (_,_)         ->  0
        thrustVec = if elem Thrust input
                    then rotate (angle e) (tΔ * thrust, 0)
                    else (0,0)

updateWorld :: WorldState -> (TimeDelta, [Action]) -> WorldState
updateWorld world (tΔ, input) = WorldState {worldShip = newShip, worldAsteroids = newAsteroids, worldBullets = newBullets}
  where newShip      = controlShip tΔ input $ updateEntity tΔ (worldShip world)
        newAsteroids = map (updateEntity tΔ) (worldAsteroids world)
        newBullets   = map (updateEntity tΔ) (worldBullets world)

-- begin state of the world
worldStart :: WorldState
worldStart = WorldState {worldShip = Entity {vel = (0,0), pos = (200,200), angle = 0, spin = 0}
                        ,worldAsteroids = []
                        ,worldBullets = []}

renderShip :: ImageData -> Entity -> Picture
renderShip img ship = Translate x y $ Rotate (angle ship + pi/2) $ shipPic
               where (x, y)  = pos ship
                     shipPic = Image Original img

worldRunner :: Var ShineInput WorldState
worldRunner = accumulate updateWorld worldStart . ((,) <$> timeDeltaNumeric <*> actions)
  where actions = fmap (map keyToAction) keysDown


worldRender :: ImageData -> ImageData -> Var ShineInput Picture
worldRender bgrImage shipImage  = fmap render worldRunner
  where render world = (Image (Stretched (map_width*2) (map_height*2)) bgrImage)
                       <> renderShip shipImage (worldShip world)


main :: IO ()
main = runWebGUI $ \ webView -> do
  ctx <- fixedSizeCanvas webView (floor map_width) (floor map_height)
  Just doc <- webViewGetDomDocument webView
  shipImage <- makeImage "http://wahtera.fi/lambdaroids/conjunctionship.png"
  bgrImage <- makeImage "http://wahtera.fi/lambdaroids/A_galactic_gathering.jpg"
  playVarying ctx doc 30 (worldRender bgrImage shipImage)

