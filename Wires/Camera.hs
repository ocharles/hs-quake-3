{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module Wires.Camera
  ( camera
  , CameraEvents(..)
  ) where

import Control.Arrow
import Control.Lens
import Control.Wire
import Control.Wires.Extra
import Data.Monoid
import Linear
import qualified Linear as L
import Prelude hiding ((.), id)
import qualified SDL.Event as SDL
import qualified SDL.Input as SDL
import SDL.Lens

data CameraEvents =
  CameraEvents {sdlEvent :: Event SDL.Event
               ,stepPhysics :: Event Double}

camera :: Wire IO CameraEvents (V3 Float, M44 Float)
camera = proc CameraEvents{..} -> do
  let keyboardEvent =
        filterJust $ fmap (preview (payload . _KeyboardEvent)) sdlEvent

      keyEvent s =
        filterE (\SDL.KeyboardEventData{..} ->
                   SDL.keysymScancode keyboardEventKeysym == s)
                keyboardEvent

      forward     = keyEvent SDL.ScancodeW
      strafeLeft  = keyEvent SDL.ScancodeA
      backward    = keyEvent SDL.ScancodeS
      strafeRight = keyEvent SDL.ScancodeD

      mouseMoved =
        filterJust $
        fmap (preview (payload . _MouseMotionEvent .
                       to SDL.mouseMotionEventRelMotion))
             sdlEvent

  V2 yaw pitch <- scan initialOrientation -< fmap reorientate mouseMoved

  forwardSpeed  <- keySpeed -< forward
  backwardSpeed <- fmap negate keySpeed -< backward
  leftSpeed     <- fmap negate keySpeed -< strafeLeft
  rightSpeed    <- keySpeed -< strafeRight

  let zSpeed = 7 * (forwardSpeed + backwardSpeed)
      xSpeed = 7 * (leftSpeed + rightSpeed)
      orientation = axisAngle (V3 0 1 0) yaw * axisAngle (V3 1 0 0) pitch
      forwardVector = rotate orientation (V3 0 0 (-1))
      upVector = rotate orientation (V3 0 1 0)
      rightVector = rotate (axisAngle (V3 0 1 0) yaw) (V3 1 0 0)

  eyePosition
    <- scan initialPosition
    -< fmap (\dt currentPosition ->
               currentPosition +
               forwardVector ^* zSpeed +
               rightVector ^* xSpeed)
            stepPhysics

  returnA -<
    (eyePosition, lookAt eyePosition (eyePosition + forwardVector) upVector)

  where

    initialPosition = V3 500 10 (-400)
    initialOrientation = V2 0 0
    mouseSensitivity = 0.002

    -- TODO Clamp pitch so you can't flip the camera
    reorientate (L.V2 x y) yawPitch =
      yawPitch + negate (V2 (fromIntegral x * mouseSensitivity) (fromIntegral y * mouseSensitivity))

    keySpeed = proc e ->
      hold 0 -< fmap getSum $
                mconcat [ Sum 1 <$ e `motion` SDL.Pressed
                        , Sum 0 <$ e `motion` SDL.Released
                        ]

    motion e m =
      filterE (\SDL.KeyboardEventData{..} -> keyboardEventKeyMotion == m) e
