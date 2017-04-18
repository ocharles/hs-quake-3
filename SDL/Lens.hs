{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module SDL.Lens
  ( payload
  , timestamp
  , _KeyboardEvent
  , _MouseMotionEvent
  , _WindowResizedEvent
  ) where

import Control.Lens
import SDL.Event

makeLensesWith (camelCaseFields) ''Event
makePrisms ''EventPayload
