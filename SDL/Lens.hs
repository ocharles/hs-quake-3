{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module SDL.Lens where

import Control.Lens
import SDL.Event

makeLensesWith (camelCaseFields) ''Event
makePrisms ''EventPayload
