module Control.Wires.Extra where

import Prelude hiding ((.), id)
import Control.Wire

filterJust :: Event (Maybe a) -> Event a
filterJust = catMapE id
