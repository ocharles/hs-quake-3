{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Quake3.RenderPipeline where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Function
import qualified Data.Map.Strict as Map
import GLObjects (Program, setUniform)
import Linear
import qualified Quake3.Shader.TypeCheck as TC
import RenderGraph

--------------------------------------------------------------------------------

newtype Sort a = Sort (MonoidalMap TC.SortLayer a)
  deriving (Functor, Monoid)

instance (MonadIO m, RenderNode a m) =>
         RenderNode (Sort a) m where
  draw (Sort m) = do
    Map.foldr
      (\child next -> do
         draw child
         next)
      (return ())
      (getMonoidalMap m)

sortLayer :: TC.SortLayer -> a -> Sort a
sortLayer l = Sort . MonoidalMap . Map.singleton l


--------------------------------------------------------------------------------

data DynamicUniform t = DynamicUniform
  { uniformName :: String
  , uniformAt :: Double -> t
  }

instance Eq (DynamicUniform a) where
  (==) = (==) `on` uniformName

instance Ord (DynamicUniform a) where
  compare = compare `on` uniformName

-- Time-dependent uniforms

newtype SetDynamicUniform uniformType a = SetDynamicUniform (MonoidalMap (DynamicUniform uniformType) a)
  deriving (Functor, Monoid)

setDynamicUniform :: String
                  -> (Double -> uniformType)
                  -> a
                  -> SetDynamicUniform uniformType a
setDynamicUniform k f =
  SetDynamicUniform . MonoidalMap . Map.singleton (DynamicUniform k f)

instance (RenderNode a (ReaderT (Double,Program) IO), GLSLType k) =>
         RenderNode (SetDynamicUniform k a) (ReaderT (Double,Program) IO) where
  draw (SetDynamicUniform m) =
    Map.foldrWithKey
      (\(DynamicUniform name f) children next -> do
         ReaderT $ \(t, program) -> GLObjects.setUniform setter program name (f t)
         draw children
         next)
      (return ())
      (getMonoidalMap m)


--------------------------------------------------------------------------------
newtype Quake3Render a = Quake3Render ((Sort |> Cull |> MultiplePasses |> BindTexture |> SetUniform Bool |> SetUniform (M33 Float) |> AlphaFunc |> BlendMode |> DepthFunc) a)
  deriving (Functor, Monoid)

instance (RenderNode a (ReaderT (Double, Program) IO)) =>
         RenderNode (Quake3Render a) (ReaderT (Double, Program) IO) where
  draw (Quake3Render node) = draw node
