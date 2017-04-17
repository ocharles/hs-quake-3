{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module RenderGraph where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Bits
import Data.Coerce
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as Map
import GLObjects
import Graphics.GL.Compatibility45
import Linear

-- | The class of types that act as rendering nodes - generally operations that
-- manipulate OpenGL state.

class RenderNode a m where
  draw :: a -> m ()

instance Monad m => RenderNode () m where
  draw = return

-- Composing render graphs (functor composition)

infixr 6 |>

newtype (g |> f) a = Compose { getCompose :: g (f a) }
  deriving (Functor)

instance Monoid (g (f a)) => Monoid ((g |> f) a) where
  mempty = Compose mempty
  mappend (Compose a) (Compose b) = Compose (a `mappend` b)

instance (RenderNode (g (f a)) m, Monad m) => RenderNode ((g |> f) a) m where
  draw (Compose f) = draw f


-- Monoidal maps

newtype MonoidalMap k a = MonoidalMap { getMonoidalMap :: Map k a }

instance Functor (MonoidalMap k) where
  fmap f (MonoidalMap a) = MonoidalMap (fmap f a)

instance (Monoid a, Ord k) =>
         Monoid (MonoidalMap k a) where
  mempty = MonoidalMap Map.empty
  mappend (MonoidalMap a) (MonoidalMap b) =
    MonoidalMap (Map.unionWith mappend a b)
  mconcat =
    coerce . Map.unionsWith mappend . (coerce :: [MonoidalMap k a] -> [Map k a])


-- glCullFace

data CullOp = CullNothing | CullFace GLuint
  deriving (Eq, Ord)

newtype Cull a = Cull (MonoidalMap CullOp a)
  deriving (Functor, Monoid)

cull :: CullOp -> a -> Cull a
cull op a = Cull (MonoidalMap (Map.singleton op a))

instance (MonadIO m, RenderNode a m) =>
         RenderNode (Cull a) m where
  draw (Cull items) = do
    Map.foldrWithKey
      (\cullop children k -> do
         case cullop of
           CullNothing -> glDisable GL_CULL_FACE
           CullFace f -> do
             glEnable GL_CULL_FACE
             glCullFace f
         draw children
         k)
      (return ())
      (getMonoidalMap items)


-- Do arbitrary IO. GL state transitions could be violated here!

newtype GLIO a = GLIO { runGLIO :: (IO (), a) }
  deriving (Monoid)

instance (MonadIO m, RenderNode a m) => RenderNode (GLIO a) m where
  draw (GLIO (io, child)) = do
    liftIO io
    draw child

glIO :: IO () -> a -> GLIO a
glIO io a = GLIO (io, a)


-- Multi-pass rendering

newtype MultiplePasses a = MultiplePasses (MonoidalIntMap a)
  deriving (Functor, Monoid)

instance (Applicative m, RenderNode a m) =>
         RenderNode (MultiplePasses a) m where
  draw (MultiplePasses (MonoidalIntMap passes)) =
    IM.foldr (\a k -> draw a *> k) (pure ()) passes

multiplePasses :: Foldable f => f a -> MultiplePasses a
multiplePasses =
  MultiplePasses . MonoidalIntMap . IM.fromDistinctAscList . zip [0 ..] . toList


-- Set blend mode

newtype BlendMode a = BlendMode (MonoidalMap (GLuint, GLuint) a)
  deriving (Functor, Monoid)

blendMode :: (GLuint, GLuint) -> a -> BlendMode a
blendMode srcDst = BlendMode . MonoidalMap . Map.singleton srcDst

instance (MonadIO m, RenderNode a m) =>
         RenderNode (BlendMode a) m where
  draw (BlendMode m) =
    Map.foldrWithKey
      (\(srcBlend, destBlend) child next -> do
         glBlendFunc srcBlend destBlend
         draw child
         next)
      (return ())
      (getMonoidalMap m)


-- Texture units

newtype BindTexture a = BindTexture (MonoidalIntMap a)
  deriving (Functor, Monoid)

bindTexture :: GLuint -> a -> BindTexture a
bindTexture t = BindTexture . MonoidalIntMap . IM.singleton (fromIntegral t)

instance (MonadIO m, RenderNode a m) =>
         RenderNode (BindTexture a) m where
  draw (BindTexture t) =
    IM.foldrWithKey
      (\texture0 m next -> do
         glActiveTexture GL_TEXTURE0
         glBindTexture GL_TEXTURE_2D (fromIntegral texture0)
         draw m
         next)
      (return ())
      (getMonoidalIntMap t)


-- Monoidal IntMaps

newtype MonoidalIntMap a = MonoidalIntMap { getMonoidalIntMap :: IntMap a }
  deriving (Functor)

instance Monoid a => Monoid (MonoidalIntMap a) where
  mempty = MonoidalIntMap IM.empty
  mappend (MonoidalIntMap a) (MonoidalIntMap b) = MonoidalIntMap (IM.unionWith mappend a b)
  mconcat = coerce . IM.unionsWith mappend . (coerce :: [MonoidalIntMap a] -> [IntMap a])

-- Set uniform parameters

newtype SetUniform uniformType a = SetUniform (MonoidalMap (String, uniformType) a)
  deriving (Functor, Monoid)

setUniform :: String -> uniformType -> a -> SetUniform uniformType a
setUniform k !v = SetUniform . MonoidalMap . Map.singleton (k, v)

class GLSLType a where
  setter :: UniformSetter a

instance GLSLType Bool where
  setter = bool

instance GLSLType (M33 Float) where
  setter = m33

instance (RenderNode a (ReaderT Program IO), GLSLType k) =>
         RenderNode (SetUniform k a) (ReaderT Program IO) where
  draw (SetUniform m) =
    Map.foldrWithKey
      (\(name, v) children next -> do
         ReaderT $ \program -> GLObjects.setUniform setter program name v
         draw children
         next)
      (return ())
      (getMonoidalMap m)


-- Setting depth functions

newtype DepthFunc a = DepthFunc (MonoidalMap GLuint a)
  deriving (Functor, Monoid)

depthFunc :: GLuint -> a -> DepthFunc a
depthFunc d = DepthFunc . MonoidalMap . Map.singleton d

instance (RenderNode a m, MonadIO m) =>
         RenderNode (DepthFunc a) m where
  draw (DepthFunc m) =
    Map.foldrWithKey
      (\d child next -> do
         glDepthFunc d
         draw child
         next)
      (return ())
      (getMonoidalMap m)


-- Alpha testing

newtype AlphaFunc a = AlphaFunc (MonoidalMap (Maybe (GLenum, GLclampf)) a)
  deriving (Functor, Monoid)

alphaFunc :: Maybe (GLuint, GLclampf) -> a -> AlphaFunc a
alphaFunc d = AlphaFunc . MonoidalMap . Map.singleton d

instance (RenderNode a m, MonadIO m) =>
         RenderNode (AlphaFunc a) m where
  draw (AlphaFunc m) =
    Map.foldrWithKey
      (\d child next -> do
         case d of
           Just (func, ref) -> do
             glEnable GL_ALPHA_TEST
             glAlphaFunc func ref
           Nothing -> glDisable GL_ALPHA_TEST
         draw child
         next)
      (return ())
      (getMonoidalMap m)


-- View from a camera, generally the top-most node in rendering.

data Viewport a = Viewport
  { viewViewport :: (GLint, GLint, GLint, GLint)
  , viewChild :: a
  } deriving (Functor)

instance (RenderNode a m, MonadIO m) => RenderNode (Viewport a) m where
  draw (Viewport (x, y, w, h) child) = do
    liftIO $ do
      glViewport x y w h
      glClearColor 0 0 0 0
      glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    draw child

viewport :: (GLint, GLint, GLint, GLint) -> a -> Viewport a
viewport = Viewport
