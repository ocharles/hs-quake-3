{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Render (Pass(..), pass) where

import Control.Monad.IO.Class
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Foreign hiding (new)
import GLObjects
import Graphics.GL.Core33
import RenderGraph

data Pass a = Pass
  { passT :: !Double
  , passFramebuffer :: Framebuffer
  , passViewport :: (GLint, GLint, GLint, GLint)
  , passDraw :: a
  }

pass
  :: (RenderNode a (ReaderT r IO), MonadIO m)
  => r -> VertexArrayObject -> Pass a -> m ()
pass program vao (Pass t (Framebuffer fboName) (x, y, w, h) scene) = do
  glBindFramebuffer GL_FRAMEBUFFER fboName
  glViewport x y w h
  glClearColor 0 0 0 0
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  glDepthFunc GL_LESS
  glBlendFunc GL_ONE GL_ZERO
  glEnable GL_CULL_FACE
  glCullFace GL_BACK
  glBindVertexArray (vertexArrayObjectName vao)
  liftIO $ flip runReaderT program $ draw scene
