{-# LANGUAGE Arrows #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow
import Control.Exception
import Control.Lens ((<&>))
import Control.Monad.Managed
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Control.Wire as W
import qualified Control.Wire.Controller as W
import Data.ByteString.Char8 (unpack)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as T
import Data.Traversable (for)
import qualified Data.Vector.Generic as V
import GLObjects hiding (Texture)
import Graphics.GL.Core33
import Parser
import Quake3.Shader.GL (compileGL)
import Quake3.Shader.Parser (parseShaderFile, shaderName)
import Quake3.Shader.TypeCheck (tcShader)
import qualified Quake3.Shader.TypeCheck as TC
import RenderGraph
import SDL hiding (identity, perspective)
import System.Directory
import System.FilePath
import Text.Megaparsec (parse)
import UI
import Wires.Game

main :: IO ()
main =
  runManaged $ do
    window <- managed (bracket UI.initialize (\_ -> SDL.quit))
    !bspFile <- liftIO $ loadBSP "maps/q3dm1.bsp"
    lightMaps <- liftIO (uploadLightMaps bspFile)
    liftIO $ print (bspEntities bspFile)
    shaderSources <-
      liftIO $ do
        files <- listDirectory "scripts"
        fmap mconcat $
          for [f | f <- files, takeExtension f == ".shader"] $ \p -> do
            src <- T.readFile ("scripts" </> p)
            case parse parseShaderFile p src of
              Left e -> do
                putStrLn $ "Failed to parse " ++ p ++ ": " ++ show e
                return mempty
              Right shaders -> do
                fmap mconcat $
                  for shaders $ \s -> do
                    let (typeChecked, warnings) = tcShader s
                    traverse_ (putStrLn . ((shaderName s ++ ": ") ++)) warnings
                    return (Map.singleton (shaderName s) typeChecked)
    compiledShaders <-
      liftIO $
      flip evalStateT mempty $
      fmap mconcat $
      for (V.toList $ bspTextures bspFile) $ \t ->
        let textureName = unpack (getASCII (Parser.textureName t))
        in Map.singleton t <$>
           case Map.lookup textureName shaderSources of
             Nothing ->
               let textureRoot = textureName
               in compileGL
                    TC.Shader
                    { _cull = mempty
                    , _sort = pure TC.Opaque
                    , _deformVertexes = mempty
                    , _passes =
                        Seq.fromList $ [
                          TC.Pass
                          { _rgbGen = mempty
                          , _blendFunc = mempty
                          , _map = pure TC.MapLightMap
                          , _tCMod = mempty
                          , _tCGen = mempty
                          , _depthFunc = mempty
                          , _depthWrite = mempty
                          , _alphaFunc = mempty
                          , _clampMap = mempty
                          , _alphaGen = mempty
                          , _detail = mempty
                          , _animMap = mempty
                          },
                          TC.Pass
                          { _rgbGen = mempty
                          , _blendFunc = pure (TC.DstColor, TC.Zero)
                          , _map = pure (TC.MapTexture textureRoot)
                          , _tCMod = mempty
                          , _tCGen = mempty
                          , _depthFunc = mempty
                          , _depthWrite = mempty
                          , _alphaFunc = mempty
                          , _clampMap = mempty
                          , _alphaGen = mempty
                          , _detail = mempty
                          , _animMap = mempty
                          }]
                    }
             Just src -> compileGL src
    shader <- liftIO $ loadVertexFragmentProgram "../vs.glsl" "../fs.glsl"
    glUseProgram (programName shader)

    vao <- liftIO $ uploadBSP bspFile
    glBindVertexArray (vertexArrayObjectName vao)

    u_projViewModel <- liftIO $ uniformLocation shader "u_projViewModel"

    liftIO $ W.control $ proc _ -> do
      e <- game bspFile (map GLObjects.textureName lightMaps) compiledShaders u_projViewModel -< ()
      W.onEvent -< e <&> \scene -> do
        runReaderT (draw scene) shader
        SDL.glSwapWindow window

      returnA -< W.never
