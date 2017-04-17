{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | CThis module compiles type-checked shader's into actual GL render passes.
module Quake3.Shader.GL where

import Control.Applicative
import Control.Exception
import Control.Lens hiding ((<.>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import GLObjects hiding (setUniform)
import Graphics.GL
import Linear
import qualified Quake3.Shader.TypeCheck as TC
import RenderGraph
import System.FilePath

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

sortLayer l = Sort . MonoidalMap . Map.singleton l

newtype CompilationCache = CompilationCache
  { textures :: Map String Texture
  } deriving (Monoid)

--------------------------------------------------------------------------------

compileGL
  :: (MonadState CompilationCache m, MonadIO m)
  => TC.Shader
  -> m (Double -> Maybe Texture -> (Sort |> Cull |> MultiplePasses |> BindTexture |> SetUniform Bool |> SetUniform (M33 Float) |> AlphaFunc |> BlendMode |> DepthFunc) ())
compileGL shader = do
  passes <-
    for (shader ^. TC.passes) $ \pass -> do
      let TextureMod texModF = foldMap compileTCMod (pass ^. TC.tCMod)
          textureSource =
            (getLast (pass ^. TC.animMap), getLast (pass ^. TC.map))
      bindTexture_ <-
        case textureSource of
          (Just animMap, _) -> do
            frames <- mapM lookupTexture (TC.animMapFrames animMap)
            return
              (\t _ ->
                 bindTexture
                   (textureName
                      (frames !!
                       (floor (TC.animMapFrequency animMap * t) `mod`
                        length frames))))
          (Nothing, Just m) ->
            case m of
              TC.MapLightMap ->
                return $ \t lm ->
                  case lm of
                    Nothing -> bindTexture 0
                    Just (Texture t) -> bindTexture t
              TC.MapTexture m -> do
                Texture t <- lookupTexture m
                return (\_ _ -> bindTexture t)
          _ -> return $ \_ _ -> bindTexture 0
      return $ \t lm ->
        Compose . bindTexture_ t lm $
        Compose .
        setUniform
          "u_lightMap"
          (case textureSource of
             (Nothing, Just TC.MapLightMap) -> True
             _ -> False) $
        Compose . setUniform "u_texMod" (fmap realToFrac <$> texModF t) $
        Compose .
        alphaFunc
          (fmap
             (\func ->
                case func of
                  TC.Ge128 -> (GL_GEQUAL, 0.5)
                  TC.Lt128 -> (GL_LESS, 0.5)
                  TC.Gt0 -> (GL_GREATER, 0))
             (getLast (pass ^. TC.alphaFunc))) $
        Compose .
        blendMode
          (case getLast (pass ^. TC.blendFunc) of
             Just (src, dst) -> (toBlendFactor src, toBlendFactor dst)
             Nothing -> (GL_ONE, GL_ZERO)) $
        depthFunc
          (case getLast (pass ^. TC.depthFunc) of
             Nothing -> GL_LEQUAL
             Just TC.Equal -> GL_EQUAL) $
        ()
  return $ \t lm ->
    Compose .
    sortLayer
      (let sortExplicit = getLast (shader ^. TC.sort)
           sortFromBlending = do
             guard
               (any
                  (\p -> isJust (getLast (p ^. TC.blendFunc)))
                  (shader ^. TC.passes))
             TC.SeeThrough <$
               guard
                 (any (\p -> getAny (p ^. TC.depthWrite)) (shader ^. TC.passes)) <|>
               return TC.Blend0
       in fromMaybe TC.Opaque (sortExplicit <|> sortFromBlending)) $
    Compose .
    cull
      (case getLast (shader ^. TC.cull) of
         Just TC.BackSided -> CullFace GL_FRONT
         Just TC.TwoSided -> CullNothing
         _ -> CullFace GL_BACK) $
    Compose $ multiplePasses $ fmap (\p -> p t lm) passes

newtype TextureMod = TextureMod (Double -> M33 Double)

instance Monoid TextureMod where
  mempty = TextureMod (\_ -> identity)
  mappend (TextureMod a) (TextureMod b) = TextureMod (\t -> a t !*! b t)

compileTCMod :: TC.TCMod -> TextureMod
compileTCMod (TC.TCModScroll TC.TCScroll {..}) =
  TextureMod
    (\t ->
       identity & column _z . _xy .~
       V2 (t * tcScrollSSpeed) (t * tcScrollTSpeed))
compileTCMod (TC.TCModScale TC.TCScale {..}) =
  TextureMod
    (\_ -> scaled (V3 tcScrollSFactor tcScrollTFactor 1))
compileTCMod (TC.TCModStretch TC.WaveFunction {..}) =
  TextureMod $ \t ->
    let f =
          case wfFunction of
            TC.Sin -> sin
            _ -> sin -- TODO
        factor = recip $ wfBase + wfAmp * f (wfPhase + wfFreq * t)
    in (identity & column _z . _xy .~ 0.5) !*! scaled (V3 factor factor 1) !*!
       (identity & column _z . _xy .~ (-0.5))
compileTCMod (TC.TCModRotate degreesSec) =
  let theta = degreesSec * 0.0175
  in TextureMod $ \t ->
       (identity & column _z . _xy .~ 0.5) !*!
       (V3
          (V3 (cos (t * theta)) (negate (sin (t * theta))) 0)
          (V3 (sin (t * theta)) (cos (t * theta)) 0)
          (V3 0 0 1)) !*!
       (identity & column _z . _xy .~ (-0.5))
compileTCMod _ = TextureMod $ \t -> identity

toBlendFactor :: TC.Factor -> GLenum
toBlendFactor TC.One = GL_ONE
toBlendFactor TC.SrcColor = GL_SRC_COLOR
toBlendFactor TC.DstColor = GL_DST_COLOR
toBlendFactor TC.SrcAlpha = GL_SRC_ALPHA
toBlendFactor TC.OneMinusSrcAlpha = GL_ONE_MINUS_SRC_ALPHA
toBlendFactor TC.OneMinusDstAlpha = GL_ONE_MINUS_DST_ALPHA
toBlendFactor TC.OneMinusSrcColor = GL_ONE_MINUS_SRC_COLOR
toBlendFactor TC.OneMinusDstColor = GL_ONE_MINUS_DST_COLOR
toBlendFactor TC.Zero = GL_ZERO

lookupTexture
  :: (MonadIO m, MonadState CompilationCache m)
  => String -> m Texture
lookupTexture t = do
  ts <- gets textures
  let nameOnly = dropExtension t
  t' <-
    case Map.lookup nameOnly ts of
      Just t -> return t
      Nothing -> do
        t <-
          liftIO $
          loadTexture (nameOnly <.> ".tga") `catch`
          (\(SomeException e) -> loadTexture (nameOnly <.> ".jpg")) `catch`
          (\(SomeException e) -> do
             putStrLn ("Missing " ++ show nameOnly)
             loadTexture "../UVCheckerMap02-512.png")
        modify (\c -> c {textures = Map.insert nameOnly t ts})
        return t
  liftIO (putStrLn $ t ++ " is " ++ show (textureName t'))
  return t'
