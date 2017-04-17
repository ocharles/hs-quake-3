{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module GLObjects where

import Codec.Picture
import Codec.Picture.Types
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State.Strict
       (State, runState, gets, modify)
import Data.Foldable
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import Data.Traversable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Foreign
import Foreign.C
import Graphics.GL.ARB.DirectStateAccess
import Graphics.GL.ARB.SeparateShaderObjects
import Graphics.GL.Core33
import Linear

newtype Texture = Texture
  { textureName :: GLuint
  } deriving (Eq, Ord)

newTexture2D :: GLsizei -> GLenum -> GLsizei -> GLsizei -> IO Texture
newTexture2D levels internalFormat width height = do
  name <- create (glCreateTextures GL_TEXTURE_2D)
  glTextureStorage2D name levels internalFormat width height
  pure (Texture name)

uploadTexture2D
  :: ()
  => SV.Vector Word8 -> IO Texture
uploadTexture2D pixels = do
  t <- newTexture2D 1 GL_RGBA32F 128 128
  glPixelStorei GL_UNPACK_LSB_FIRST 0
  glPixelStorei GL_UNPACK_SWAP_BYTES 0
  glPixelStorei GL_UNPACK_ROW_LENGTH 0
  glPixelStorei GL_UNPACK_IMAGE_HEIGHT 0
  glPixelStorei GL_UNPACK_SKIP_ROWS 0
  glPixelStorei GL_UNPACK_SKIP_PIXELS 0
  glPixelStorei GL_UNPACK_SKIP_IMAGES 0
  glPixelStorei GL_UNPACK_ALIGNMENT 1
  SV.unsafeWith pixels $ \ptr ->
    glTextureSubImage2D (textureName t) 0 0 0 128 128 GL_RGB GL_UNSIGNED_BYTE ptr
  pure t

loadTexture :: FilePath -> IO Texture
loadTexture filePath = do
  res <- readImage filePath
  t <-
    case res of
      Left e -> error ("Could not load texture: " ++ e)
      Right img ->
        case img of
          ImageY8 _ -> error "ImageY8"
          ImageY16 _ -> error "(Image Pixel16)"
          ImageYF _ -> error "(Image PixelF)"
          ImageYA8 _ -> error "(Image PixelYA8)"
          ImageYA16 _ -> error "(Image PixelYA16)"
          ImageRGB16 _ -> error "(Image PixelRGB16)"
          ImageRGBF _ -> error "(Image PixelRGBF)"
          ImageYCbCr8 (convertImage -> Image width height pixels :: Image PixelRGB8) -> do
            t <-
              newTexture2D
                (floor (logBase 2 (fromIntegral (max width height))))
                GL_SRGB8
                (fromIntegral width)
                (fromIntegral height)
            SV.unsafeWith
              pixels
              (glTextureSubImage2D
                 (textureName t)
                 0
                 0
                 0
                 (fromIntegral width)
                 (fromIntegral height)
                 GL_RGB
                 GL_UNSIGNED_BYTE .
               castPtr)
            return t
          ImageCMYK8 _ -> error "(Image PixelCMYK8)"
          ImageCMYK16 _ -> error "(Image PixelCMYK16)"
          ImageRGBA8 (Image width height pixels) -> do
            t <-
              newTexture2D
                (floor (logBase 2 (fromIntegral (max width height))))
                GL_SRGB8_ALPHA8
                (fromIntegral width)
                (fromIntegral height)
            SV.unsafeWith
              pixels
              (glTextureSubImage2D
                 (textureName t)
                 0
                 0
                 0
                 (fromIntegral width)
                 (fromIntegral height)
                 GL_RGBA
                 GL_UNSIGNED_BYTE .
               castPtr)
            pure t
          ImageRGB8 (Image width height pixels) -> do
            t <-
              newTexture2D
                (floor (logBase 2 (fromIntegral (max width height))))
                GL_SRGB8
                (fromIntegral width)
                (fromIntegral height)
            SV.unsafeWith
              pixels
              (glTextureSubImage2D
                 (textureName t)
                 0
                 0
                 0
                 (fromIntegral width)
                 (fromIntegral height)
                 GL_RGB
                 GL_UNSIGNED_BYTE .
               castPtr)
            return t
  glActiveTexture GL_TEXTURE1
  glGenerateTextureMipmap (textureName t)
  glTextureParameteri
    (textureName t)
    GL_TEXTURE_MIN_FILTER
    (fromIntegral GL_LINEAR_MIPMAP_LINEAR)
  glTextureParameteri
    (textureName t)
    GL_TEXTURE_MAG_FILTER
    (fromIntegral GL_LINEAR)
  pure t

newtype Renderbuffer = Renderbuffer
  { renderbufferName :: GLuint
  } deriving (Eq, Ord)

newRenderbuffer :: GLenum -> GLsizei -> GLsizei -> IO Renderbuffer
newRenderbuffer internalFormat width height = do
  name <- create glCreateRenderbuffers
  glNamedRenderbufferStorage name internalFormat width height
  pure (Renderbuffer name)

newtype Framebuffer = Framebuffer
  { framebufferName :: GLuint
  } deriving (Eq, Ord)

data AttachTo
  = AttachToTexture Texture
                    GLint
  | AttachToRenderbuffer Renderbuffer

data FramebufferAttachment
  = ColorAttachment GLenum
  | DepthAttachment
  | StencilAttachment

attachmentForGL :: FramebufferAttachment -> GLenum
attachmentForGL (ColorAttachment n) = GL_COLOR_ATTACHMENT0 + fromIntegral n
attachmentForGL DepthAttachment = GL_DEPTH_ATTACHMENT
attachmentForGL StencilAttachment = GL_STENCIL_ATTACHMENT

newFramebuffer :: (FramebufferAttachment -> Maybe AttachTo) -> IO Framebuffer
newFramebuffer f = do
  name <- create glCreateFramebuffers
  maxColorAttachments <-
    alloca (\ptr -> glGetIntegerv GL_MAX_COLOR_ATTACHMENTS ptr *> peek ptr)
  for_
    (DepthAttachment :
     StencilAttachment :
     map ColorAttachment [0 .. fromIntegral maxColorAttachments - 1])
    (\attachment ->
       for_
         (f attachment)
         (\case
            AttachToTexture (Texture t) level ->
              glNamedFramebufferTexture
                name
                (attachmentForGL attachment)
                t
                level
            AttachToRenderbuffer (Renderbuffer rb) ->
              glNamedFramebufferRenderbuffer
                name
                (attachmentForGL attachment)
                GL_RENDERBUFFER
                rb))
  status <- glCheckNamedFramebufferStatus name GL_FRAMEBUFFER
  putStrLn
    (case status of
       GL_FRAMEBUFFER_UNDEFINED -> "Framebuffer undefined"
       GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT -> "Incomplete attachment"
       GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT -> "Missing attachment"
       GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER -> "Incomplete draw buffer"
       GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER -> "Incomplete read buffer"
       GL_FRAMEBUFFER_UNSUPPORTED -> "Unsupported"
       GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE -> "Incomplete multisample"
       GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS -> "Incomplete layer targets"
       GL_FRAMEBUFFER_COMPLETE -> "Complete"
       _ -> "Unknown status")
  pure (Framebuffer name)

data StageSource
  = VertexShader
  | FragmentShader
  deriving (Show)

glShaderStage :: StageSource -> GLenum
glShaderStage VertexShader = GL_VERTEX_SHADER
glShaderStage FragmentShader = GL_FRAGMENT_SHADER

newtype Program = Program
  { programName :: GLuint
  } deriving (Eq, Ord)

newProgram :: (StageSource -> Maybe Text) -> IO Program
newProgram f = do
  name <- glCreateProgram
  shaders <-
    for
      [VertexShader, FragmentShader]
      (\stage ->
         for
           (f stage)
           (\src -> do
              shaderName <- glCreateShader (glShaderStage stage)
              withCString
                (unpack src)
                (\srcPtr ->
                   withArray
                     [srcPtr]
                     (\srcs -> glShaderSource shaderName 1 srcs nullPtr))
              glCompileShader shaderName
              compiled <-
                alloca
                  (\ptr ->
                     glGetShaderiv shaderName GL_COMPILE_STATUS ptr *> peek ptr)
              unless
                (fromIntegral compiled == GL_TRUE)
                (do putStrLn ("Shader stage failed to compile: " <> show stage)
                    T.putStrLn src
                    logLen <-
                      alloca
                        (\ptr ->
                           glGetShaderiv shaderName GL_INFO_LOG_LENGTH ptr *>
                           peek ptr)
                    allocaBytes (fromIntegral logLen) $ \infoLogPtr ->
                      alloca $ \lenPtr -> do
                        glGetShaderInfoLog shaderName 1024 lenPtr infoLogPtr
                        peekCString infoLogPtr >>= putStrLn)
              glAttachShader name shaderName
              pure shaderName))
  withCString "a_position" (glBindAttribLocation name attribPosition)
  withCString "a_normal" (glBindAttribLocation name attribNormal)
  withCString "a_uv_0" (glBindAttribLocation name (attribUV 0))
  withCString "a_uv_1" (glBindAttribLocation name (attribUV 1))
  glLinkProgram name
  compiled <-
    alloca (\ptr -> glGetProgramiv name GL_LINK_STATUS ptr *> peek ptr)
  unless
    (fromIntegral compiled == GL_TRUE)
    (do putStrLn "Program failed to link"
        logLen <-
          alloca
            (\ptr -> glGetProgramiv name GL_INFO_LOG_LENGTH ptr *> peek ptr)
        allocaBytes (fromIntegral logLen) $ \infoLogPtr ->
          alloca $ \lenPtr -> do
            glGetProgramInfoLog name 1024 lenPtr infoLogPtr
            peekCString infoLogPtr >>= putStrLn)
  for_ (catMaybes shaders) (glDetachShader name)
  pure (Program name)

create
  :: (Num a, Storable b)
  => (a -> Ptr b -> IO c) -> IO b
create m = alloca (\ptr -> m 1 ptr *> peek ptr)

attribPosition, attribNormal, attribColor :: GLuint
attribPosition = 0
attribNormal = 1
attribColor = 4

attribUV :: GLuint -> GLuint
attribUV 0 = 2
attribUV 1 = 3

newtype VertexArrayObject = VertexArrayObject
  { vertexArrayObjectName :: GLuint
  } deriving (Eq, Ord)

newtype UniformSetter a = UniformSetter
  { setUniform :: Program -> String -> a -> IO ()
  }

data UniformLocation = UniformLocation Program Int32

uniformLocation :: Program -> String -> IO UniformLocation
uniformLocation program@(Program p) u =
  UniformLocation program <$>
  withCString u (glGetUniformLocation p)

bool :: UniformSetter Bool
bool =
  UniformSetter
    (\(Program p) uniform value -> do
       uView <- withCString uniform (glGetUniformLocation p)
       glProgramUniform1ui p uView (if value then 1 else 0))

m44 :: UniformSetter (M44 Float)
m44 =
  UniformSetter
    (\(Program p) uniform value -> do
       uView <- withCString uniform (glGetUniformLocation p)
       with
         value
         (glProgramUniformMatrix4fv p uView 1 (fromIntegral GL_TRUE) . castPtr))

m22 :: UniformSetter (M22 Float)
m22 =
  UniformSetter
    (\(Program p) uniform value -> do
       uView <- withCString uniform (glGetUniformLocation p)
       with
         value
         (glProgramUniformMatrix2fv p uView 1 (fromIntegral GL_TRUE) . castPtr))

m33 :: UniformSetter (M33 Float)
m33 =
  UniformSetter
    (\(Program p) uniform value -> do
       uView <- withCString uniform (glGetUniformLocation p)
       with
         value
         (glProgramUniformMatrix3fv p uView 1 (fromIntegral GL_TRUE) . castPtr))

v4Array :: UniformSetter [V4 Float]
v4Array =
  UniformSetter
    (\(Program p) uniform value -> do
       location <- withCString uniform (glGetUniformLocation p)
       withArray
         value
         (glProgramUniform4fv p location (fromIntegral (length value)) . castPtr))

v4i32
  :: UniformSetter (V4 Int32)
v4i32 =
  UniformSetter
    (\(Program p) uniform (V4 x y z w) -> do
       location <- withCString uniform (glGetUniformLocation p)
       glProgramUniform4i p location x y z w)

v2f :: UniformSetter (V2 Float)
v2f =
  UniformSetter
    (\(Program p) uniform value -> do
       location <- withCString uniform (glGetUniformLocation p)
       with value (glProgramUniform2fv p location 1 . castPtr))

textureUnit :: UniformSetter GLint
textureUnit =
  UniformSetter
    (\(Program p) uniform value -> do
       location <- withCString uniform (glGetUniformLocation p)
       glProgramUniform1i p location value)

data Vertex =
  Vertex (V3 Float)
         (V3 Float)
         (V2 Float)
  deriving (Show, Eq, Ord)

loadVertexFragmentProgram :: FilePath -> FilePath -> IO Program
loadVertexFragmentProgram vs fs = do
  do depthVS <- T.readFile vs
     depthFS <- T.readFile fs
     newProgram
       (\case
          VertexShader -> Just depthVS
          FragmentShader -> Just depthFS)

data Attribute
  = Position
  | Normal
  | UV GLuint
  | Color

data VertexAttrib a = VertexAttrib
  { vertexAttribSize :: GLuint
  , vertexAttribPoke :: Ptr a -> a -> IO ()
  , vertexAttribFormat :: [(Attribute, GLint, GLenum, GLenum, GLuint)]
  }

instance Contravariant VertexAttrib where
  contramap f (VertexAttrib sz poke_ fmt) =
    VertexAttrib sz (\ptr v -> poke_ (castPtr ptr) (f v)) fmt

instance Divisible VertexAttrib where
  conquer = VertexAttrib 0 (\_ _ -> return ()) []
  divide f fb fc =
    VertexAttrib
    { vertexAttribSize = vertexAttribSize fb + vertexAttribSize fc
    , vertexAttribPoke =
        \ptr a -> do
          let (b, c) = f a
          vertexAttribPoke fb (castPtr ptr) b
          vertexAttribPoke
            fc
            (castPtr ptr `plusPtr` fromIntegral (vertexAttribSize fb))
            c
    , vertexAttribFormat =
        vertexAttribFormat fb <>
        (map
           (\(a, b, c, d, e) -> (a, b, c, d, e + vertexAttribSize fb))
           (vertexAttribFormat fc))
    }

position :: VertexAttrib (V3 Float)
position =
  VertexAttrib
  { vertexAttribSize = fromIntegral (sizeOf (0 :: V3 Float))
  , vertexAttribPoke = poke
  , vertexAttribFormat = [(Position, 3, GL_FLOAT, fromIntegral GL_FALSE, 0)]
  }

normal :: VertexAttrib (V3 Float)
normal =
  VertexAttrib
  { vertexAttribSize = fromIntegral (sizeOf (0 :: V3 Float))
  , vertexAttribPoke = poke
  , vertexAttribFormat = [(Normal, 3, GL_FLOAT, fromIntegral GL_FALSE, 0)]
  }

uv :: GLuint -> VertexAttrib (V2 Float)
uv channel =
  VertexAttrib
  { vertexAttribSize = fromIntegral (sizeOf (0 :: V2 Float))
  , vertexAttribPoke = poke
  , vertexAttribFormat = [(UV channel, 2, GL_FLOAT, fromIntegral GL_FALSE, 0)]
  }

color :: VertexAttrib (V4 Int8)
color =
  VertexAttrib
  { vertexAttribSize = fromIntegral (sizeOf (0 :: V2 Float))
  , vertexAttribPoke = poke
  , vertexAttribFormat = [(Color, 4, GL_UNSIGNED_BYTE, fromIntegral GL_FALSE, 0)]
  }

uploadTriangles
  :: Ord v
  => VertexAttrib v -> [[v]] -> IO VertexArrayObject
uploadTriangles attribs triangles = do
  let sizeInBytes = fromIntegral (vertexAttribSize attribs) * length vertices
  buffer <- mallocBytes sizeInBytes
  sequence_
    (zipWith
       (\v offset ->
          vertexAttribPoke attribs (buffer `plusPtr` fromIntegral offset) v)
       vertices
       (iterate (+ vertexAttribSize attribs) 0))
  vbo <- create glCreateBuffers
  glNamedBufferData vbo (fromIntegral sizeInBytes) buffer GL_STATIC_DRAW
  vao <- create glCreateVertexArrays
  let bindingIndex = 0
  glVertexArrayVertexBuffer
    vao
    bindingIndex
    vbo
    0
    (fromIntegral (vertexAttribSize attribs))
  for_
    (vertexAttribFormat attribs)
    (\(attribTy, components, compTy, normalized, offset) -> do
       let attribIndex =
             case attribTy of
               Position -> attribPosition
               Normal -> attribNormal
               UV n -> attribUV n
       glEnableVertexArrayAttrib vao attribIndex
       glVertexArrayAttribBinding vao attribIndex bindingIndex
       glVertexArrayAttribFormat
         vao
         attribIndex
         components
         compTy
         (fromIntegral normalized)
         offset)
  ebo <- create glCreateBuffers
  withArray
    indices
    (\ptr ->
       glNamedBufferData
         ebo
         (fromIntegral (length indices * sizeOf (0 :: GLuint)))
         ptr
         GL_STATIC_DRAW)
  glVertexArrayElementBuffer vao ebo
  pure (VertexArrayObject vao)
  where
    (indices, vertices) =
      let (indices, (_, vertices)) =
            runState (traverse dedupVertex dat) (mempty, mempty)
      in (indices, V.toList vertices)
    dedupVertex
      :: Ord v
      => v -> State (Map.Map v GLuint, V.Vector v) GLuint
    dedupVertex v = do
      vertices <- gets fst
      case Map.lookup v vertices of
        Just index -> pure index
        Nothing -> do
          let i = fromIntegral (Map.size vertices)
          modify (Map.insert v i *** flip V.snoc v)
          pure i
    dat = [v | tri <- triangles, v <- tri]

newtype ElementBufferObject = ElementBufferObject { eboName :: GLuint }
  deriving (Eq, Ord)

uploadIndices
  :: [Int32] -> IO ElementBufferObject
uploadIndices indices = do
  ebo <- create glCreateBuffers
  withArray
    indices
    (\ptr ->
       glNamedBufferData
         ebo
         (fromIntegral (length indices * sizeOf (0 :: GLuint)))
         ptr
         GL_STATIC_DRAW)
  return $ ElementBufferObject ebo


uploadTriangleSoup :: VertexAttrib v -> [v] -> IO VertexArrayObject
uploadTriangleSoup attribs vertices = do
  let sizeInBytes = fromIntegral (vertexAttribSize attribs) * length vertices
  buffer <- mallocBytes sizeInBytes
  sequence_
    (zipWith
       (\v offset ->
          vertexAttribPoke attribs (buffer `plusPtr` fromIntegral offset) v)
       vertices
       (iterate (+ vertexAttribSize attribs) 0))
  vbo <- create glCreateBuffers
  glNamedBufferData vbo (fromIntegral sizeInBytes) buffer GL_STATIC_DRAW
  vao <- create glCreateVertexArrays
  let bindingIndex = 0
  glVertexArrayVertexBuffer
    vao
    bindingIndex
    vbo
    0
    (fromIntegral (vertexAttribSize attribs))
  for_
    (vertexAttribFormat attribs)
    (\(attribTy, components, compTy, normalized, offset) -> do
       let attribIndex =
             case attribTy of
               Position -> attribPosition
               Normal -> attribNormal
               UV n -> attribUV n
               Color -> attribColor
       glEnableVertexArrayAttrib vao attribIndex
       glVertexArrayAttribBinding vao attribIndex bindingIndex
       glVertexArrayAttribFormat
         vao
         attribIndex
         components
         compTy
         (fromIntegral normalized)
         offset)
  pure (VertexArrayObject vao)

attribIndex attribTy =
  case attribTy of
    Position -> attribPosition
    Normal -> attribNormal
    UV n -> attribUV n
