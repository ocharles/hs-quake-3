{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Quake3.Shader.Parser where

import Control.Monad
import Data.Char
import Text.Megaparsec hiding (token)
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.Text

--------------------------------------------------------------------------------
-- | Parse a complete @.shader@ file, up to EOF.
parseShaderFile :: Parser [Shader]
parseShaderFile = spaceConsumer *> many parseShader <* eof


--------------------------------------------------------------------------------
-- | The top level definition of a shader.
data Shader = Shader
  { shaderName :: String
    -- ^ The name of this shader
  , shaderInstructions :: [Instruction]
    -- ^ The individual instructions of a shader.
  } deriving (Show)

parseShader :: Parser Shader
parseShader = do
  shaderName <-
    L.lexeme spaceConsumer (anyChar `someTill` eol) <?> "shader name"
  shaderInstructions <-
    label "shader instructions" $
    between
      (L.symbol spaceConsumer "{")
      (L.symbol spaceConsumer "}")
      (parseInstruction `sepEndBy1` L.lexeme spaceConsumer eol)
  return Shader {..}


--------------------------------------------------------------------------------
-- | Top-level instructions inside a shader.
data Instruction
  = ShaderOp Op
    -- ^ A global parameter, set independent of individual passes.
  | ShaderPass [Op]
    -- ^ A single pass/stage in this shader.
  deriving (Show)

parseInstruction :: Parser Instruction
parseInstruction =
  ShaderOp <$> parseOp <|> ShaderPass <$> pass <?> "shader instruction"
  where
    pass =
      label "pass" $
      between
        (L.symbol spaceConsumer "{")
        (L.symbol hspaceConsumer "}")
        (parseOp `sepEndBy1` L.lexeme spaceConsumer eol)


--------------------------------------------------------------------------------
-- | A single operation, consisting of a function name and a list of arguments
data Op =
  Op String [String]
  deriving (Show)

parseOp :: Parser Op
parseOp =
  Op <$> L.lexeme hspaceConsumer (some argumentChar) <*>
  many (L.lexeme hspaceConsumer (some argumentChar))


--------------------------------------------------------------------------------
-- For tokenization

-- | Consume all whitespace and comments.
spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) (L.skipLineComment "//") mzero

-- | Consume all whitespace and comments, excluding newlines.
hspaceConsumer :: Parser ()
hspaceConsumer = L.space (void horizSpace) (L.skipLineComment "//") mzero

-- | Parse horizontal whitespace.
horizSpace :: Parser Char
horizSpace = char ' ' <|> char '\t' <|> char '(' <|> char ')'

-- | Characters that can appear in 'Op'.
argumentChar :: Parser Char
argumentChar =
  fmap
    Data.Char.toLower
    (alphaNumChar <|> char '_' <|> char '$' <|> char '/' <|> char '.' <|>
     char '-' <|> char '*') <?> "argument character"
