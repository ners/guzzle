module Template
    ( Template
    , parse
    , render
    , expand
    , extension
    , unique
    )
where

import Data.Char (isControl)
import Data.Text qualified as Text
import System.FilePath (dropExtension, takeExtension)
import Prelude

data Part
    = Literal String
    | Placeholder Char
    deriving stock (Eq, Show)

newtype Template = Template [Part]
    deriving stock (Eq, Show)

parse :: String -> Either Text Template
parse = fmap Template . parts
  where
    parts [] = Right []
    parts "%" = Left "template ends with a lone %"
    parts ('%' : '%' : rest) = (Literal "%" :) <$> parts rest
    parts ('%' : c : rest)
        | elem @[] c "nkoapIixywhd" = (Placeholder c :) <$> parts rest
        | otherwise = Left $ "unknown placeholder %" <> Text.singleton c
    parts s = (Literal literal :) <$> parts rest
      where
        (literal, rest) = break (== '%') s

render :: (Char -> Maybe String) -> Template -> FilePath
render = substitute sanitise
  where
    sanitise =
        take 64
            . dropWhile (== '.')
            . fmap \c -> if c == '/' || isControl c then '_' else c

expand :: (Char -> Maybe String) -> Template -> String
expand = substitute $ fmap \c -> if isControl c then ' ' else c

substitute
    :: (String -> String) -> (Char -> Maybe String) -> Template -> String
substitute clean value (Template template) = foldMap part template
  where
    part (Literal s) = s
    part (Placeholder c) = maybe "" clean (value c)

extension :: Template -> Maybe String
extension (Template template) =
    case takeExtension . foldMap literal . trailingLiterals $ template of
        "" -> Nothing
        ext -> Just ext
  where
    trailingLiterals = reverse . takeWhile isLiteral . reverse
    isLiteral Literal{} = True
    isLiteral Placeholder{} = False
    literal (Literal s) = s
    literal Placeholder{} = ""

unique :: [FilePath] -> [FilePath]
unique paths = zipWith numbered [1 :: Int ..] paths
  where
    numbered i path
        | length (filter (== path) paths) > 1 =
            dropExtension path <> "-" <> show i <> takeExtension path
        | otherwise = path
