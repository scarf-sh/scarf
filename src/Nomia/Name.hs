{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nomia.Name
  ( NameId (..),
    Name (..),
    NamespaceId (..),
    Params (..),
    emptyParams,
    ParseError,
    parseName,
    printName,
  )
where

import Control.Applicative
import Data.Foldable (asum)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text as Text
import qualified Text.Megaparsec as MP

-- bash
-- Scarf:bash
-- Nix:bash
-- (local-service?foo=True:run-scarf-server-namespace):postgres
--
-- Valid identifiers? escapes (print and parse)? types for params? Full grammar
--
-- TODO: Allow more structured names that are not representable as strings
-- Maybe we want response-file like syntax in the parser, e.g. @foo.nom includes a full name (can be put in composition, in namespace field, etc.)
--
-- One idea to represent composition in printable names is to use function
-- application possibly with dot-accessors for specific outputs.
--
-- Maybe some kind of local binding for complex compositions? let-bind nodes, make edges, specify the relevant output edge???
--
-- What about notation to specify a specific output?

-- TODO Params etc.
newtype NameId = NameId Text deriving (Eq, Ord, Show, IsString)

-- TODO Compositions
data Name
  = AtomicName NamespaceId NameId
  deriving (Eq, Ord, Show)

-- TODO More types for param values,
newtype Params = Params (HashMap Text Text)
  deriving (Eq, Ord, Show)

emptyParams :: Params
emptyParams = Params $ Map.empty

data NamespaceId
  = PrimitiveNamespace -- TODO is "primitive" right here?
      Params
      Text -- identifier, could be somehow typed to specify parameter or types of names it knows about
  | NameNamespace Name
  deriving (Eq, Ord, Show)

printParams :: Params -> [Text]
printParams (Params p)
  | p == Map.empty = []
  | otherwise = "?" : printParams' (Map.toList p)
  where
    printParams' [] = [] -- Should never get here but whateber
    printParams' ((k, v) : []) = [k, "=", v]
    printParams' ((k, v) : tl) = [k, "=", v, "&"] ++ printParams' tl

printNsid :: NamespaceId -> Text
printNsid (PrimitiveNamespace params ident) =
  Text.concat (ident : printParams params)
printNsid (NameNamespace nm) =
  Text.concat
    [ "(",
      printName Nothing nm,
      ")"
    ]

-- TODO roundtripping proptest with parser
printName ::
  Maybe NamespaceId ->
  Name ->
  Text
printName defaultNs (AtomicName nsid (NameId nm))
  | Just nsid == defaultNs = nm
  | otherwise =
    Text.concat
      [ printNsid nsid,
        ":",
        nm
      ]

-- TODO We should have structured representations of names here (e.g. JSON object)
-- This would be like the difference between short command line flags, which humans
-- use, and long ones, which scripts use, for clarity and structure.

type Parser = MP.Parsec () Text

type ParseError = MP.ParseError Text ()

parseName ::
  -- | Default namespace
  NamespaceId ->
  -- | Source of input for parser errors
  String ->
  -- | Input to parse
  Text ->
  Either (NonEmpty ParseError) Name
parseName defaultNamespace inputSource input =
  let identifier :: Parser Text
      identifier =
        MP.takeWhileP (Just "identifier") (`notElem` [':', ' ', '?', '=', '&']) -- TODO separate out idents, param keys, param values?
      parseParams' :: HashMap Text Text -> Parser Params
      parseParams' acc = do
        key <- identifier
        _ <- MP.single '='
        value <- identifier
        let acc' = Map.insert key value acc
        (MP.try (MP.single '&') *> parseParams' acc') <|> pure (Params acc')

      parseParams :: Parser Params
      parseParams =
        (MP.try (MP.single '?') *> parseParams' Map.empty)
          <|> pure emptyParams

      parseNamespaceId :: Parser NamespaceId
      parseNamespaceId = do
        namespaceId <- identifier
        params <- parseParams
        -- TODO parse nested namespaces
        pure (PrimitiveNamespace params namespaceId)

      parseNames :: Parser Name
      parseNames = do
        asum
          [ parseNamespacedName,
            parseSimpleName
          ]

      -- Example: bash
      parseSimpleName :: Parser Name
      parseSimpleName = do
        name <- identifier
        pure (AtomicName defaultNamespace (NameId name))

      -- Example: nix:bash, scarf-pkgset:bash
      parseNamespacedName :: Parser Name
      parseNamespacedName = do
        namespaceId <- MP.try $ do
          namespaceId <- parseNamespaceId
          _ <- MP.single ':'
          pure namespaceId
        name <- identifier
        pure (AtomicName namespaceId (NameId name))
   in case MP.runParser (parseNames <* MP.eof) inputSource (Text.strip input) of
        Left errorBundle -> Left (MP.bundleErrors errorBundle)
        Right name -> Right name
