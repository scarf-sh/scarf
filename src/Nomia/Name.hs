{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nomia.Name
  ( Name (..),
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
import Data.Text as Text
import Development.Placeholders
import qualified Text.Megaparsec as MP

-- bash
-- Scarf:bash
-- Nix:bash
-- (local-service?foo=True:run-scarf-server-namespace):postgres
--
-- Valid identifiers? escapes? types for params? Full grammar
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

-- TODO Compositions
data Name
  = AtomicName NamespaceId Text
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

printNsid :: NamespaceId -> Text
-- TODO colons in prim
printNsid (PrimitiveNamespace params ident) = if params == emptyParams then ident else $notImplemented
printNsid (NameNamespace nm) =
  Text.concat
    [ "(",
      printName Nothing nm,
      ")"
    ]

printName ::
  Maybe NamespaceId ->
  Name ->
  Text
printName defaultNs (AtomicName nsid nm)
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
        pure (AtomicName defaultNamespace name)

      -- Example: nix:bash, scarf-pkgset:bash
      parseNamespacedName :: Parser Name
      parseNamespacedName = do
        namespaceId <- MP.try $ do
          namespaceId <- parseNamespaceId
          _ <- MP.single ':'
          pure namespaceId
        name <- identifier
        pure (AtomicName namespaceId name)
   in case MP.runParser (parseNames <* MP.eof) inputSource (Text.strip input) of
        Left errorBundle -> Left (MP.bundleErrors errorBundle)
        Right name -> Right name
