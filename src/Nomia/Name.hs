{-# LANGUAGE OverloadedStrings #-}

module Nomia.Name where

import Control.Monad
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    withText,
  )
import Data.Text as Text

-- bash
-- Scarf:bash
-- Nix:bash
-- (local-service?foo=True:run-scarf-server-namespace):postgres
--
-- TODO: Allow more structured names that are not representable as strings
--
-- One idea to represent composition in printable names is to use function
-- application possibly with dot-accessors for specific outputs.
--
-- Maybe some kind of local binding for complex compositions? let-bind nodes, make edges, specify the relevant output edge???
--
-- What about notation to specify a specific output?

data Name = Name AtomicName
  deriving (Eq, Ord)

-- TODO Use some kind of builder notion for these print functions?
printName :: Name -> Text
printName (Name anm) = printAtomicName anm

data NamespaceId
  = PrimitiveNamespace Text
  | NameNamespace Name
  deriving (Eq, Ord)

printNsid :: NamespaceId -> Text
-- TODO colons in prim
printNsid (PrimitiveNamespace prim) = prim
printNsid (NameNamespace nm) =
  Text.concat
    [ "(",
      printName nm,
      ")"
    ]

data AtomicName
  = AtomicName NamespaceId Text
  deriving (Eq, Ord)

printAtomicName :: AtomicName -> Text
printAtomicName (AtomicName nsid nm) =
  Text.concat
    [ printNsid nsid,
      ":",
      nm
    ]

-- TODO Make a proper parser
parseName ::
  NamespaceId ->
  Text ->
  Maybe Name
parseName defaultNamespace input = case Text.splitOn ":" input of
  [name] -> Just $ Name (AtomicName defaultNamespace name)
  [namespace, name] -> Just $ Name (AtomicName (PrimitiveNamespace namespace) name)
  _ -> Nothing

-- TODO This shouldn't be here!
defaultPackageNs :: NamespaceId
defaultPackageNs = PrimitiveNamespace "scarf-pkgset"

-- TODO This should support both a JSON-structured repr as well as the string repr,
-- much like we have short flags for humans and long flags for scripts.
instance FromJSON Name where
  parseJSON = withText "Text" (maybe mzero pure . parseName defaultPackageNs)

instance ToJSON Name where
  toJSON = toJSON . printName
