{-# LANGUAGE OverloadedStrings #-}

module Nomia.Name where

import Data.Text as Text

-- bash
-- Scarf:bash
-- Nix:bash
-- (local-service?foo=True:run-scarf-server-namespace):postgres
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
  deriving (Eq, Ord)

data NamespaceId
  = PrimitiveNamespace Text -- TODO is "primitive" right here?
  | NameNamespace Name
  deriving (Eq, Ord)

-- TODO Make a proper parser
parseName ::
  NamespaceId ->
  Text ->
  Maybe Name
parseName defaultNamespace input = case Text.splitOn ":" input of
  [name] -> Just $ AtomicName defaultNamespace name
  [namespace, name] -> Just $ AtomicName (PrimitiveNamespace namespace) name
  _ -> Nothing

printNsid :: NamespaceId -> Text
-- TODO colons in prim
printNsid (PrimitiveNamespace prim) = prim
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
