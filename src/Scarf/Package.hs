{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Scarf.Package where

import Data.Aeson
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Maybe
import Data.Text
import Data.UUID as UUID
import Nomia.Name
import Nomia.Namespace
import Type.Reflection

defaultPackageNs :: NamespaceId
defaultPackageNs = PrimitiveNamespace emptyParams "scarf-pkgset"

-- Pull from <nixpkgs>
-- Pull from specific nixpkgs checkout
-- TODO Should really be rev and ref, we're conflating, YOLO for now
-- TODO more complex nix expressions?
-- TODO Hard-coded nix path?
data NixyAnomicPackageName
  = FromNixpkgs (Maybe Text) Text -- maybe rev, attr name, TODO Docstrings etc.

-- TODO How much work here vs in nix expressions
nixyAnomicPackageNameToJSON :: NixyAnomicPackageName -> Value
nixyAnomicPackageNameToJSON (FromNixpkgs Nothing nm) = String nm
nixyAnomicPackageNameToJSON (FromNixpkgs (Just rev) nm) =
  Object $
    Map.fromList
      [ ("rev", String rev),
        ("name", String nm)
      ]

nixyAnomicPackageNameType :: AnomicNameType NixyAnomicPackageName
nixyAnomicPackageNameType = AnomicNameType . fromJust $ UUID.fromString "1e9ff42b-05b5-4a6c-92e6-30181773bbe7"

scarfPkgset :: Params -> Namespace
scarfPkgset (Params params) =
  Namespace
    { makeAnomicInNs = mkAnomic
    }
  where
    mkAnomic :: Typeable a => Text -> AnomicNameType a -> Maybe a
    mkAnomic nm ant = case eqAnomicNameType ant nixyAnomicPackageNameType of
      Just HRefl -> Just $ FromNixpkgs rev nm
      Nothing -> Nothing

    -- TODO Should we have some short/long fancinies here e.g. recognizing "rev"
    knownParams = Set.toMap $ Set.singleton "revision"

    params' = Map.intersection params knownParams

    -- TODO we should propagate this error somehow
    rev :: Maybe Text
    rev = if params' /= params then error "unknown param" else Map.lookup "revision" params'
