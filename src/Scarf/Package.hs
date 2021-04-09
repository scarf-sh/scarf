{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
nixyAnomicPackageNameType = anomicNameType . fromJust $ UUID.fromString "1e9ff42b-05b5-4a6c-92e6-30181773bbe7"

data NixpkgsResolvedName = NixpkgsResolvedName
  { rev :: Maybe Text,
    pkg :: Text
  }

instance ResolvedName NixpkgsResolvedName where
  makeAnomic (NixpkgsResolvedName {..}) ant = pure $ case eqAnomicNameType ant nixyAnomicPackageNameType of
    Just HRefl -> Just $ FromNixpkgs rev pkg
    Nothing -> Nothing

  acquireHandle _ _ = pure Nothing

nixpkgsPkgset :: Params -> Namespace
nixpkgsPkgset (Params params) =
  if params' /= params
    then error "unknown param"
    else
      Namespace
        { resolveInNs = resolve
        }
  where
    resolve (NameId nm) _mObs = pure . Just . SomeResolvedName $ NixpkgsResolvedName {rev = rev, pkg = nm}

    -- TODO Should we have some short/long fanciness here e.g. recognizing "rev"
    knownParams = Set.toMap $ Set.singleton "revision"

    params' = Map.intersection params knownParams

    -- TODO we should propagate this error somehow
    rev :: Maybe Text
    rev = Map.lookup "revision" params'

scarfPkgset :: Params -> Namespace
scarfPkgset params =
  if params /= emptyParams
    then error "unknown param"
    else
      Namespace
        { resolveInNs = resolve
        }
  where
    nixpkgsNixPath = nixpkgsPkgset emptyParams
    resolve nm mObs = do
      maybeObserve
        mObs
        ( ReductionMessage
            { rm_ns = nixpkgsNixPath,
              rm_nsid = "nixpkgs",
              rm_nm = nm
            }
        )
      resolveInNs nixpkgsNixPath nm mObs
