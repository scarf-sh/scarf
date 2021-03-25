{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Scarf.Package where

import Data.Maybe
import Data.Text
import Data.UUID as UUID
import Nomia.Name
import Nomia.Namespace
import Type.Reflection

defaultPackageNs :: NamespaceId
defaultPackageNs = PrimitiveNamespace "scarf-pkgset"

data NixyAnomicPackageName
  = FromNixpkgs Text

nixyAnomicPackageNameType :: AnomicNameType NixyAnomicPackageName
nixyAnomicPackageNameType = AnomicNameType . fromJust $ UUID.fromString "1e9ff42b-05b5-4a6c-92e6-30181773bbe7"

scarfPkgset :: Namespace
scarfPkgset =
  Namespace
    { makeAnomicInNs = mkAnomic
    }
  where
    mkAnomic :: Typeable a => Text -> AnomicNameType a -> Maybe a
    mkAnomic nm ant = case eqAnomicNameType ant nixyAnomicPackageNameType of
      Just HRefl -> Just $ FromNixpkgs nm
      Nothing -> Nothing
