-- | Types for managing SSL certificates.
--
-- Maintainer: Evan Cofsky <evan@theunixman.com>

module Propellor.Types.PKI (SSLType(..), DHBits, fileName) where

import System.FilePath

type DHBits = Int

data SSLType
	= CRL -- ^ Certificate revocation list
	| Chain -- ^ CA Chain
	| Certificate -- ^ Certificate
	| RSAKey -- ^ Private key
	| DHParams DHBits -- ^ DH Params

-- | File extensions for different SSLType types.
fileExt :: SSLType -> String
fileExt CRL = "crl"
fileExt Chain = "ca-chain"
fileExt Certificate = "cert"
fileExt RSAKey = "key"
fileExt (DHParams s) = "dh" ++ (show s)

-- | Add the .pem to the end.
pemName :: SSLType -> FilePath
pemName t = fileExt t <.> "pem"

-- | Create the pem file name for a given base name.
fileName :: String -> SSLType -> FilePath
fileName name a = do
	case a of
          DHParams _ -> pemName a -- ^ DH Parameters don't have other names
          _ -> name <.> pemName a -- ^ All others are {name}.{ext}.pem
