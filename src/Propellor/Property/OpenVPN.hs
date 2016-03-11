{-# Language OverloadedStrings #-}

-- | Maintainer: Evan Cofsky <evan@theunixman.com>

module Propellor.Property.OpenVPN where

import Data.List (intercalate)

import Propellor.Base
import Utility.FileMode
import System.Posix.Files

import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

type VPNInstance = String
type ConfigFile = FilePath

openvpn :: VPNInstance -> String -> String -> [String] -> Bool -> Int -> CombinedType (Property NoInfo) (Property HasInfo)
openvpn inst addrBase addrMask dnsServers redirectGateway dhbits =
	latestInstalled
	`before` combineProperties ("creating config dirs for " ++ inst) [configDirExists inst, certsDirExists inst]
	`before` genDhParams inst dhbits
	`before` installPems inst
	`before` installConfig inst addrBase addrMask dnsServers redirectGateway dhbits
	`before` Service.restarted "openvpn"

latestInstalled :: CombinedType (Property HasInfo) (Property NoInfo)
latestInstalled = installOVPNGpgKey `before` installOVPNRepo `before` Apt.update `before` Apt.installed ["openvpn"]

installOVPNGpgKey :: Property HasInfo
installOVPNGpgKey =
	withPrivData GpgKey (Context "openvpn.repository") $
		\getkey -> property "Trust OpenVPN repository GPG key" $
			getkey $ \key -> aptKey $ privDataLines key
  where
	aptKey key =
		let
			writer h = hPutStr h (unlines key)
		in
			liftIO $ writeReadProcessEnv "apt-key" ["add", "-"] Nothing (Just writer) Nothing >> return MadeChange

installOVPNRepo :: Property NoInfo
installOVPNRepo =
	Apt.setSourcesListD ["deb http://swupdate.openvpn.net/apt trusty main"] "openvpn"

configDirExists :: VPNInstance -> Property NoInfo
configDirExists = dirExists . instConfigDir

certsDirExists :: VPNInstance -> Property NoInfo
certsDirExists = dirExists . instPkiDir

dirExists :: FilePath -> Property NoInfo
dirExists d =
	File.dirExists d
	`before` File.mode d (combineModes [ownerReadMode, ownerWriteMode, ownerExecuteMode])

-- | Root of the OpenVPN configuration
--
-- This is where the instance openvpn config file will go.
openVPNConfDir :: FilePath
openVPNConfDir = "/etc/openvpn"

confFilePath :: VPNInstance -> FilePath
confFilePath inst = openVPNConfDir </> inst <.> "conf"

-- | Absolute path to the configuration for this instance
instConfigDir :: VPNInstance -> FilePath
instConfigDir inst = openVPNConfDir </> inst

-- | Absolute path to the instance PKI directory
instPkiDir :: VPNInstance -> FilePath
instPkiDir inst = instConfigDir inst </> "pki"

pkiFileName :: VPNInstance -> SSLType -> FilePath
pkiFileName inst typ = instPkiDir inst </> fileName inst typ

-- | Generates the DH Parameters
genDhParams :: VPNInstance -> DHBits -> Property NoInfo
genDhParams inst bits =
	let
		dhpem = pkiFileName inst (DHParams bits)
		gendh = cmdProperty "openssl" ["dhparam", "-out", dhpem, show bits]
	in
		check (not <$> doesFileExist dhpem) gendh

installPems :: VPNInstance -> Property HasInfo
installPems inst =
  combineProperties ("Install PKI PEMS for " ++ inst) $ map (installPem inst) [CRL, Chain, Certificate, RSAKey]

-- | Install a pem file from private data
--
-- The context should be "openvpn.<instance name>"
installPem :: VPNInstance -> SSLType -> Property HasInfo
installPem inst typ =
	let
		privCtx = Context $ intercalate "." ["openvpn", inst]
	in
		File.hasPrivContent (pkiFileName inst typ) privCtx

installConfig :: VPNInstance -> String -> String -> [String] -> Bool -> Int -> Property NoInfo
installConfig inst addrBase addrMask dnsServers redirectGateway dhbits =
	let
		config = genConfig inst addrBase addrMask dnsServers redirectGateway dhbits
	in
		File.hasContent (confFilePath inst) config

genConfig :: VPNInstance -> String -> String -> [String] -> Bool ->Int -> [String]
genConfig inst addrBase addrMask dnsServers redirectGateway dhbits =
	let
		confItem :: String -> String -> [String]
		confItem key val = [intercalate " " [key, val]]

		qs :: String -> String
		qs s = intercalate "" ["\"", s, "\""]

		push vals = confItem "push" $ qs (intercalate " " vals)

		serverOpt = confItem "server" (intercalate " " [addrBase, addrMask])
		pemOpts = concatMap (\(k, t) -> confItem k (pkiFileName inst t)) $
			zip ["ca", "key", "cert", "crl-verify"] [Chain, RSAKey, Certificate, CRL]
		dnsOpts = concatMap (\s -> push ["dhcp-option", "DNS", s]) dnsServers
		redirectOpt = if redirectGateway then push ["redirect-gateway"] else []
		dhOpt = confItem "dh" (pkiFileName inst (DHParams dhbits))
	in
		concat [templateConfig, serverOpt, pemOpts, dnsOpts, redirectOpt, dhOpt]

templateConfig :: [String]
templateConfig = [
	"# Network options",
	"",
	"port 1194",
	"proto udp",
	"dev tun",
	"",
	"# Server options",
	"topology subnet",
	"ifconfig-pool-persist ipp.txt",
	"keepalive 10 120",
	"comp-lzo",
	"",
	"# Push to client",
	"",
	"push \"comp-lzo yes\"",
	"# The persist options will try to avoid",
	"# accessing certain resources on restart",
	"# that may no longer be accessible because",
	"# of the privilege downgrade.",
	"persist-key",
	"persist-tun",
	"",
	"# Set the appropriate level of log",
	"# file verbosity.",
	"#",
	"# 0 is silent, except for fatal errors",
	"# 4 is reasonable for general usage",
	"# 5 and 6 can help to debug connection problems",
	"# 9 is extremely verbose",
	"verb 3",
	"",
	"## TLS Options",
	"# remote-cert-tls client",
	"remote-cert-ku e0 88 80 08",
	""]
