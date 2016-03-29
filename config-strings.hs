{-# Language OverloadedStrings #-}

-- Installation for a Buntish box demonstrating OverloadedStrings for some
-- configuration.

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.AptSoftwarePropertiesCommon as SWPC
import qualified Propellor.Property.Gpg as Gpg

--import qualified Propellor.Property.Ssh as Ssh
--import qualified Propellor.Property.Sudo as Sudo
--import qualified Propellor.Property.Hostname as Hostname
--import qualified Propellor.Property.Tor as Tor

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[buntishbox]

buntishbox :: Host
buntishbox = host "buntish.example.com"
	& os (System (Buntish "wily") "amd64")
	& Apt.unattendedUpgrades
	& Gpg.installed
	& SWPC.addPpa "ppa:jtgeibel/ghc-7.10.3"
	& SWPC.addPpa "ppa:zfs-native/stable"
	& SWPC.addKeyId xamarinAptKeyId
	& SWPC.addRepository xamarinAptRepository
	& Apt.update
	& Apt.upgrade
	& Apt.installed xamarinTools

-- | Xamarin's GPG key they sign their releases with.
xamarinAptKeyId :: SWPC.AptKeyId
xamarinAptKeyId = SWPC.AptKeyId "Xamarin Mono" "3FA7E032" "keyserver.ubuntu.com"

-- | Xamarin's repository all the monodevelop packages are hosted in.
xamarinAptRepository :: SWPC.AptRepository
xamarinAptRepository =
	SWPC.AptRepositorySource
		"deb http://download.mono-project.com/repo/debian wheezy main"

-- | Xamarin's Mono development tools for Linux.
xamarinTools :: [String]
xamarinTools = [
	"mono-complete", "fsharp", "monodevelop", "exuberant-ctags",
	"mono-vbnc", "mono-xsp4", "monodevelop-database",
	"monodevelop-nunit", "monodevelop-versioncontrol",
	"monodoc-browser"]
