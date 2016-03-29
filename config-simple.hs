-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Apt.PPA as PPA
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Gpg as Gpg

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[ mybox
	, buntishbox
	]

-- An example host.
mybox :: Host
mybox = host "mybox.example.com" $ props
	& osDebian Unstable "amd64"
	& Apt.stdSourcesList
	& Apt.unattendedUpgrades
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh"]
	& User.hasSomePassword (User "root")
	& File.dirExists "/var/www"
	& Cron.runPropellor (Cron.Times "30 * * * *")

buntishbox :: Host
buntishbox = host "buntish.example.com" $ props
	& osBuntish "Wily" "amd64"
	& Apt.unattendedUpgrades
	& Gpg.installed
	& PPA.addPpa ghc710ppa
	& PPA.addPpa zfsPPA
	& PPA.addKeyId xamarinAptKeyId
	& PPA.addRepository xamarinAptRepository
	& Apt.update
	& Apt.upgrade
	& Apt.installed xamarinTools

-- | PPA for GHC 7.10.3
ghc710ppa :: PPA.PPA
ghc710ppa = PPA.PPA "jtgeibel" "ghc-7.10.3"

-- | PPA for ZFS on Linux
zfsPPA :: PPA.PPA
zfsPPA = PPA.PPA "zfs-native" "stable"

-- | Xamarin's GPG key they sign their releases with.
xamarinAptKeyId :: PPA.AptKeyId
xamarinAptKeyId = PPA.AptKeyId "Xamarin Mono" "3FA7E032" "keyserver.ubuntu.com"

-- | Xamarin's repository all the monodevelop packages are hosted in.
xamarinAptRepository :: PPA.AptRepository
xamarinAptRepository =
	PPA.AptRepositorySource $
		PPA.AptSource "http://download.mono-project.com/repo/debian" "wheezy" ["main"]

-- | Xamarin's Mono development tools for Linux.
xamarinTools :: [String]
xamarinTools = [
	"mono-complete", "fsharp", "monodevelop", "exuberant-ctags",
	"mono-vbnc", "mono-xsp4", "monodevelop-database",
	"monodevelop-nunit", "monodevelop-versioncontrol",
	"monodoc-browser"]
