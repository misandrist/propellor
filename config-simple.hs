-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.AptSoftwarePropertiesCommon as SWPC
import qualified Propellor.Property.Gpg as Gpg
import qualified Propellor.Property.Network as Network
--import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.User as User

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[ mybox,
	  buntishbox
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

-- A generic webserver in a Docker container.
webserverContainer :: Docker.Container
webserverContainer = Docker.container "webserver" (Docker.latestImage "debian")
	& os (System (Debian (Stable "jessie")) "amd64")
	& Apt.stdSourcesList
	& Docker.publish "80:80"
	& Docker.volume "/var/www:/var/www"
	& Apt.serviceInstalledRunning "apache2"

buntishbox :: Host
buntishbox = host "buntish.example.com"
	& os (System (Buntish "wily") "amd64")
	& Apt.unattendedUpgrades
	& Gpg.installed
	& SWPC.addPpa ghc710ppa
	& SWPC.addPpa zfsPPA
	& SWPC.addKeyId xamarinAptKeyId
	& SWPC.addRepository xamarinAptRepository
	& Apt.update
	& Apt.upgrade
	& Apt.installed xamarinTools

-- | PPA for GHC 7.10.3
ghc710ppa :: SWPC.PPA
ghc710ppa = SWPC.PPA "jtgeibel" "ghc-7.10.3"

-- | PPA for ZFS on Linux
zfsPPA :: SWPC.PPA
zfsPPA = SWPC.PPA "zfs-native" "stable"

-- | Xamarin's GPG key they sign their releases with.
xamarinAptKeyId :: SWPC.AptKeyId
xamarinAptKeyId = SWPC.AptKeyId "Xamarin Mono" "3FA7E032" "keyserver.ubuntu.com"

-- | Xamarin's repository all the monodevelop packages are hosted in.
xamarinAptRepository :: SWPC.AptRepository
xamarinAptRepository =
	SWPC.AptRepositorySource $
		SWPC.AptSource "http://download.mono-project.com/repo/debian" "wheezy" ["main"]

-- | Xamarin's Mono development tools for Linux.
xamarinTools :: [String]
xamarinTools = [
	"mono-complete", "fsharp", "monodevelop", "exuberant-ctags",
	"mono-vbnc", "mono-xsp4", "monodevelop-database",
	"monodevelop-nunit", "monodevelop-versioncontrol",
	"monodoc-browser"]
