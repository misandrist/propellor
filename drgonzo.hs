{-# Language OverloadedStrings #-}

-- Installation for DrGonzo, my new laptop from Lara.

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network
--import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Cron as Cron
import Propellor.Property.Scheduled
--import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
--import qualified Propellor.Property.Hostname as Hostname
--import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.Docker as Docker

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[drgonzo]

-- An example host.
drgonzo :: Host
drgonzo = host "DrGonzo"
	& os (System (Buntish "Wily") "amd64")
	& Apt.unattendedUpgrades
	& Apt.addPpa "ppa:justinbogner/haskell-platform"
	& Apt.addPpa "ppa:zfs-native/stable"
	& Apt.update
	& Apt.upgrade
	& Apt.removed removedPkgs
	& Apt.installed installedPkgs

removedPkgs :: [String]
removedPkgs = ["firefox"]

installedPkgs :: [String]
installedPkgs = concat [systools, zfstools, browsing, development, crypto, texlive]

systools :: [String]
systools = ["zsh", "emacs24", "most"]

zfstools :: [String]
zfstools = ["zfs-dkms", "zfs-doc", "zfs-initramfs", "zfsutils-linux", "zfsnap", "zfs-zed"]

browsing :: [String]
browsing = ["chromium-browser"]

development :: [String]
development = ["build-essential", "zlib1g-dev"]

crypto :: [String]
crypto = ["ssh", "gpg", "gnutls-bin", "gnutls-doc"]

texlive :: [String]
texlive = [
	"texlive",
	"texlive-bibtex-extra",
	"texlive-extra-utils",
	"texlive-font-utils",
	"texlive-fonts-extra-doc",
	"texlive-fonts-recommended",
	"texlive-fonts-recommended-doc",
	"texlive-generic-recommended",
	"texlive-humanities-doc",
	"texlive-latex-base",
	"texlive-latex-base-doc",
	"texlive-latex-extra",
	"texlive-latex-extra-doc",
	"texlive-latex-recommended",
	"texlive-latex-recommended-doc",
	"texlive-math-extra",
	"texlive-metapost-doc",
	"texlive-pictures",
	"texlive-pictures-doc",
	"texlive-pstricks",
	"texlive-pstricks-doc",
	"texlive-publishers-doc",
	"texlive-science-doc",
	"context",
	"texlive-fonts-extra",
	"texlive-formats-extra",
	"texlive-games",
	"texlive-generic-extra",
	"texlive-humanities",
	"texlive-lang-arabic",
	"texlive-lang-cjk",
	"texlive-lang-english",
	"texlive-lang-european",
	"texlive-lang-other",
	"texlive-luatex",
	"texlive-metapost",
	"texlive-music",
	"texlive-omega",
	"texlive-plain-extra",
	"texlive-publishers",
	"texlive-science",
	"texlive-xetex"]
