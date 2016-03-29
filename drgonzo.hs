{-# Language OverloadedStrings #-}

-- Installation for DrGonzo, my new laptop from Lara.

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Apt.PPA as PPA
import qualified Propellor.Property.Gpg as Gpg

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[drgonzo]

-- An example host.
drgonzo :: Host
drgonzo = host "DrGonzo" $ props
	& osBuntish "Wily" "amd64"
	& Apt.unattendedUpgrades
	& PPA.addPpa "ppa:jtgeibel/ghc-7.10.3"
	& PPA.addPpa "ppa:zfs-native/stable"
	& PPA.addKeyId xamarinAptKeyId
	& PPA.addRepository xamarinAptRepository
	& PPA.addKeyId spotifyAptKeyId
	& PPA.addRepository spotifyAptRepository
	& Apt.update
	& Apt.upgrade
	& Gpg.installed
	& Apt.removed removedPkgs
	& Apt.installed installedPkgs

removedPkgs :: [String]
removedPkgs = ["firefox"]

installedPkgs :: [String]
installedPkgs = concat [systools, zfstools, browsing, development, crypto, texlive, virtualization, kde, spotify]

systools :: [String]
systools = ["zsh", "emacs24", "most", "btrfs-tools"]

virtualization :: [String]
virtualization = ["lxd", "lxd-client", "lxd-tools", "debootstrap", "virtualbox", "virtualbox-dkms", "virtualbox-qt"]

zfstools :: [String]
zfstools = ["spl-dkms", "zfs-dkms", "zfsutils"]

browsing :: [String]
browsing = ["chromium-browser"]

development :: [String]
development = ["build-essential", "zlib1g-dev", "virtualenv",
	       "mono-complete", "fsharp", "monodevelop", "exuberant-ctags",
	       "mono-vbnc", "mono-xsp4", "monodevelop-database",
	       "monodevelop-nunit", "monodevelop-versioncontrol",
	       "monodoc-browser"]

crypto :: [String]
crypto = ["ssh", "gnutls-bin", "gnutls-doc"]

kde :: [String]
kde = ["akonadi-backend-postgresql"]

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

spotifyAptKeyId :: PPA.AptKeyId
spotifyAptKeyId =
	PPA.AptKeyId "Spotify" "BBEBDCB318AD50EC6865090613B00F1FD2C19886" "hkp://keyserver.ubuntu.com:80"

spotifyAptRepository :: PPA.AptRepository
spotifyAptRepository =
	PPA.AptRepositorySource
		"deb http://repository.spotify.com stable non-free"

spotify :: [String]
spotify = ["spotify-client"]

xamarinAptKeyId :: PPA.AptKeyId
xamarinAptKeyId = PPA.AptKeyId "Xamarin Mono" "3FA7E032" "keyserver.ubuntu.com"

xamarinAptRepository :: PPA.AptRepository
xamarinAptRepository =
	PPA.AptRepositorySource
		"deb http://download.mono-project.com/repo/debian wheezy main"
