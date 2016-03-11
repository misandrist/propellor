-- | Navipoint system configuration settings.

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.OpenVPN as OVPN

main :: IO ()
main = defaultMain hosts

hosts :: [Host]
hosts = [navipoint_vpn]

navipoint_vpn :: Host
navipoint_vpn = host "52.70.117.175"
	& os (System (Buntish "trusty") "amd64")
	& Apt.unattendedUpgrades
	& Apt.update
	& Apt.upgrade
	& OVPN.openvpn "navipoint" "10.241.0.0" "255.255.254.0" [] False 2048
