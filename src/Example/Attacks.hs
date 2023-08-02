{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Attacks where
import Prelude
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types

data Protocol = TCP | UDP | ICMP | ESP | GRE | PIM | IPv6 | OSPF | VRRP | SCTP | NULL
  | IGMP | VISA | GGP | NVPII | IPIP | ST | CBT deriving (Show,Generic,Read,FromJSON,ToJSON,Eq)


data AttackData = AD { name :: String, filt :: Flow -> Bool, markers :: [Marker], minEntropy :: Int }
instance Show AttackData where
  show (AD nm _ _ _) = nm
  
data Marker = Marker { markerEnum :: MarkerEnum, threshold :: Int }
instance Show Marker where
  show (Marker en _) = show en
  
data MarkerEnum = BPS | PPS deriving Show

data Flow = Flow { proto :: Protocol, dA :: String, sA :: String, dP :: String, sP :: String, fA :: Bool, fS :: Bool, fR :: Bool, fF :: Bool, fP :: Bool, fU :: Bool, fX :: Bool}

suffix :: MarkerEnum -> String
suffix BPS = "_ibps" 
suffix PPS = "_ipps" 

tcpSynFloodPpsThreshold = 14030 
tcpSynFloodIpEntropy = 50
tcpSynFloodFilter :: Flow -> Bool
tcpSynFloodFilter flow = proto flow == TCP
                            && fS flow
                            && not (fR flow)
                            && not (fF flow)
                            && not (fA flow)
                            && not (fP flow)
                            && not (fX flow)
                            && not (fU flow)
tcpSynFlood :: AttackData
tcpSynFlood = AD "TCP_SYN_FLOOD" tcpSynFloodFilter [Marker PPS tcpSynFloodPpsThreshold] tcpSynFloodIpEntropy

ipProtoNullPpsThreshold = 2000
ipProtoNullIpEntropy = 0
ipProtoNullFilter :: Flow -> Bool
ipProtoNullFilter flow = proto flow == NULL
ipProtoNull :: AttackData
ipProtoNull = AD "IP_PROTO_NULL" ipProtoNullFilter [Marker PPS ipProtoNullPpsThreshold] ipProtoNullIpEntropy

tcpFlagNullPpsThreshold = 2000
tcpFlagNullIpEntropy = 0
tcpFlagNullFilter :: Flow -> Bool
tcpFlagNullFilter flow = proto flow == TCP
tcpFlagNull :: AttackData
tcpFlagNull = AD "TCP_FLAG_NULL" tcpFlagNullFilter [Marker PPS tcpFlagNullPpsThreshold] tcpFlagNullIpEntropy

malformTcpPpsThreshold = 2000
malformTcpIpEntropy = 5
malformTcpFilter :: Flow -> Bool 
malformTcpFilter flow = proto flow == TCP
                          && (dP flow) == "0"
malformTcp :: AttackData
malformTcp = AD "MALFORM_TCP" malformTcpFilter [Marker PPS malformTcpPpsThreshold] malformTcpIpEntropy

malformUdpPpsThreshold = 2000
malformUdpIpEntropy = 5
malformUdpFilter :: Flow -> Bool
malformUdpFilter flow = proto flow == UDP
                          && dP flow == "0"
malformUdp :: AttackData
malformUdp = AD "MALFORM_UDP" malformUdpFilter [Marker PPS malformUdpPpsThreshold] malformUdpIpEntropy

icmpMisusePpsThreshold = 2000
icmpMisuseIpEntropy = 5
icmpMisuseFilter :: Flow -> Bool
icmpMisuseFilter flow = proto flow == ICMP
icmpMisuse :: AttackData
icmpMisuse = AD "ICMP_MISUSE" icmpMisuseFilter [Marker PPS icmpMisusePpsThreshold] icmpMisuseIpEntropy

tcpRstFloodPpsThreshold = 14030
tcpRstFloodIpEntropy = 10
tcpRstFloodFilter :: Flow -> Bool
tcpRstFloodFilter flow = proto flow == TCP
                            && fR flow
                            && not (fS flow)
                            && not (fF flow)
                            && not (fA flow)
                            && not (fP flow)
                            && not (fX flow)
                            && not (fU flow)
tcpRstFlood :: AttackData
tcpRstFlood = AD "TCP_RST_FLOOD" tcpRstFloodFilter [Marker PPS tcpRstFloodPpsThreshold] tcpRstFloodIpEntropy

udpFloodBpsThreshold = 1000000000
udpFloodIpEntropy = 100
udpFloodFilter :: Flow -> Bool
udpFloodFilter flow = proto flow == UDP
udpFlood :: AttackData
udpFlood = AD "UDP_FLOOD" udpFloodFilter [Marker BPS udpFloodBpsThreshold] udpFloodIpEntropy
                         
hostTcpTrafficBpsThreshold = 206530000
hostTcpTrafficIpEntropy = 100
hostTcpTrafficFilter :: Flow -> Bool
hostTcpTrafficFilter flow = proto flow == TCP
hostTcpTraffic :: AttackData
hostTcpTraffic = AD "HOST_TCP_TRAFFIC" hostTcpTrafficFilter [Marker BPS hostTcpTrafficBpsThreshold] hostTcpTrafficIpEntropy

dnsAmplificationPpsThreshold = 49430
dnsAmplificationBpsThreshold = 10000
dnsAmplificationIpEntropy = 50
dnsAmplificationFilter :: Flow -> Bool
dnsAmplificationFilter flow = proto flow == UDP
                                && dP flow == "53"
dnsAmplification :: AttackData
dnsAmplification = AD "DNS_AMPLIFICATION" dnsAmplificationFilter [Marker PPS dnsAmplificationPpsThreshold,
                                                                     Marker BPS dnsAmplificationBpsThreshold] dnsAmplificationIpEntropy

tcpSynFinFloodPpsThreshold = 500
tcpSynFinFloodIpEntropy = 10
tcpSynFinFloodFilter :: Flow -> Bool
tcpSynFinFloodFilter flow = proto flow == TCP
                                && fS flow
                                && fF flow
                                && not (fR flow)
                                && not (fA flow)
                                && not (fP flow)
                                && not (fX flow)
                                && not (fU flow)
tcpSynFinFlood :: AttackData
tcpSynFinFlood = AD "TCP_SYN_FIN_FLOOD" tcpSynFinFloodFilter [Marker PPS tcpSynFinFloodPpsThreshold] tcpSynFinFloodIpEntropy

tcpSynRstFloodPpsThreshold = 14030
tcpSynRstFloodIpEntropy = 10
tcpSynRstFloodFilter :: Flow -> Bool
tcpSynRstFloodFilter flow = proto flow == TCP
                                && fS flow
                                && fR flow
                                && not (fF flow)
                                && not (fA flow)
                                && not (fP flow)
                                && not (fX flow)
                                && not (fU flow)
tcpSynRstFlood :: AttackData
tcpSynRstFlood = AD "TCP_SYN_RST_FLOOD" tcpSynRstFloodFilter [Marker PPS tcpSynRstFloodPpsThreshold] tcpSynRstFloodIpEntropy

tcpFinFloodPpsThreshold = 14030
tcpFinFloodIpEntropy = 10
tcpFinFloodFilter :: Flow -> Bool
tcpFinFloodFilter flow = proto flow == TCP
                            && fF flow
                            && not (fR flow)
                            && not (fA flow)
                            && not (fS flow)
                            && not (fP flow)
                            && not (fU flow)
                            && not (fX flow)
tcpFinFlood :: AttackData
tcpFinFlood = AD "TCP_FIN_FLOOD" tcpFinFloodFilter [Marker PPS tcpFinFloodPpsThreshold] tcpFinFloodIpEntropy

ntpAmplificationBpsThreshold = 500000000
ntpAmplificationIpEntropy = 50
ntpAmplificationFilter :: Flow -> Bool
ntpAmplificationFilter flow = proto flow == UDP
                                && dP flow == "123"
ntpAmplification :: AttackData
ntpAmplification = AD "NTP_AMPLIFICATION" ntpAmplificationFilter [Marker BPS ntpAmplificationBpsThreshold] ntpAmplificationIpEntropy

ssdpAmplificationPpsThreshold = 500
ssdpAmplificationIpEntropy = 50
ssdpAmplificationFilter :: Flow -> Bool
ssdpAmplificationFilter flow = proto flow == UDP
                                 && dP flow == "1900"
ssdpAmplification :: AttackData
ssdpAmplification = AD "SSDP_AMPLIFICATION" ssdpAmplificationFilter [Marker PPS ssdpAmplificationPpsThreshold] ssdpAmplificationIpEntropy

xmasDdosPpsThreshold = 500
xmasDdosIpEntropy = 10
xmasDdosFilter :: Flow -> Bool
xmasDdosFilter flow = proto flow == TCP
                        && fF flow
                        && fP flow
                        && fU flow
                        && not (fR flow)
                        && not (fA flow)
                        && not (fS flow)
                        && not (fX flow)
xmasDdos :: AttackData
xmasDdos = AD "XMAS_DDOS" xmasDdosFilter [Marker PPS xmasDdosPpsThreshold] xmasDdosIpEntropy

httpFloodPpsThreshold = 377260000
httpFloodBpsThreshold = 2000
httpFloodIpEntropy = 100
httpFloodFilter :: Flow -> Bool
httpFloodFilter flow = proto flow == TCP
                       && (dP flow == "80" || dP flow == "443" || dP flow == "8080")
httpFlood :: AttackData
httpFlood = AD "HTTP_FLOOD" httpFloodFilter [Marker PPS httpFloodPpsThreshold, Marker BPS httpFloodBpsThreshold] httpFloodIpEntropy

chargenAmplificationPpsThreshold = 500
chargenAmplificationIpEntropy = 50
chargenAmplificationFilter :: Flow -> Bool
chargenAmplificationFilter flow = proto flow == UDP
                                    && dP flow == "19"
chargenAmplification :: AttackData
chargenAmplification = AD "CHARGEN_AMPLIFICATION" chargenAmplificationFilter [Marker PPS chargenAmplificationPpsThreshold] chargenAmplificationIpEntropy

snmpAmplificationPpsThreshold = 500
snmpAmplificationIpEntropy = 50
snmpAmplificationFilter :: Flow -> Bool
snmpAmplificationFilter flow = proto flow == UDP
                                 && dP flow == "161"
snmpAmplification :: AttackData
snmpAmplification = AD "SNMP_AMPLIFICATION" snmpAmplificationFilter [Marker PPS snmpAmplificationPpsThreshold] snmpAmplificationIpEntropy

tftpAmplificationPpsThreshold = 500
tftpAmplificationIpEntropy = 50
tftpAmplificationFilter :: Flow -> Bool
tftpAmplificationFilter flow = proto flow == UDP
                                 && dP flow == "69"
tftpAmplification :: AttackData
tftpAmplification = AD "TFTP_AMPLIFICATION" tftpAmplificationFilter [Marker PPS tftpAmplificationPpsThreshold] tftpAmplificationIpEntropy

netbiosAmplificationPpsThreshold = 500
netbiosAmplificationIpEntropy = 50
netbiosAmplificationFilter :: Flow -> Bool
netbiosAmplificationFilter flow = proto flow == UDP
                                    && dP flow == "137"
netbiosAmplification :: AttackData
netbiosAmplification = AD "NETBIOS_AMPLIFICATION" netbiosAmplificationFilter [Marker PPS netbiosAmplificationPpsThreshold] netbiosAmplificationIpEntropy

qotdAmplificationPpsThreshold = 500
qotdAmplificationIpEntropy = 50
qotdAmplificationFilter :: Flow -> Bool
qotdAmplificationFilter flow = proto flow == UDP
                                 && dP flow == "17"
qotdAmplification :: AttackData
qotdAmplification = AD "QOTD_AMPLIFICATION" qotdAmplificationFilter [Marker PPS qotdAmplificationPpsThreshold] qotdAmplificationIpEntropy

quakeAmplificationPpsThreshold = 500
quakeAmplificationIpEntropy = 50
quakeAmplificationFilter :: Flow -> Bool
quakeAmplificationFilter flow = proto flow == UDP
                                  && dP flow == "27960"
quakeAmplification :: AttackData
quakeAmplification = AD "QUAKE_AMPLIFICATION" quakeAmplificationFilter [Marker PPS quakeAmplificationPpsThreshold] quakeAmplificationIpEntropy

steamAmplificationPpsThreshold = 500
steamAmplificationIpEntropy = 50
steamAmplificationFilter :: Flow -> Bool
steamAmplificationFilter flow = proto flow == UDP
                                  && dP flow == "27015"
steamAmplification :: AttackData
steamAmplification = AD "STEAM_AMPLIFICATION" steamAmplificationFilter [Marker PPS steamAmplificationPpsThreshold] steamAmplificationIpEntropy

portmapperAmplificationPpsThreshold = 500
portmapperAmplificationIpEntropy = 50
portmapperAmplificationFilter :: Flow -> Bool
portmapperAmplificationFilter flow = proto flow == UDP
                                       && dP flow == "111"
portmapperAmplification :: AttackData
portmapperAmplification = AD "PORTMAPPER_AMPLIFICATION" portmapperAmplificationFilter [Marker PPS portmapperAmplificationPpsThreshold] portmapperAmplificationIpEntropy

mssqlAmplificationPpsThreshold = 500
mssqlAmplificationIpEntropy = 50
mssqlAmplificationFilter :: Flow -> Bool
mssqlAmplificationFilter flow = proto flow == UDP
                                  && dP flow == "1434"
mssqlAmplification :: AttackData
mssqlAmplification = AD "MSSQL_AMPLIFICATION" mssqlAmplificationFilter [Marker PPS mssqlAmplificationPpsThreshold] mssqlAmplificationIpEntropy

ripv1AmplificationPpsThreshold = 500
ripv1AmplificationIpEntropy = 50
ripv1AmplificationFilter :: Flow -> Bool
ripv1AmplificationFilter flow = proto flow == UDP
                                  && dP flow == "520"
ripv1Amplification :: AttackData
ripv1Amplification = AD "RIPV1_AMPLIFICATION" ripv1AmplificationFilter [Marker PPS ripv1AmplificationPpsThreshold] ripv1AmplificationIpEntropy

sentinelAmplificationPpsThreshold = 500
sentinelAmplificationIpEntropy = 50
sentinelAmplificationFilter :: Flow -> Bool
sentinelAmplificationFilter flow = proto flow == UDP
                                     && dP flow == "5093"
sentinelAmplification :: AttackData
sentinelAmplification = AD "SENTINEL_AMPLIFICATION" sentinelAmplificationFilter [Marker PPS sentinelAmplificationPpsThreshold] sentinelAmplificationIpEntropy

memchachedPpsThreshold = 500
memchachedIpEntropy = 50
memchachedFilter :: Flow -> Bool
memchachedFilter flow = proto flow == UDP
                         && dP flow == "11211"
memchached :: AttackData
memchached = AD "MEMCHACHED" memchachedFilter [Marker PPS memchachedPpsThreshold] memchachedIpEntropy

clapdAmplificationPpsThreshold = 500
clapdAmplificationIpEntropy = 50
clapdAmplificationFilter :: Flow -> Bool
clapdAmplificationFilter flow = proto flow == UDP
                                  && dP flow == "389"
clapdAmplification :: AttackData
clapdAmplification = AD "CLAPD_AMPLIFICATION" clapdAmplificationFilter [Marker PPS clapdAmplificationPpsThreshold] clapdAmplificationIpEntropy

greFloodBpsThreshold = 200000000
greFloodIpEntropy = 10
greFloodFilter :: Flow -> Bool
greFloodFilter flow = proto flow == GRE
greFlood :: AttackData
greFlood = AD "GRE_FLOOD" greFloodFilter [Marker BPS greFloodBpsThreshold] greFloodIpEntropy


ad :: [AttackData]
ad = [
      malformTcp,
      malformUdp,
      icmpMisuse,
      tcpRstFlood,
      tcpSynFinFlood,
      tcpSynRstFlood,
      tcpFinFlood,
      greFlood,
      clapdAmplification,
      portmapperAmplification,
      mssqlAmplification,
      qotdAmplification,
      quakeAmplification,
      steamAmplification
     ]

