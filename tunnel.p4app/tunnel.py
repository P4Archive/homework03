
from scapy.all import *
import sys, os

TYPE_TUNNEL = 0x6114
TYPE_IPV4 = 0x0800

class Tunnel(Packet):
    name = "MyTunnel"
    fields_desc = [
        XByteField("pre", 0),
        XByteField("tunnel_id", 0),
        XByteField("post", 255)
    ]
    def mysummary(self):
        return self.sprintf("pre=%pre%, tunnel_id=%tunnel_id%, post=%post%")

bind_layers(Ether, Tunnel, type=TYPE_TUNNEL)
bind_layers(Tunnel, IP, pid=TYPE_IPV4)
