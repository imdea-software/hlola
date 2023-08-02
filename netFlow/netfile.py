import os
import json
import subprocess
import datetime
import sys #sys.maxsize
import random #randint

from enum import IntEnum

def execute_command(cmd):
  '''
  Executes command cmd and yields output line by line
  '''
  popen = subprocess.Popen(cmd, stdout=subprocess.PIPE, universal_newlines=True)
  for stdout_line in iter(popen.stdout.readline, ""):
    yield stdout_line
  popen.stdout.close()
  return_code = popen.wait()
  if return_code:
    raise subprocess.CalledProcessError(return_code, cmd)

def parse_date(d):
  '''
  Converts netflow date into seconds since epoch
  2001-01-01 01:01:01
  '''
  date_hour = d.split(" ")
  date = date_hour[0].split("-")
  time = date_hour[1].split(":")
  epoch = datetime.datetime(int(date[0]), int(date[1]), int(date[2]), int(time[0]), int(time[1]), int(time[2])).timestamp()
  return int(epoch)

# Attack = IntEnum('Attack', ['TCP_SYN_FLOOD', 'IP_PROTO_NULL', 'TCP_FLAG_NULL', 'MALFORM_TCP', 'MALFORM_UDP', 'ICMP_MISUSE', 'LAND_ATTACK', 'TCP_RST_FLOOD', 'UDP_FLOOD', 'IP_FRAGMENTATION', 'HOST_TOTAL_TRAFFIC', 'HOST_TCP_TRAFFIC', 'TCP_PORT_SCAN', 'DNS_AMPLIFICATION', 'TCP_SYN_FIN_FLOOD', 'TCP_SYN_RST_FLOOD', 'TCP_FIN_FLOOD', 'NTP_AMPLIFICATION', 'SSDP_AMPLIFICATION', 'XMAS_DDOS', 'HTTP_FLOOD', 'CHARGEN_AMPLIFICATION', 'SNMP_AMPLIFICATION', 'TFTP_AMPLIFICATION', 'NETBIOS_AMPLIFICATION', 'QOTD_AMPLIFICATION', 'QUAKE_AMPLIFICATION', 'STEAM_AMPLIFICATION', 'PORTMAPPER_AMPLIFICATION', 'MSSQL_AMPLIFICATION', 'RIPV1_AMPLIFICATION', 'SENTINEL_AMPLIFICATION', 'MEMCHACHED', 'CLAPD_AMPLIFICATION', 'GRE_FLOOD'], start=0)

class Attack(IntEnum):
    TCP_SYN_FLOOD = 0
    IP_PROTO_NULL = 1
    TCP_FLAG_NULL = 2
    MALFORM_TCP = 3
    MALFORM_UDP = 4
    ICMP_MISUSE = 5
    TCP_RST_FLOOD = 6
    UDP_FLOOD = 7
    HOST_TCP_TRAFFIC = 8
    TCP_PORT_SCAN = 9
    DNS_AMPLIFICATION = 10
    TCP_SYN_FIN_FLOOD = 11
    TCP_SYN_RST_FLOOD = 12
    TCP_FIN_FLOOD = 13
    NTP_AMPLIFICATION = 14
    SSDP_AMPLIFICATION = 15
    XMAS_DDOS = 16
    HTTP_FLOOD = 17
    CHARGEN_AMPLIFICATION = 18
    SNMP_AMPLIFICATION = 19
    TFTP_AMPLIFICATION = 20
    NETBIOS_AMPLIFICATION = 21
    QOTD_AMPLIFICATION = 22
    QUAKE_AMPLIFICATION = 23
    STEAM_AMPLIFICATION = 24
    PORTMAPPER_AMPLIFICATION = 25
    MSSQL_AMPLIFICATION = 26
    RIPV1_AMPLIFICATION = 27
    SENTINEL_AMPLIFICATION = 28
    MEMCHACHED = 29
    CLAPD_AMPLIFICATION = 30
    GRE_FLOOD = 31
    
    def get_command_marker(self):
      return (self.list_attack[self.value][0] + self.list_attack[self.value][1])
    def get_command_filter(self):
      return self.list_attack[self.value][1]    
    def get_key(self):
      return self.list_attack[self.value][2]
      
'''
For each Attack (indexed by the enum), this list contains
- the command to get the summary
- the key for the marker
'''      
Attack.list_attack = [
  [["-n", "1", "-s", "dstip"], [" proto tcp and flags S and not flags RAFPUX"], ['ipps']], # TCP_SYN_FLOOD
  [["-n", "1", "-s", "dstip"], [" proto 0"], ['ipps']], # IP_PROTO_NULL
  [["-n", "1", "-s", "dstip"], [" proto tcp and not flags A and not flags SFRPUX"], ['ipps']], # TCP_FLAG_NULL
  [["-n", "1", "-s", "dstip"], [" dst port 0 and proto tcp"], ['ipps']], # MALFORM_TCP
  [["-n", "1", "-s", "dstip"], [" dst port 0 and proto udp"], ['ipps']], # MALFORM_UDP
  [["-n", "1", "-s", "dstip"], [" proto icmp"], ['ipps']], # ICMP_MISUSE
  [["-n", "1", "-s", "dstip"], [" proto tcp and flags R and not flags FASPUX"], ['ipps']], # TCP_RST_FLOOD
  [["-n", "1", "-s", "dstip"], [" proto udp"], ['ibps']], # UDP_FLOOD
  [["-n", "1", "-s", "dstip"], [" proto tcp"], ['ibps']], # HOST_TCP_TRAFFIC
  [["-n", "1", "-s", "dstip"], [" proto tcp and flags S and not flags RAFPUX and bpp < 66"], ['ipps']], # TCP_PORT_SCAN
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 53"], ['ipps', 'ibps']], # DNS_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto tcp and flags SF and not flags ARPUX"], ['ipps']], # TCP_SYN_FIN_FLOOD
  [["-n", "1", "-s", "dstip"], [" proto tcp and flags SR and not flags AFPUX"], ['ipps']], # TCP_SYN_RST_FLOOD
  [["-n", "1", "-s", "dstip"], [" proto tcp and flags F and not flags RASPUX"], ['ipps']], # TCP_FIN_FLOOD
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 123"], ['ibps']], # NTP_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 1900"], ['ipps']], # SSDP_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto tcp and flags FPU and not flags RASX"], ['ipps']], # XMAS_DDOS
  [["-n", "1", "-s", "dstip"], [" (proto tcp and port 80) or (proto tcp and port 443) or (proto tcp and port 8080)"], ['ipps', 'ibps']], # HTTP_FLOOD
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 19"], ['ipps']], # CHARGEN_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 161"], ['ipps']], # SNMP_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 69"], ['ipps']], # TFTP_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 137"], ['ipps']], # NETBIOS_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 17"], ['ipps']], # QOTD_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 27960"], ['ipps']], # QUAKE_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 27015"], ['ipps']], # STEAM_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 111"], ['ipps']], # PORTMAPPER_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 1434"], ['ipps']], # MSSQL_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 520"], ['ipps']], # RIPV1_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 5093"], ['ipps']], # SENTINEL_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 11211"], ['ipps']], # MEMCHACHED
  [["-n", "1", "-s", "dstip"], [" proto udp and dst port 389"], ['ipps']], # CLAPD_AMPLIFICATION
  [["-n", "1", "-s", "dstip"], [" proto gre"], ['ibps']] # GRE_FLOOD
]

class Netfile:

  def __init__(self, fpath, csv_flow_dict, csv_summary_dict):
    self.fpath = fpath
    self.csv_flow_dict = csv_flow_dict
    self.csv_summary_dict = csv_summary_dict

    
  def process_flow_file(self, list_keys, csv_dict, command, until=0, sample=0):
    '''
    @param list_keys List of keys to be printed
    @param csv_dict all keys
    @param command nfdump command (in list of strings format)
    Calls nfdump with the options defined in 'command' for Netfile
    Prints a json line with the keys and corresponding values
    '''
    first = True
    count_flows = True if (until and until != 0) else False
    sampling = True if sample > 0 else False
    count_sampling = 0

    for flow in execute_command(["nfdump", "-r", self.fpath, "-q", "-o", "csv"] + command):
      if first:
        if len(flow) > 1:
          first = False
      elif len(flow) > 1:
        
        if sampling:
          count_sampling = (count_sampling + 1) % sample
          if count_sampling != 0:
            if count_flows:
              until = until - 1
              if until == 0:
                break
            continue

        my_list = flow.split(",")
        zip_iterator = zip(csv_dict, my_list)
        nf_dict = dict(zip_iterator)

        list_values = [nf_dict[k] for k in list_keys]
        zip_it = zip(list_keys, list_values)
        json_dict = dict(zip_it)
        json_dict['file'] = self.fpath.split(".")[-1]
        
        if 'pr' in list_keys:
          if json_dict['pr'] == "0":
            json_dict['pr'] = "NULL"
        if 'ibyt' in list_keys:
          json_dict['ibyt'] = int(json_dict['ibyt'])
        if 'ipkt' in list_keys:
          json_dict['ipkt'] = int(json_dict['ipkt'])
        if 'td' in list_keys:
          json_dict['td'] = int(float(json_dict['td']))
        if 'te' in list_keys: 
          json_dict['te'] = parse_date(json_dict['te'])
        if 'ts' in list_keys: 
          json_dict['ts'] = parse_date(json_dict['ts'])
        if 'flg' in list_keys:
          json_dict['flagA'] = True if ('A' in json_dict['flg']) else False
          json_dict['flagS'] = True if ('S' in json_dict['flg']) else False
          json_dict['flagR'] = True if ('R' in json_dict['flg']) else False
          json_dict['flagF'] = True if ('F' in json_dict['flg']) else False
          json_dict['flagP'] = True if ('P' in json_dict['flg']) else False
          json_dict['flagU'] = True if ('U' in json_dict['flg']) else False
          json_dict['flagX'] = True if ('X' in json_dict['flg']) else False
          json_dict.pop('flg')

        # # # To change flows to random Source address for tests
        # if 'sa' in list_keys:
        #   json_dict['sa'] = str(random.randint(0, sys.maxsize))
        # if 'da' in list_keys:
        #   json_dict['da'] = str(random.randint(0, 100))          

        print(json.dumps(json_dict), flush = True)

        if count_flows:
          until = until - 1
          if until == 0:
            break
         
  def process_flow_file_marker(self, list_keys, csv_dict, command):
    '''
    @param key List with keys from csv dictionary
    @param csv_dict The dictionary of possible keys
    @param command nfdump command (in list of strings format) for getting the summary
    Calls nfdump with the options defined in 'command' for Netfile and returns for each key a tuple (key, val)
    '''
    first = True
    for flow in execute_command(["nfdump", "-r", self.fpath, "-q", "-o", "csv"] + command):
      if first:
        # Skip first NON ZERO flow
        if len(flow) > 1:
          first = False
      elif len(flow) > 1:
        my_list = flow.split(",")
        zip_iterator = zip(csv_dict, my_list)
        nf_dict = dict(zip_iterator)
        list_values = [(k, 0) if nf_dict[k] == k else (k, nf_dict[k]) for k in list_keys]
        return list_values
    list_values_empty = [(k, '0') for k in list_keys]
    return list_values_empty

  def get_marker(self, attack):
    return (self.process_flow_file_marker(attack.get_key(), self.csv_summary_dict, attack.get_command_marker()))
         
