# # Netflow parsing based on https://github.com/alieissa/python-netflow

# from src.netflow.collector_v9 import ExportV9Packet
# from src.netflow.recorder import Recorder
# import struct


# def bytes_from_file(filename, buffsize=8192):
#   with open(filename, "rb") as f:
#     while True:
#       buff = f.read(buffsize)
#       if buff:
#         for b in buff:
#           yield b
#         else:
#           break
        
# def get_netflow_version(data):
#   version = struct.unpack('!H', bytes(data[:2]))[0]
#   return version


# file_path = 'data/nfcapd.XXXX1'
# #file_path = 'data/nfcapd.XXXX2'

# # pack = struct.unpack('!HH', data[:4])

# int_byte_list = []
# for b in bytes_from_file(file_path):
#   int_byte_list.append(b)
  
# # s = "Received data from {}, length {}".format(host, len(data))
# data = bytes(b)

# # Unpack netflow packet
# print("test")
# netflow_version = get_netflow_version(data)
# print(netflow_version)
# if(netflow_version == 1):
#   export = ExportV1Packet(data)
# elif(netflow_version == 5):
#   export = ExportV5Packet(data)
# elif(netflow_version == 9):
#   print("success")
# #   export = ExportV9Packet(data, self.TEMPLATES)
# # self.TEMPLATES.update(export.templates)

# # Append new flows
# # flows = [flow.data for flow in export.flows]
# # self.recorder.save(flows)
        


# import netflow
# import socket

# def bytes_from_file(filename, buffsize=8192):
#   with open(filename, "rb") as f:
#     while True:
#       buff = f.read(buffsize)
#       if buff:
#         for b in buff:
#           yield b
#         else:
#           break

# file_path = 'data/nfcapd.XXXX1'
# int_byte_list = []
# for b in bytes_from_file(file_path):
#   int_byte_list.append(b)

  
# p = netflow.parse_packet(b)  # Test result: <ExportPacket v5 with 30 records>
# print(p.header.version)




import ipfix.message
import ipfix.v9pdu
import ipfix.reader

file_path = 'data/nfcapd.XXXX2'
msg_obj = ipfix.reader.from_stream(open (file_path, mode="rb"))
print(msg_obj)
print("success")










