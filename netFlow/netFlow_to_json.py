import struct


def bytes_from_file(filename, buffsize=8192):
  with open(filename, "rb") as f:
    while True:
      buff = f.read(buffsize)
      if buff:
        for b in buff:
          yield b
        else:
          break
              

class Flow_Bin:
  
  def __init__(self, header):

    self.header = header[0:23]
    self.version = (header[0] << 8) | header[1]
    print(header[2])
    print(header[3])
    self.count = (header[2] << 8) | header[3]
    self.sysUptime = (header[4] << 24) | (header[5] << 16) | (header[6] << 8) | header[7]
    self.unix_secs = (header[8] << 24) | (header[9] << 16) | (header[10] << 8) | header[11]
    self.unix_nsecs = (header[12] << 24) | (header[13] << 16) | (header[14] << 8) | header[15]
    self.flow_sequence = (header[16] << 24) | (header[17] << 16) | (header[18] << 8) | header[19]
    self.engine_type = header[20]
    self.engine_id = header[21]
    self.sampling_interval = (header[22] << 8) | header[23]
    print(list(self.header))

# file_path = 'data/nfcapd.XXXX1
file_path = 'data/nfcapd.XXXX1'

# pack = struct.unpack('!HH', data[:4])

int_byte_list = []
for b in bytes_from_file(file_path):
  int_byte_list.append(b)

myFlow = Flow_Bin(int_byte_list)
print('version: ' + str(myFlow.version))
print('count: ' + str(myFlow.count))
print('sysUptime: ' + str(myFlow.sysUptime))
print('unix_secs: ' + str(myFlow.unix_secs))
print('unix_nsecs: ' + str(myFlow.unix_nsecs))
print('flow_seq: ' + str(myFlow.flow_sequence))
print('engine_type: ' + str(myFlow.engine_type))
print('engine_id: ' + str(myFlow.engine_id))
print('sampling_int: ' + str(myFlow.sampling_interval))

# for b in bytelist:
#   print(b)
  















