#!python3
import argparse
import subprocess
import os
import time
import json

import netfile as nf

data_path = './data/'

# dates = ['XXXXX1', 'XXXXX2']
dates = ['20220713'] 

csv_flow_dict = ['ts','te','td','sa','da','sp','dp','pr','flg','fwd','stos','ipkt','ibyt','opkt',
            'obyt','in','out','sas','das','smk','dmk','dtos','dir','nh','nhb','svln','dvln',
            'ismc','odmc','idmc','osmc','mpls1','mpls2','mpls3','mpls4','mpls5','mpls6',
            'mpls7','mpls8','mpls9','mpls10','cl','sl','al','ra','eng','exid','tr']

csv_summary_dict = ['ts','te','td','pr','val','fl','flP','ipkt','ipktP','ibyt','ibytP','ipps','ibps','ibpp']

# Keys used from the full flow dict
list_keys = ['te', 'ts', 'pr', 'da', 'sa', 'dp', 'sp', 'dp', 'flg', 'ibyt', 'ipkt', 'td']
  
def get_all_flows_cmd(command, list_keys, csv_dict):
  '''
  Goes over the directory data_path, and calls nfdump for each
  file with the options defined in 'command'
  '''
  for date in dates:
    file_path = data_path + date
    for file_name in sorted(os.listdir(file_path)):
      f = os.path.join(file_path, file_name)
      if os.path.isfile(f) and ("nfcapd." in f):
        netfile = nf.Netfile(f, csv_flow_dict, csv_summary_dict)
        netfile.process_flow_file(list_keys, csv_dict, command)
        break
    break

  
def get_flows_filtered(my_file, attack="", addr="", until=0, sample=0):
  '''Returns all flows in the indicated file, optionally filtering by
  attack and address, optionally until indicated batch, optionally
  sampling at indicated rate
  '''
  
  netfile = nf.Netfile(my_file, csv_flow_dict, csv_summary_dict)
  command = [""]
  if attack != "":
    command = attack.get_command_filter()
  if addr != "":
    command = command + [' and dst ip '] + [addr]
    
  netfile.process_flow_file(list_keys, csv_flow_dict, command, until, sample)
  
  
def get_all_flows(batches=0, until=0, skip=0, sample=0):
  '''
  Goes over the directory data_path, and calls nfdump for each
  file with the options defined in 'command'
  '''
  
  # Skip to flow 86 for NO ATTACK batch on XXXXX1
  # Skip to flow 87 for ATTACK batch on XXXXX1
  count_skip = 0
  count_batches = 0
  stop = False
  stopped = False
  
  if batches and batches > 0:
    stop = True
    
  for date in dates:

    # If enough batches have been printed, stop everything
    if stopped:
      break
    
    file_path = data_path + date
    for file_name in sorted(os.listdir(file_path)):
      f = os.path.join(file_path, file_name)

      if os.path.isfile(f) and ("nfcapd." in f):
        count_skip += 1
        if count_skip > skip:
          count_batches += 1
          if stop and count_batches > batches:
            stopped = True
            break
          netfile = nf.Netfile(f, csv_flow_dict, csv_summary_dict)
          netfile.process_flow_file(list_keys, csv_flow_dict, [""], until, sample)

          
def get_all_attack_markers_for_file(f):
  '''
  Prints in json format all attack markers for one file (batch)
  '''
  
  file_name = os.path.basename(f).replace('nfcapd.', '')
  list_attacks = ['file']
  list_markers = [file_name]
  netfile = nf.Netfile(f, csv_flow_dict, csv_summary_dict)
  for a in nf.Attack:
    markers = netfile.get_marker(a)
    if markers:
      for (k,m) in markers:
        list_attacks.append(str(a.name) + "_" + k)
        list_markers.append(int(m))
  zip_it = zip(list_attacks, list_markers)
  json_dict = dict(zip_it)
  print(json.dumps(json_dict), flush = True)

  
def get_markers_all_files(batches=0, skip=0):
  '''
  Prints in json format all attack markers for all batches in 'dates'.

  The optional argument batches indicates how many batches should be
  printed (after skip)

  '''
  
  # Change 'skip' value to skip an amount of batches
  # skip = 87
  # skip = 0
  count = 0
  stop = False
  if batches and batches > 0:
    stop = True  
  for date in dates:
    file_path = data_path + date
    for file_name in sorted(os.listdir(file_path)):
      f = os.path.join(file_path, file_name)
      if os.path.isfile(f) and ("nfcapd." in f):
        count += 1
        if count > skip:
          if stop and (count-skip) > batches:
            break
          get_all_attack_markers_for_file(f)
        
def main():
  '''
  Use:
  
  ./process_netflow.py --flows --batches 1
  '''
  
  # store_true is for just storing a boolean: whether the argument was used or not
  parser = argparse.ArgumentParser('Netflow processer')
  parser.add_argument('--date', help='File to process', type=str)
  parser.add_argument('--attack', help='Get flows filtering by attack', type=str)
  parser.add_argument('--flows', help='Get all flows', action = 'store_true')
  parser.add_argument('--attackaddr', action='store_true')
  parser.add_argument('--markers', help='Get json with markers for all attacks', action='store_true')
  parser.add_argument('--addr', help='Get flows filtered by dest address', type=str)
  parser.add_argument('--until', help='Get the indicated number of flows', type=int)
  parser.add_argument('--skip', help='Skip number of batches', default=0, type=int)
  parser.add_argument('--batches', help='Get all the flows from the indicated number of batches', type=int)
  parser.add_argument('--sampling', help='This will reduce the output by filtering to only 1 out of X flows', type=int)
  args = parser.parse_args()

  # The output will be flows
  if args.flows:

    sampling = 0
    if args.sampling:
      sampling = args.sampling
    
    address=""
    if args.addr:
      address=args.addr
      
    if args.date:
      # Find if file argument is a file
      my_date = args.date[0:-4]
      my_file = data_path + my_date + "/nfcapd." + args.date
      if os.path.isfile(my_file):
        
        if args.attack: 
          # Find which attack is indicated by flows
          attack_name = args.attack[0:args.attack.rfind("_")]
          attack = nf.Attack[attack_name]
          get_flows_filtered(my_file, attack=attack, addr=address, until=args.until, sample=sampling)
          
        # No attack
        else:
            get_flows_filtered(my_file, addr=address, until=args.until, sample=sampling)
            
    # No date
    else:
      get_all_flows(batches=args.batches, until=args.until, skip=args.skip, sample=sampling)
        
  # The outputs will be markers (summaries)
  else:

    # Markers for specific file
    if args.date:
      my_date = args.date[0:-4]
      my_file = data_path + my_date + "/nfcapd." + args.date
      if os.path.isfile(my_file):
        get_all_attack_markers_for_file(my_file)

    # Markers for all files    
    else:    
      get_markers_all_files(batches=args.batches, skip=args.skip)
           
    
if __name__ == '__main__':
    main()


