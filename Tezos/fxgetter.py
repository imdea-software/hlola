#!/usr/bin/env python3
import argparse
import requests
import json
import sys

extractList = ['sender.address', 'senderaddress', 'target.address', 'targetaddress', 'parameter.value.value', 'paramvalue', 'parameter.entrypoint', 'paramentry', 'sender.alias', 'senderalias', 'target.alias', 'targetalias', 'parameter.value.route.aTokenAmount', 'atokenamt', 'parameter.value.route.bTokenAmount', 'btokenamt', 'parameter.value', 'val']

urlBase = "https://api.tzkt.io/v1/operations/transactions/?"

def err(msg):
  print(msg)
  exit(-1)

def getpath(dic, ls):
  dic = dic[0] if isinstance(dic,list) and dic != [] else dic
  if ls:
    if (not hasattr(dic, '__iter__')) or ls[0] not in dic:
      return None
    return getpath(dic[ls[0]], ls[1:])
  return dic

def filterandprint(skip, remaining, evs, extract):
  i=0
  print()
  for ev in evs:
    for e in extract:
      val = getpath(ev, e[0])
      val = val[0] if isinstance(val,list) and val != [] else val
      ev[e[1]] = "" if val == None else val
    sev = json.dumps(ev)
    if skip <= 0:
      print(sev)
    skip -=1 
    i+=1
    if remaining-i==0:
      break
  return i

def getevs(reqstr):
  r = requests.get(reqstr)
  try:
    evs = r.json()
  except json.decoder.JSONDecodeError as e:
    err(e,r)
  return evs

def getAllEvents(url,skip, maxevs,maxreqs,extract):
  pagesize=1000
  if '?' in url:
    url += '&'
  else:
    url += '?'
  url += f'limit={pagesize}'
  evs = getevs(url)
  nevs = filterandprint(skip, maxevs, evs, extract)
  skip = skip - nevs
  i=0
  while len(evs) >= pagesize and (maxevs<0 or nevs < maxevs) and (maxreqs<0 or i < maxreqs):
    i+=1
    suffix = "&offset="+str(i*pagesize)
    evs = getevs(url+suffix)
    tmp_nevs = filterandprint(skip, maxevs-nevs, evs, extract)
    nevs += tmp_nevs
    skip = skip - tmp_nevs

def processextract(raws):
  extracts = []
  if raws:
    if len(raws)%2 == 1:
      err("Odd number of arguments in extract")
    for i in range(len(raws)//2):
      path = raws[i*2].split('.')
      extracts.append([path, raws[i*2+1]])
  return extracts

def main():
  '''
  Use:
  
  ./tzgetter.py [--block block_number] [--target transaction_target] [--maxevs number_of_events] [--maxreqs maximum_requests][--extract paths]
  '''
  parser = argparse.ArgumentParser('Tezos events retriever')
  parser.add_argument('--blocksfrom', help='Get all blocks starting from arg', type=int, default=-1)
  parser.add_argument('--block', help='Block to retrieve', type=str, default="0")
  parser.add_argument('--target', help='Get transaction target', type=str, default="0")
  parser.add_argument('--skipevs', help='Skip first events', type=int, default=-1)
  parser.add_argument('--maxevs', help='Max number of events', type=int, default=-1)
  parser.add_argument('--maxreqs', help='File to process', type=int, default=-1)
  parser.add_argument('--extract', nargs='*')
  
  args = parser.parse_args()
  allExtract = extractList + args.extract if args.extract else extractList
  
  if args.blocksfrom != -1:
    bl = int(args.blocksfrom)
    while bl < 5300000:
      getAllEvents(urlBase + "level=" + str(bl), -1, -1, -1, processextract(allExtract))
      bl += 1
      
  if args.block != "0":
    getAllEvents(urlBase + "level=" + args.block, args.skipevs, args.maxevs, args.maxreqs, processextract(allExtract))
  else:
    getAllEvents(urlBase + "target=" + args.target, args.skipevs, args.maxevs, args.maxreqs, processextract(allExtract))
  
if __name__ == '__main__':
    main()
    sys.stdout.close()
    exit(0)

# ./fxgetter.py "https://api.tzkt.io/v1/operations/transactions/?level=5070834" > datagetter.json
    # https://api.tzkt.io/v1/operations/transactions/?level=5070834
# ./fxgetter.py "https://api.tzkt.io/v1/operations/transactions/?level=5070834" --extract sender.address senderaddress > datagetter.json
# ./fxgetter.py "https://api.tzkt.io/v1/operations/transactions/?level=5070834" --extract sender.address senderaddress parameter.value.value paramvalue > datagetter.json
# ./fxgetter.py "https://api.tzkt.io/v1/operations/transactions/?level=5070834" --extract sender.address senderaddress parameter.value.value paramvalue parameter.entrypoint paramentry sender.alias senderalias target.alias targetalias > datagetter.json
