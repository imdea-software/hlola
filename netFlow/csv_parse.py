import csv
import json
import os

data_path = 'data/inputs'
stat_path = 'data/csv_stat'
flow_path = 'data/csv'


def dict_filter(it, *keys):
    for d in it:
        yield dict((k, d[k]) for k in keys)
        
def make_json(csvFilePath, jsonFilePath):
    malf_pak = 0
    data = {}
    count = 0
    
    with open(csvFilePath, encoding='utf-8') as csvf:
        csvReader = csv.DictReader(csvf)
        
        # for d in dict_filter(csvReader, 'ts', 'te', 'sa', 'da', 'sp', 'dp', 'pr', 'ipkt', 'ibyt', 'in', 'out', 'sas', 'das'):
        for d in csvReader:
#            if (d['pr'] == 'UDP') and (d['dp'] == '0') and (d['da'] == '147.96.254.19'):
            if (d['pr'] == 'UDP') and (d['dp'] == '0') and (d['sp'] == '0'):
                malf_pak += 1
                
            # key = d['ts']
            key = count
            count += 1
            data[key] = d
            
    print (malf_pak)

    with open(jsonFilePath, 'w', encoding='utf-8') as jsonf:
        jsonf.write(json.dumps(data, indent=4))
        
# csv_path = 'data/output.csv'
# json_path = 'data/output_python.json'
# make_json(csv_path, json_path)

# csv_path = 'data/output_attack.csv'
# json_path = 'data/output_attack_python.json'
# make_json(csv_path, json_path)



directory = flow_path

for filename in os.listdir(directory):
    f = os.path.join(directory, filename)

    if os.path.isfile(f):
        print(f)
        
        malf_pak = 0
        data = {}
        count = 0
        
        with open(f, encoding='utf-8') as csvf:
            csvReader = csv.DictReader(csvf)
        
            # for d in dict_filter(csvReader, 'ts', 'te', 'sa', 'da', 'sp', 'dp', 'pr', 'ipkt', 'ibyt', 'in', 'out', 'sas', 'das'):
            for d in csvReader:
                
                #if (d['pr'] == 'UDP') and (d['dp'] == '0') and (d['da'] == '147.96.254.19'):
                # if (d['pr'] == 'UDP') and (d['dp'] == '0') and (d['sp'] == '0'):
                if (d['dp'] == '0') and (d['sp'] == '0'):
                    malf_pak += 1
                
                # key = d['ts']
                key = count
                count += 1
                data[key] = d
            
        print (malf_pak)
        print(count)











