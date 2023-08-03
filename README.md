[![HLola logo](hlola.png)](https://software.imdea.org/hlola/)
# HLola

**HLola** is a Haskell implementation of the Stream Runtime Verification language [Lola](http://software.imdea.org/~cesar/papers/2005/time05/time05.pdf).
In this repository, you will find the source code of **HLola**, example specifications, and the steps to use the engine and define new specifications.
For more information, visit the [HLola official website](https://software.imdea.org/hlola).

## RV 2023
The following instructions indicate how to replicate the empirical evaluation described in the paper "A Stream Runtime Verification Tool with Nested and Retroactive Parametrization".

### Prerequisites
- A linux installation 
- Python 3 
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) 

### Installation 

Install HLola by cloning this branch of this repository and running the Haskell builder tool Stack, as follows: 

    $> git clone -b RV2023 https://github.com/imdea-software/hlola.git
    $> cd hlola
    $> stack install --local-bin-path .

This will create the executable `HLola` in the root directory.

Download the data to the root of the repository by running the following command:

    $> curl -o data.zip http://beastest.software.imdea.org/data.zip
    $> unzip data.zip

### DDOS detection

The data for the DDOS attack detection can be found in the `data` folder. 

The attacks, thresholds and types are defined in `src/Example/Attack.hs`; while the monitors are in `src/Example/Netflow.hs` and `src/Example/NetflowSummary.hs`.

The Python script `netFlow/process_netflow.py` processes the netflow data so it can serve as input streams for the monitor. 

#### Brute force and retroactive tests 

In order to execute the **brute force tests** for the attack date in a batch **with no attack**,run the following: 

    $> python3 ./netFlow/process_netflow.py --flows --batches 1 | ./HLola netflow 

(Note: the date to be processed are listed it the variable `dates` in `process_netflow.py`).

This will call the Python script that will read and process the netflow data, and pass it to the HLola monitor. 
    
The argument `--flows` indicates that the script must output all the individual flows for the date (and not just the default aggregated data). 

The argument `--batches 1` indicates that only one batch of data is to be processed. By default it will be the first batch of that date.

In order to execute the **brute force tests** for the attack date in the **attacked batch**, run the following: 

    $> python3 ./netFlow/process_netflow.py --flows --batches 1 --skip 87 | ./HLola netflow 

This will run the same test as previously, but the argument `--skip 87` indicates that it should skip the first 87 batches, and send to HLola the attacked batch (`--batches 1` indicates that only one batch will be processed after skipping the first 87).

In order to execute the **retroactive tests** for a date, run the following: 

    $> python3 ./netFlow/process_netflow.py --flows --batches 1 | ./HLola netflow --over 

In this case, `--over` indicates to the monitor that the it has to use this operator for calculating the entropy. The data from Python is identical from the previous case, but the monitor will behave differently.

#### Aggregated tests

The aggregated tests can be executed by running: 

    $> python3 ./netFlow/process_netflow.py | ./HLola netflowsummary 

In this case, the Python script will output (in JSON format), an aggregated attack marker per batch, that will be the input for the monitor. After 87 batches, the monitor will detect an attack.

If a marker surpasses the threshold, the monitor in `src/Examples/NetflowSummary.hs` will create a nested monitor in `src/Examples/Netflow.hs`, and the Python script will be called from inside the monitor to obtain the full flows for the batch where the threshold is exceeded.
The call to Python can be found in the function `innSpec` in `src/Examples/Netflow.hs`.
The parameters `--date` and `--attack` are received when the monitor is created and allow the Python script to filter the flows so that the monitor does not need to process all of them, but only those related to the attack in one batch.
