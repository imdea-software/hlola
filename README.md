**HLola** is a Haskell implementation of the Stream Runtime Verification language [Lola](http://software.imdea.org/~cesar/papers/2005/time05/time05.pdf).
In this repository, you will find the source code of **HLola**, example specifications, and the steps to use the engine and define new specifications.
For more information, visit the [HLola official website](https://software.imdea.org/hlola).

## FM 2024
The following instructions indicate how to replicate the empirical evaluation described in the paper "Retroactive Parametrized Runtime Verification".

### Prerequisites
- A linux installation 
- Python 3 
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) 

### Installation 

Install HLola by cloning this branch of this repository and running the Haskell builder tool Stack, as follows: 

    $> git clone -b FM2024 https://github.com/imdea-software/hlola.git
    $> cd hlola
    $> stack install --local-bin-path .

This will create the executable `HLola` in the root directory.

Download the data to the root of the repository by running the following command:

    $> curl -o data.zip http://beastest.software.imdea.org/data.zip
    $> unzip data.zip

### Sandwich attack detection

The data used in the paper can be found in the Tezos folder under the name `sandwichData.json`

The python script `Tezos/fxgetter.py` retrieves data from the Tezos indexer. It is used by the monitor when it needs to retrieve past information. The data in `sandwichData.json` was also produce using this script and the tezos indexer, but it is provided in a .json file so that the experiment can be reproduced in a more efficient way.

The monitors are defined in `src/Sandwich.hs`.

### Sandwich test execution
 
In order to execute the monitor to detect sandwich attacks and retrieve a list of suspicious addresses, run the following from the root of the repository:

    $> cat Tezos/sandwichData.json | ./HLola sandwich | grep "AttackYes"

This will execute the monitor using transactions stored in `src/Sandwich.hs` as input.

The `grep` command indicates that only the transactions when an attack is detected will be displayed. The output `listSuspicious` indicates the addresses that were in contact with the malicious contract. 
