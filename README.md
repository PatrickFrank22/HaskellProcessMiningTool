# Haskell & Process Mining

This uploaded project was created as part of the bachelor thesis "Haskell & Process Mining".
In particular, it consists of an implementation of the Inductive Miner and Inductive Miner - Infrequent algorithms, as well as two XES and one CSV parser.
The XES parsers are based on libraries xml-conduit and hxt.
The CSV parser is based on library cassava.

All implemented functions related to the Inductive Miner and Inductive Miner - Infrequent as well as the XES and CSV parsers are explained in *Patrick Frank. Haskell & Process Mining, 2024*.
The thesis also shows or argues about the correctness of the Inductive Miner implementations.


## Project Setup

Once the folder has been downloaded from GitHub, and GHC and cabal have been installed, please follow these steps:

1. Select the "app" directory within the downloaded folder in the console.

To install the necessary libraries, run the following commands: 

2. cabal update
3. chmod +x install_libraries.sh 
3. ./install_libraries.sh


## Compile and run the application: 

The application can be compiled and run using the following commands:

1. ghc -O -rtsopts -o haskellApp Main.hs -package base -package time -package bytestring -package filepath -package yesod-form 
2. ./haskellApp +RTS -sstderr


All further information and more detailed explanations can be found in the thesis.


## Changes made to the version originally uploaded on 19 February 2024:

### xorCut: 
Version with improved runtime has been implemented.

### Log filtering change:
Once we have the frequent transitions and start and end activities, the better (and correct) procedure is as follows:
Activities that remain in the event log must satisfy the condition:

$\forall_{a{\in}\Sigma} \exists_{s{\in}Start} \exists_{e{\in}End} (s \rightarrow^+ a \lor a{\in}Start) \land (a \rightarrow^+ e \lor a{\in}End)$

This means that all activities in the event log must be reachable from one of the start activities via a path of frequent transitions, or be a start activity.
In addition, all activities must reach one of the end activities or be an end activity.
Otherwise, they must be removed.
We have implemented this change as well.

(The original version can be found in Project "HaskellProcessMining".)