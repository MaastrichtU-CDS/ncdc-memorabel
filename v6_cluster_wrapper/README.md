# Vantage6 Algorithm Wrapper to run in a Cluster

The NCDC Memorabel project requires more computational power to certain tasks that can't be provided by the default server running the vantage6 node. 
Being able to run an algorithm in a different machine while using the vantage6 infrastructure requires some adaptations.

## Solution
Each cohort will have a default docker image consisting of an adaptation from the usual algorithm wrapper used in vantage6.
This image will have the configurations required to deploy the algorithm in a different machine.
The algorithm can be generalize and only needs to have a standard way to input and output data, compliant with the wrapper.

## Maastricht
The Maastricht node will take advantage of a GPU cluster using OpenShift.
