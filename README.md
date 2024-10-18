# NCDC Memorabel

The NCDC (Netherlands Consortium of Dementia Cohorts) project aims to take advantage of the data collected by different cohorts for research focused on dementia and cognitive aging.

One of the approaches for data analysis in this project consists of using the PHT (Personal Health Train) architecture. This will allow the analysis of data from 9 cohorts following a federated learning methodology which provides an additional layer of privacy by keeping the data at its original location.
This repository comprises information and tools used in this project.

## Ontology

The EMIF-AD ontology provides a starting point for a formal representation of the knowledge domain.
Moreover, it can enhance the data representation by providing a richer metadata description of each concept.

## Data Harmonization

The collaboration between the different cohorts requires an interoperable, maintainable, and reusable data infrastructure.
This led to the development of scripts that can automate and facilitate the process of harmonizing and standardizing the data to a clinical data model.

## Vantage6 Cluster Wrapper

In a federated infrastructure, the computational requirements for the different tasks vary greatly.
Usually more intensive tasks would be handled by a GPU cluster, a HPC, or another solution that can be momentarily reserved to execute it.
However, in a federated context the first point of contact won't be a machine with such computational power.
With this challenge in mind, this POC provides a docker wrapper acting as the bridge between the vantage6 node and another server/cluster/hpc that will run the algorithm.

## Use Cases

### Use case 1

For this use case, we trained a convolutional neural network to predict brain age across 3 cohorts using a federated infrastructure.
In this repository, we included the tools for the different pre-processing steps necessary to have the data and methods ready for use case 1.
The federated algorithm (https://github.com/MaastrichtU-CDS/federated-brain-age) makes use of the vantage6 cluster wrapper.

### Use case 2

Developing linear models to explore the association of plasma AD biomarkers with cognitive decline and progression to dementia.
This use case was initially performed by a researcher with direct access to the data (https://doi.org/10.1016/j.cccb.2024.100271).
Here, we adapted the scripts for the federated infrastructure with the goal of reproducing the results.

### Use case 3

Developing linear mixed models to evaluate the association between plasma biomarkers and cognitive decline across the NCDC cohorts.
