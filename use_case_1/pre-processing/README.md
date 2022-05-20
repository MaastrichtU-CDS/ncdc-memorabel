## Pre-processing pipeline for NCDC use case 1

The input images used in use case 1 are the result of a pre-processing applied to MRI scans based on the freesurfer segmentations.
The pipeline has been dockerized in order to be easily applied in an external cluster with more computational power.

The original pre-processing pipeline can be found [here](https://github.com/roshchupkin/FSfslVBM).

### Building the docker image

In order to build the image, you need to provide the external functions from Freesurfer and FSL in the following folder `./external_functions`. As an alternative, you can also install Freesurfer and FSL, although it'll increase the image size with unecessary content.
Run the `./Dockerfile` in order to create the docker image to run the pipeline.

### Running

A docker image is available under the following tag: `pmateus/memorabel-mri-preprocessing:0.0.12`.
The following environment variables should be provided to the container:
- `JOBS`: number of parallel jobs to run.
- `INPUT_FOLDER`: input folder where the data will be placed in the expected format.
- `SAVE_PATH`: output folder to store the results.

Note: The current version doesn't use the multiprocessing tools due to problems in the running environment. However, the script can be initialized in parallel multiple times and the multiprocessing code is still available (commented).
