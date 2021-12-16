FROM vistalab/fsl-v5.0 as fsl
FROM python:2.7

# Create file structure
RUN mkdir /usr/local/freesurfer /VBM /fsl

# Copy the FSL files
# Using directly the 'vistalab/fsl-v5.0' was causing problems
# when trying to install the necessary python packages
COPY --from=fsl /usr/share/fsl/5.0 /usr/share/fsl/5.0
COPY --from=fsl /usr/lib/fsl/5.0 /usr/lib/fsl/5.0
COPY --from=fsl /usr/share/fsl/data /usr/share/fsl/data
COPY --from=fsl /usr/lib/ /usr/lib/
RUN chmod +x /usr/share/fsl/5.0/bin/libnewimage.so
RUN chmod +x /usr/share/fsl/5.0/bin/libmeshclass.so
ENV FSLDIR=/usr/share/fsl/5.0
ENV PATH=$PATH:$FSLDIR
ENV LD_LIBRARY_PATH=$FSLDIR:/usr/share/fsl/5.0/bin
ENV FSLOUTPUTTYPE=NIFTI_GZ
# RUN /bin/bash -c 'source /usr/local/fsl/etc/fslconf/fsl.sh'

# Install the requirements
COPY requirements.txt ./
RUN pip install -r requirements.txt

# Configuring freesurfer
ENV FREESURFER_HOME=/usr/local/freesurfer
COPY ./license.txt /usr/local/freesurfer/.license

# Alternative
# Using the freesurfer image as base will take around 10GB of space
# FROM freesurfer/freesurfer:7.1.1

# Copy the necessary files
COPY ./external_functions /utils
COPY ./VBM /VBM
COPY pre_processing.sh import_from_xnat.py upload_to_xnat.py ./
RUN chmod +x ./pre_processing.sh

# Set the necessary environment variables
ENV CONFIG=/VBM/GM_2_MNI152GM_1mm.cnf
ENV VBM_DIR=/VBM

# Import environment variables
#ENV IMAGES_INPUT = ${IMAGES_INPUT}
#ENV IMAGES_OUTPUT = ${IMAGES_OUTPUT}
#ENV XNAT_USER = ${XNAT_USER}
#ENV XNAT_PASSWORD = ${XNAT_PASSWORD}
#ENV XNAT_HOST = ${XNAT_HOST}
#ENV XNAT_PROJECT = ${XNAT_PROJECT}

# Start the service to pre-process the images
CMD [ "bash" ]
