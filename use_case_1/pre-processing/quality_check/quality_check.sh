#!/bin/bash

# Original: https://gitlab.com/radiology/neuro/brain-age/vbm/-/tree/master/FSfslVBM

while getopts ":i:o:" opt; do
    case $opt in
    i) IMAGE=$OPTARG ;;
    o) OUTPUT=$OPTARG ;;
    \?)
        echo "Invalid option: -$OPTARG" >&2
        exit 1
        ;;
    :)
        echo "Option -$OPTARG requires an argument." >&2
        exit 1
        ;;
    esac
done

if [ -z "$IMAGE" ]; then
	echo " need -i full path vbm nifti results (niftis)"
	exit 2
fi
if [ -z "$OUTPUT" ]; then
	echo "need -f path to specify where to save QC stats and logs ) "
	exit 2
fi

mkdir -p $OUTPUT/QC 
mkdir -p $OUTPUT/nparray
mkdir -p $OUTPUT/np_logs


# Convert niftis to nparrays 

for i in $(seq 1 188);
do
    python3 nii2np.py -i $IMAGE -o $OUTPUT/nparray -atlas ./brain_vbm_atlas.nii.gz -logs $OUTPUT/np_logs -code $i
done

# Do QCs

# QC with mode region: Threshold recommended to be 10

for i in $(seq 1 188);
do
    python3 QC_vbm_reg.py  -mode region -i $OUTPUT/nparray -o $OUTPUT/QC -logs $OUTPUT/np_logs -code $i -q 10
done

# run QC with mode summary
python3 QC_vbm_reg.py  -mode summary -i $OUTPUT/nparray -o $OUTPUT/QC -logs $OUTPUT/np_logs -q 10
