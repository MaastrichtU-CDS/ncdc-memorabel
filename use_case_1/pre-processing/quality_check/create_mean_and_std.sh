#!/bin/bash


# module load fsl/6.0.4

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
	echo " need -i path to vbm nifti results"
	exit 2
fi
if [ -z "$OUTPUT" ]; then
	echo "need -o path to save map"
	exit 2
fi



fslmerge -t $OUTPUT/4D_merge.nii.gz $IMAGE/*

fslmaths $OUTPUT/4D_merge.nii.gz -Tmean $OUTPUT/mean.nii.gz

fslmaths $OUTPUT/4D_merge.nii.gz -Tstd $OUTPUT/std.nii.gz
