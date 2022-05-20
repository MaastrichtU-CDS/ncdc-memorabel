#!/usr/bin/env bash

while getopts ":i:n:" opt; do
  case $opt in
    i) IMAGE_PATH=$OPTARG ;;
    # f) FREESURFER=$OPTARG ;;
    # o) SAVE_PATH=$OPTARG;;
    n) ID=$OPTARG;;
    # v) VBM_DIR=$OPTARG;;
    # t) TEMPLATE=$OPTARG;;
    # c) CONFIG=$OPTARG;;
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


# if [ -z "$IMAGE_PATH" ]; then
# 	echo " need -i full path to freesurfer  aseg.mgz image"
# 	exit 2
# fi
# if [ -z "$FREESURFER" ]; then
# 	echo "need -f path to FreeSurfer ( example: /cm/shared/apps/freesurfer/5.1.0/ ) "
# 	exit 2
# fi
# if [ -z "$SAVE_PATH" ]; then
# 	echo "need -o output folder"
# 	exit 2
# fi
# if [ -z "$VBM_DIR" ]; then
# 	echo "need -v path to VBM scripts and templates folder"
# 	exit 2
# fi
if [ -z "$ID" ]; then
	echo "need -n uniq name for image"
	exit 2
fi
# if [ -z "$CONFIG" ]; then
# 	echo "need -c full path to fsl config file for registration"
# 	exit 2
# fi

NAME=`basename $IMAGE_PATH`
NAME=`echo "$NAME" | cut -d'.' -f1`

NAME=${ID}_${NAME}

SAVE_RESULT="${SAVE_PATH}/${ID}"
mkdir ${SAVE_RESULT}
touch ${SAVE_RESULT}/start.txt

### FreeSurfer function to convert mgz to nifti
echo "Converting the segmentation format"
/utils/mri_convert --in_type mgz --out_type nii --out_orientation LAS ${IMAGE_PATH} ${SAVE_RESULT}/${NAME}.nii

### python script to extract grey matter regions from segmentation image
echo "Extracting the grey matter regions"
python ${VBM_DIR}/tissue_segmentation.py -o ${SAVE_RESULT}  -s ${SAVE_RESULT}/${NAME}.nii -type GM -info freesurfer

### remove converted nifti image
rm ${SAVE_RESULT}/${NAME}.nii

cd ${SAVE_RESULT}

### FSL registration to template + save Jacobian of transformation field
echo "FSL registration to template"
#${FSLDIR}/bin/fsl_reg GM_${NAME} ${NAME}_template.nii ${NAME}_GM_to_template_GM -fnirt "--refmask=${VBM_DIR}/brain_mask_1mm.nii.gz --config=${CONFIG} --jout=${NAME}_JAC_nl"
${FSLDIR}/bin/fsl_reg GM_${NAME} ${VBM_DIR}/mni_icbm152_gm_kNN.nii ${NAME}_GM_to_template_GM -fnirt "--refmask=${VBM_DIR}/brain_mask_1mm.nii.gz --config=${CONFIG} --jout=${NAME}_JAC_nl"

## remove GM mask image
rm ${SAVE_RESULT}/GM_${NAME}.nii

### Apply modulation on transformed image (multiply voxel values by log(JAC))
${FSLDIR}/bin/fslmaths ${NAME}_GM_to_template_GM -mul ${NAME}_JAC_nl ${NAME}_GM_to_template_GM_mod -odt float

#rm ${SAVE_RESULT}/${NAME}_JAC_nl.nii.gz

# Clean all files except the result
find . -type f -name "${NAME}*" -not -name "${NAME}_GM_to_template_GM_mod.nii.gz" -not -name "${NAME}_aseg_start.txt" -delete

# Unecessary for this purpose
### Smooth final image by kernel -s "x" (mm)
# ${FSLDIR}/bin/fslmaths ${NAME}_GM_to_template_GM_mod -s 3  ${NAME}_GM_to_template_GM_mod_s3

### remove images which we do not need for VBM. Comment lines below if you want to keep them
#rm ${SAVE_RESULT}/${NAME}_GM_to_template_GM.nii.gz
#rm ${SAVE_RESULT}/${NAME}_GM_to_template_GM_warp.nii.gz
#rm ${SAVE_RESULT}/${NAME}_JAC_nl.nii.gz
