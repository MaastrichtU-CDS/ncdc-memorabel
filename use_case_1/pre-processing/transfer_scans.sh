#!/bin/bash

while getopts ":s:n:i:" opt; do
    case $opt in
    s) START=$OPTARG ;;
    n) NUMBER=$OPTARG ;;
    i) IMAGES_PATH=$OPTARG ;;
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

if [ -z "$IMAGES_PATH" ]; then
	echo "Missing mandatory argument: i) Images path "
        exit 2
fi
if [ -z "$START" ]; then
	echo "Starting point not provided, starting from the first scan available"
	START=0
fi
if [ -z "$NUMBER" ]; then
	echo "Number of scans to transfer not provided, transfering 100 scans "
	NUMBER=100
fi

NUMBER=$(($START+$NUMBER))
i=0
for d in "$IMAGES_PATH"/* ; do
    #echo "$d"
    if [ -d "$d/mri" -a -f "$d/mri/aseg.mgz" -a $i -gt $START -a $i -le $NUMBER ]; then
	    echo "$d"
	    INPUT_PATH="/mnt/data/input/`basename $d`"
	    oc exec preprocessing mkdir "$INPUT_PATH"
	    oc cp $d/mri/aseg.mgz preprocessing:$INPUT_PATH/aseg.mgz
    fi
    if [ $i -gt $NUMBER ]; then
	    break
    fi
    i=$((i+1))
done
echo "$i"