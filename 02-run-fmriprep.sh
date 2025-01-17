#!/bin/bash

## set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement

programName=`basename $0`

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
RAW_DATA=$ROOT/rawdata
SOURCE_DATA=$ROOT/fmriprep_test_sourcedata
DERIVATIVE_DATA=$ROOT/derivative
CODE_DIR=${ROOT}/code


docker run --cpus=8 -ti --rm \
       -v ${SOURCE_DATA}:/data:ro \
       -v ${DERIVATIVE_DATA}/fmriprep_test:/out \
       -v ${HOME}/Applications/freesurfer-7.4/.license:/opt/freesurfer/license.txt \
       -v ${CODE_DIR}/t1_bids_filter.json:/t1_bids_filter.json:ro \
       nipreps/fmriprep:latest \
       --skip_bids_validation \
       --bids-filter-file /t1_bids_filter.json \
       --nprocs 8 \
       --omp-nthreads 8 \
       --ignore fieldmaps \
       --output-spaces MNI152NLin2009cAsym \
       --longitudinal \
       --me-t2s-fit-method curvefit \
       --slice-time-ref 0 \
       --dummy-scans 0 \
       --cifti-output 91k \
       --notrack \
       /data /out participant
       
