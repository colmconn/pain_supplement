#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="resting-state"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
ANAT_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code
regressorDir=${CODE_DIR}/regressors

tappingDuration=20 # seconds of knee tapping
restDuration=30 # seconds of rest between knee tapping blocks
blockDuration=$(( tappingDuration + restDuration ))
nBlocks=6 # number of blocks of knee tapping + rest
nRuns=2

if [[ ! -d ${CODE_DIR}/regressors/ ]] ; then
    mkdir -p ${CODE_DIR}/regressors/
fi

cat /dev/null > ${CODE_DIR}/regressors/task-tapping-married.1D
for rr in $( seq 1 ${nRuns} ) ; do
    for bb in $( seq 0 $(( nBlocks - 1 )) ) ; do
	start=$(( 60 + blockDuration*bb))
	## end=$(( start + tappingDuration -1 ))
	## echo -n "${start}:${end} "
	echo -n "${start}:${tappingDuration} " >> ${CODE_DIR}/regressors/task-tapping-married.1D
    done
    echo "" >> ${CODE_DIR}/regressors/task-tapping-married.1D
done
