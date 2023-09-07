#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"
ttests_task="${task}-ttests"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${MDD_ROOT:-/data/colmconn/${studyName}}

DDATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
TASK_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${ttests_task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

##GROUP_DATA=$DATA/Group.data
##GROUP_RESULTS=$DATA/Group.results
MDD_STANDARD=$ROOT/standard
MDD_TISSUEPRIORS=$ROOT/tissuepriors
scriptsDir=${ROOT}/scripts

info_message_ln "Will use group results files in ${PIPELINE_DIR}"

tLabelPrefix="baseline-followup"
suffix=${tLabelPrefix}

cd ${PIPELINE_DIR}

if [[ -f clusters.order.${suffix}+tlrc.HEAD ]] ; then
    
    roi_range=( $( 3dBrickStat -slow -min -max clusters.order.baseline-followup+tlrc.HEAD ) )
    info_message_ln "Cluster order range: ${roi_range[*]}"
    if [[ ${roi_range[1]} -gt ${roi_range[0]} ]] ; then
	(( roi_range[0] = roi_range[0] + 1 ))
	for (( rr=${roi_range[0]}; rr <= ${roi_range[1]}; rr=rr+1 )) ; do
	    ## pp = pretty printed
	    rr_pp=$( printf "%02d" ${rr} )
	    info_message_ln "Extracting ROI: ${rr_pp}"
	    3dcalc -a clusters.order.baseline-followup+tlrc.HEAD -expr "equals(a, ${rr})" -prefix roi-${rr_pp}
	done
    else
	info_message_ln "Got no clusters :-("
    fi
    
fi

