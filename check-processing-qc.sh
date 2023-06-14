#!/bin/bash

## set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"
##task="task-rest"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
RAW_DATA=$ROOT/rawdata
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
ANAT_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ -d ${LOG_DIR} ]] || mkdir -p ${LOG_DIR}

subjects=( $( cd ${RAW_DATA} ; find  ./ -maxdepth 1 -name 'sub-[0-9][0-9][0-9]' | sort ) )
## subjects=( sub-105 )

[[ ! -d ../log/processing_progress/ ]] || mkdir -p ../log/processing_progress/
progress_file=../log/processing_progress/${task}-processing-qc-$( date +"%Y%m%d" ).csv
echo "subject,session,vorig,ve2a,va2t,vstat,mot,regr,warns,qsum,FINAL" > ${progress_file}


for ss in ${subjects[*]} ; do
    subject=${ss##./}
    for ses in baseline followup ; do
	session="ses-${ses}"
	ss=${subject}/${session}

	## check the raw directories exist
	if [[ ! -d ${RAW_DATA}/${ss} ]] ; then
	    data="NA,NA,NA,NA,NA,NA,NA,NA,NA"
	else
	    data=",,,,,,,,"
	fi
	echo "${subject},${session},${data}" >> ${progress_file}

    done
done
