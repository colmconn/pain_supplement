#!/bin/bash

# set -x

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
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
CODE_DIR=${ROOT}/code

if ! command -v dcm2bids &> /dev/null
then
    error_message_ln "The dcm2bids command could not be found."
    error_message_ln "Install it with pip install dcm2bids."    
    exit
fi

GETOPT_OPTIONS=$( $GETOPT  -o "d:e:s:" --longoptions "ddp:,session:,subject:" -n ${programName} -- "$@" )
exitStatus=$?
if [ $exitStatus != 0 ] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

# Note the quotes around `$GETOPT_OPTIONS': they are essential!
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-s|--subject)
	    subjectNumber=$2; shift 2 ;;
	-e|--session)
	    session=$2; shift 2 ;;
	-d|--ddp)
	    dicom_dir_prefix=$2; shift 2 ;;
	--) 
	    shift ; break ;;

	*) 
	    error_message_ln "${programName}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

if [ -z $subjectNumber ] ; then 
    error_message_ln "ERROR: The subject ID was not provided."
    exit
fi

if [ -z $session ] ; then 
    error_message_ln "ERROR: The subject's session was not provided."
    exit
fi

if [ -z ${ddp} ] ; then
    error_message_ln "ERROR: The DICOM directory prefix (DDP), that is the start of the directory name containing the DICOMS was not provided."
    exit
fi

if [[ ${session:0:4} != "ses-" ]] ; then
    session="ses-${session}"
fi

if [[ ${subjectNumber:0:4} != "sub-" ]] ; then
    subjectNumber="sub-${subjectNumber}"
fi

dcm2bids \
    --dicom_dir ${RAW_DATA}/${subject}/${session}/${ddp}* \
    --participant ${subject} \
    --session $session} \
    --output_dir ${SOURCE_DATA} \
    --config ${CODE_DIR}/config_pain_supplement.json
