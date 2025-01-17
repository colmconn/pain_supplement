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
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
CODE_DIR=${ROOT}/code

source ${HOME}/envs/dcm2bids/bin/activate

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
	    subject=$2; shift 2 ;;
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

if [ -z ${subject} ] ; then 
    error_message_ln "ERROR: The subject ID was not provided."
    exit
fi

if [ -z ${session} ] ; then 
    error_message_ln "ERROR: The subject's session was not provided."
    exit
fi

if [ -z ${dicom_dir_prefix} ] ; then
    error_message_ln "ERROR: The DICOM directory prefix (DDP), that is the start of the directory name containing the DICOMS was not provided."
    exit
fi

if [[ ${session:0:4} != "ses-" ]] ; then
    session="ses-${session}"
fi

if [[ ${subject:0:4} != "sub-" ]] ; then
    subject="sub-${subject}"
fi

if [[ -f ${CODE_DIR}/${subject}_${session}_dcm2bids_config.json ]] ; then 
    dcm2bids_config_file=${CODE_DIR}/${subject}_${session}_dcm2bids_config.json
else
    if [[ ${subject} =~ sub-10[0-9][0-9][0-9] ]] ; then
	dcm2bids_config_file=${CODE_DIR}/dcm2bids_uofaz_config.json
    else
	dcm2bids_config_file=${CODE_DIR}/dcm2bids_fsu_config.json
    fi
fi
dcm2bids \
    --dicom_dir ${RAW_DATA}/${subject}/${session}/*${dicom_dir_prefix}* \
    --participant ${subject} \
    --session ${session} \
    --output_dir ${SOURCE_DATA} \
    --config ${dcm2bids_config_file} \
    --force_dcm2bids --clobber
