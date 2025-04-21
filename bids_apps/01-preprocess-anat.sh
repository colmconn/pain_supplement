#!/bin/bash

set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task=smriprep

programName=`basename $0`

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
RAW_DATA=$ROOT/rawdata
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
CODE_DIR=${ROOT}/code

source ${HOME}/envs/smriprep-docker/bin/activate

if ! command -v smriprep-docker &> /dev/null
then
    error_message_ln "The smriprep-docker command could not be found."
    error_message_ln "Install it with pip install smriprep-docker."    
    exit
fi

GETOPT_OPTIONS=$( $GETOPT  -o "s:" --longoptions "subject:" -n ${programName} -- "$@" )
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
	--) 
	    shift ; break ;;

	*) 
	    error_message_ln "${programName}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

# if [ -z ${subject} ] ; then 
#     error_message_ln "ERROR: The subject ID was not provided."
#     exit
# fi

if [[ ${subject:0:4} != "sub-" ]] ; then
    subject="sub-${subject}"
fi

# smriprep-docker  \
#     --fs-license-file ${HOME}/license.txt \
#     ${SOURCE_DATA} ${DERIVATIVE_DATA}/${task} participant

docker run --rm -e DOCKER_VERSION_8395080871=26.1.3 \
       -it \
       --cpus 12 \
       --memory  32GB \
       -v /home/colmconn/Applications/freesurfer-7.4/.license:/opt/freesurfer/license.txt:ro \
       -v /data/colmconn/pain_supplement/sourcedata:/data:ro \
       -v /data/colmconn/pain_supplement/derivative/smriprep:/out \
       -v ${CODE_DIR}/bids_apps/smriprep-bids-filter.json:/tmp/smriprep-bids-filter.json \
       nipreps/smriprep:0.15.0 \
       --participant-label sub-162 sub-177 \
       --bids-filter-file /tmp/smriprep-bids-filter.json \
       --longitudinal \
       /data /out participant
