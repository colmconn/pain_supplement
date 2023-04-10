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

GETOPT_OPTIONS=$( $GETOPT  -o "e:s:" --longoptions "session::,subject::" -n ${programName} -- "$@" )
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

if [[ ${session:0:4} != "ses-" ]] ; then
    session="ses-${session}"
fi

if [[ ${subjectNumber:0:4} != "sub-" ]] ; then
    subjectNumber="sub-${subjectNumber}"
fi

export SUBJECTS_DIR=${DERIVATIVE_DATA}/freesurfer-7.0/

prefix=${subjectNumber}/${session}
ss=${subjectNumber}_${session}

cd ${SUBJECTS_DIR}

freeview -v \
	 ${ss}/mri/T1.mgz \
	 ${ss}/mri/wm.mgz \
	 ${ss}/mri/brainmask.mgz \
	 ${ss}/mri/aseg.mgz:colormap=lut:opacity=0.2 \
	 -f ${ss}/surf/lh.white:edgecolor=blue \
	 ${ss}/surf/lh.pial:edgecolor=red \
	 ${ss}/surf/rh.white:edgecolor=blue \
	 ${ss}/surf/rh.pial:edgecolor=red
