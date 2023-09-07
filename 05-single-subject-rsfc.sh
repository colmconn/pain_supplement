#!/bin/bash

# set -x

#exit immediately on an error
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
PREPROC_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}-corr
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}

#this is for the command line options; on mac would have to have gnu get opt installed; probably in macports, definitely in homebrew"
GETOPT_OPTIONS=$( $GETOPT \
		      -o "l:" \
		      --longoptions "seedlist:,session:,subject:" \
		      -n ${PROGRAM_NAME} -- "$@" )
#exitStatus of getopt, getopt returns zero if had no parsing errors on
#command line
exitStatus=$?
if [ $exitStatus != 0 ] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

#
# Note the quotes around `$GETOPT_OPTIONS': they are essential!
#
# shift because command line arguments. shift command tells it to pop
# them off the top so that don't get processed again.  shift 2 takes
# off "-m .2"
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-l|--seedlist)
	    seedList=$2; shift 2 ;;
	--session)
	    session=$2; shift 2 ;;
	--subject)
	    subject=$2; shift 2 ;;	
	--) 
	    shift ; break ;;
	
	*) 
	    error_message_ln "${PROGRAM_NAME}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

if [ -z $subject ] ; then 
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

if [[ ${subject:0:4} != "sub-" ]] ; then
    subject="sub-${subject}"
fi

if [ ! -f $seedList ] ; then
    error_message "ERROR: The seed list file does not exit. Exiting"
    exit
else 
    seeds=$( eval echo $( cat $seedList ) )
fi

info_message_ln "Computing RSFC for the following seeds:"
info_message_ln "${seeds}"

ss=${subject}/${session}

preprocessedRsfcDir=${PREPROC_PIPELINE_DIR}/${ss}/${task}-preprocessed-polortA-NL

if [[ -f ${preprocessedRsfcDir}/errts.${subject}_${session}_REML+tlrc.HEAD ]] ; then
    if [[ ! -d ${PIPELINE_DIR}/${ss} ]] ; then
	mkdir -p ${PIPELINE_DIR}/${ss}/
    fi
    
    cd ${PIPELINE_DIR}/${ss}/
    
    for seed in ${seeds} ; do

        seedName=${seed##*/}
        if echo ${seedName} | grep -q "nii" ; then 
            seedName=${seedName%%.nii*}
        else 
            seedName=${seedName%%+*}
        fi

        if [[ ! -d ${seedName} ]] ; then
            mkdir ${seedName}
        fi

        info_message_ln "Extracting timeseries for seed ${seed}"
        3dROIstats -quiet -mask_f2short -mask ${seed} \
		   ${preprocessedRsfcDir}/errts.${subject}_${session}_REML+tlrc.HEAD > ${seedName}/${seedName}.ts.1D

        info_message_ln "Computing Fisher Z-transformed Pearson Correlation for seed ${seedName}"
        3dTcorr1D -pearson -Fisher \
		  -mask ${preprocessedRsfcDir}/mask_group+tlrc.HEAD \
		  -prefix ${seedName}/${seedName}.z-score \
		  ${preprocessedRsfcDir}/errts.${subject}_${session}_REML+tlrc.HEAD \
		  ${seedName}/${seedName}.ts.1D

        3drefit -sublabel 0 ${subject}_${session} ${seedName}/${seedName}.z-score+tlrc.HEAD
    done
fi
