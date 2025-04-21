#!/bin/bash

#set -x 

# if ctrl-c is typed exit immediatly
trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivatives
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code  

if ! command -v open_apqc.py &> /dev/null
then
    error_message_ln "The open_apqc.py command could not be found."
    error_message_ln "Install AFNI to make it available or make sure the AFNI directory is on yor path."    
    exit
fi

#this is for the command line options; on mac would have to have gnu get opt installed; probably in macports, definitely in homebrew"
GETOPT_OPTIONS=$( $GETOPT \
		      -o "t:d:" \
		      --longoptions "task:,directory:" \
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
	-t|--task)
	    task=$2; shift 2 ;;
	-d|--directory)
	    directory=$2; shift 2 ;;
	--) 
	    shift ; break ;;
	
	*) 
	    error_message_ln "${PROGRAM_NAME}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

if [ -z ${task} ] ; then 
    error_message_ln "ERROR: The task was not provided."
    exit
fi
 
# if [ -z ${directory} ] ; then 
#     error_message_ln "ERROR: The subject's analysis directory was not provided."
#     exit
# fi

if [[ ! -d ${DERIVATIVE_DATA}/functional-qc/afni-${task} ]] ; then
    mkdir -p ${DERIVATIVE_DATA}/functional-qc/afni-${task}
fi
cd ${DERIVATIVE_DATA}/functional-qc/afni-${task}

count=1
# qc_files=$( find ${DERIVATIVE_DATA}/afni-${task} -type f -name  index.html | grep -v test | grep -E "(sub-10198)|(sub-10208)|(sub-10214)" )
qc_files=$( find ${DERIVATIVE_DATA}/afni-${task} -type f -name  index.html | \
		grep -v test        | \
		grep -E "sub-10207" | \
		grep followup       | \
		grep task-tapping-preprocessed-polortA-NL )

ss_regex=".*/(sub-[0-9]{3,5})/(ses-([A-Za-z0-9])+)/.*"
for ff in ${qc_files} ; do
    if [[ ${ff} =~ ${ss_regex} ]] ; then
	echo $( printf "[%03d] | Subject: %20s Session: %s | %s"  ${count} ${BASH_REMATCH[1]} ${BASH_REMATCH[2]} "${ff}" )
    else
	echo $( printf "[%03d] | Subject: %20s Session: %s | %s " ${count} UNKNOWN UNKNOWN "${ff}" )
    fi
    (( count=count + 1))
done

open_apqc.py              \
    -infiles  ${qc_files}

# echo "####################################################################################################"
# echo "### All done!"
# echo "####################################################################################################"
