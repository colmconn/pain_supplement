#!/bin/bash

# set -x

#exit immediately on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="afni-anat"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
RAW_DATA=$ROOT/rawdata
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivatives
SUBJECTS_DIR=$DERIVATIVE_DATA/freesurfer-7.0/
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

## the %P argument in printf prints the file's name with the name of
## the starting-point under which it was found removed.
## See https://man7.org/linux/man-pages/man1/find.1.html
subjects=$( cd ${RAW_DATA}; find ./ -regex '.*sub-10[0-9][0-9][0-9]/ses-[a-z]*' -printf "%P\n" )

## echo ${subjects}

# taskFile=$CODE_DIR/run/${task}.taskfile
# info_message_ln "List of tasks to be executed is stored in $taskFile"
# cat /dev/null > $taskFile

printf "%-20s%-20s%-10s\n" "subject" "session" "aseg?"
for ss in ${subjects} ; do
    subject=${ss%%/*}
    session=${ss##*/}
    if [[ ${ss} == "sub-10207/ses-followup" ]] ; then
	## warn_message_ln "Skipping sub-10207/ses-followup"
	continue
    fi
    s2=${subject}_${session}

    if [[ -f ${SUBJECTS_DIR}/${s2}/mri/aparc.a2009s+aseg.mgz ]] ; then
	printf "%-20s%-20s%-10s\n" ${subject} ${session} "YES"
    else
	printf "%-20s%-20s%-10s\n" ${subject} ${session} "NO"
    fi
done
