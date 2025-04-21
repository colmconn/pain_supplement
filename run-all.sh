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
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

## the %P argument in printf prints the file's name with the name of
## the starting-point under which it was found removed.
## See https://man7.org/linux/man-pages/man1/find.1.html
## subjects=$( cd ${RAW_DATA}; find ./ -regex '.*sub-10[0-9][0-9][0-9]/ses-[a-z]*' -printf "%P\n" )

## subjects=$( cd ${RAW_DATA}; find ./ -regex '.*sub-[0-9][0-9][0-9]/ses-[a-z]*' -printf "%P\n" )
subjects="sub-10198/ses-baseline sub-10198/ses-followup sub-10208/ses-baseline sub-10208/ses-followup sub-10214/ses-baseline sub-10214/ses-followup"
## echo ${subjects}

# taskFile=$CODE_DIR/run/${task}.taskfile
# info_message_ln "List of tasks to be executed is stored in $taskFile"
# cat /dev/null > $taskFile

for ss in ${subjects} ; do
    subject=${ss%%/*}
    session=${ss##*/}
    # if [[ ${ss} == "sub-10186/ses-baseline" ]] ; then
    # 	warn_message_ln "Skipping sub-10186/ses-baseline"
    # 	continue
    # fi
    
    ## echo "${CODE_DIR}/01-convertDicoms.sh --subject ${subject} --session ${session} --ddp AHN_PAIN" >> ${taskFile}
    # echo "${CODE_DIR}/03-preprocess-anat-fsl.sh --subject ${subject} --session ${session} " >> ${taskFile}
    # ./01-convertDicoms.sh -s ${subject} -e ${session} -d AHN

    ##./03-preprocess-anat-afni.sh --subject=${subject} --session=${session} -h 2 -q
    ## ./02-run-freesurfer-cross.sh --subject=${subject} --session=${session} -h 2 -q
    ./04-preprocess-task-tapping-uofaz.sh --subject=${subject} --session=${session} -h 4 -n -b 4 -q
    ## ./04-preprocess-resting-state-uofaz.sh --subject=${subject} --session=${session} -h 4 -n -b 4 -q

    # ./04-preprocess-resting-state-fsu.sh --subject=${subject} --session=${session} -c 3 -h 4 -n -b 4 -q
    ## ./04-preprocess-task-tapping-fsu.sh  --subject=${subject} --session=${session} -c 3 -h 4 -n -b 4 -q
done
