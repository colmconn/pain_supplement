#!/bin/bash

# set -x

#exit immediatly on an error
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
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code
regressorDir=${CODE_DIR}/regressors

tappingDuration=20 # seconds of knee tapping
restDuration=30 # seconds of rest between knee tapping blocks
blockDuration=$(( tappingDuration + restDuration ))
nBlocks=6 # number of blocks of knee tapping + rest
nRuns=2


function make_bids_events_tsv {
    local subject=$1
    local session=$2
    local task=$3
    local run=$4
    local afni_regressor=$5
    
    tsv_filename=${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-${task}_run-${run}_events.tsv
    json_filename=${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-${task}_run-${run}_events.json
    echo -e "onset\tduration\ttrial_type" > ${tsv_filename}
    tail -n${run} ${afni_regressor} |
	head -n1 |
	tr ' ' '\n' |
	sed -r '/^\s*$/d' |
	awk -F':' -v tt=tapping 'BEGIN { OFS = "\t"; } {print $1, $2, tt}' >> ${tsv_filename}

    cat <<EOF > ${json_filename}
{
    "trial_type": {
        "LongName": "Tapping",
        "Description": "When the knee was tapped during the experimental blocks",
        "Levels": {
            "tapping": "The knee is lightly tapped"
        }
    }
}
EOF

}

## the %P argument in printf prints the file's name with the name of
## the starting-point under which it was found removed.
## See https://man7.org/linux/man-pages/man1/find.1.html
subjects=$( cd ${SOURCE_DATA}; find ./ -regex '.*sub-10[0-9][0-9][0-9]/ses-[a-z]*' -printf "%P\n" )

if [[ ! -d ${CODE_DIR}/regressors/ ]] ; then
    mkdir -p ${CODE_DIR}/regressors/
fi

cat /dev/null > ${CODE_DIR}/regressors/task-tapping-married.1D
for rr in $( seq 1 ${nRuns} ) ; do
    for bb in $( seq 0 $(( nBlocks - 1 )) ) ; do
	start=$(( 60 + blockDuration*bb))
	## end=$(( start + tappingDuration -1 ))
	## echo -n "${start}:${end} "
	echo -n "${start}:${tappingDuration} " >> ${CODE_DIR}/regressors/task-tapping-married.1D
    done
    echo "" >> ${CODE_DIR}/regressors/task-tapping-married.1D
     for ss in ${subjects} ; do
	subject=${ss%%/*}
	session=${ss##*/}
	if [[ ${subject} =~ sub-10[0-9][0-9][0-9] ]] ; then
	    jsonFile=${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-tapping_run-${rr}_bold.json
	else
	    jsonFile=${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-tapping_run-${rr}_echo-1_bold.json
	fi
	if [[ -f ${jsonFile} ]] ; then
	    make_bids_events_tsv ${subject} ${session} "tapping" ${rr} ${CODE_DIR}/regressors/task-tapping-married.1D
	fi
     done
    
done
