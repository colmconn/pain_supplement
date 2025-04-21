#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"
lmes_task="${task}-lmes-mt0.25-ex0.30"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivatives
TASK_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${lmes_task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}
[[ ! -d ${PIPELINE_DIR} ]] && mkdir -p ${PIPELINE_DIR}

if [[ ! -f ${PIPELINE_DIR}/lme_data_table.tsv ]] ; then
    error_message_ln "LME data table file does not exist: ${PIPELINE_DIR}/lme_data_table.tsv"
    error_message_ln "Run read-motion-qa.data.r to create it"
    exit
fi

btemplate=MNI152_2009_template_SSW.nii.gz    
tpath=$( @FindAfniDsetPath ${btemplate} )
if [[ ${tpath} == "" ]] ; then 
   error_message_ln "Couldn't find path to MNI152_2009_template_SSW.nii.gz template."
   error_message_ln "Exiting"
   exit 1
fi

cd ${PIPELINE_DIR}
ln -sf $tpath/$btemplate .
first_stats_file=$( head -2 lme_data_table.tsv  | tail -1 | awk '{print $NF}' | awk -F"'" '{print $1}' )
3dbucket -prefix Bmask $tpath/$btemplate'[Bmask]' $tpath/$btemplate 
3dresample -rmode NN -master ${first_stats_file} -input Bmask+tlrc -prefix Bmask_epi+tlrc 
export OMP_NUM_THREADS=8

3dLME \
    -prefix session_by_intervention \
    -jobs ${OMP_NUM_THREADS} \
    -model 'session*intervention' \
    -ranEff '~1' \
    -SS_type 3 \
    -num_glt 7 \
    -gltLabel 1 'sham-interv'     -gltCode 1 'intervention : 1*Sham -0.33*tDCS.only -0.33*Meditation.only -0.33*tDCS.and.Meditation' \
    -gltLabel 2 'sham-tDCSOnly'   -gltCode 2 'intervention : 1*Sham -1*tDCS.only' \
    -gltLabel 3 'sham-MedOnly'    -gltCode 3 'intervention : 1*Sham -1*Meditation.only' \
    -gltLabel 4 'sham-tDCSAndMed' -gltCode 4 'intervention : 1*Sham -1*tDCS.and.Meditation' \
    -gltLabel 5 'sham-Med'        -gltCode 5 'intervention : 1*Sham -0.50*Meditation.only -0.50*tDCS.and.Meditation' \
    -gltLabel 6 'sham-tDCS'       -gltCode 6 'intervention : 1*Sham -0.50*tDCS.only -0.50*tDCS.and.Meditation' \
    -gltLabel 7 'base-foll'       -gltCode 7 'session : 1*ses-baseline -1*ses-followup' \
    -dataTable @lme_data_table.tsv
    
    
    
