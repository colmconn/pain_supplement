#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"
lmes_task="${task}-lmes-mt0.35-ex0.30"

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
3dbucket -prefix Bmask $tpath/$btemplate'[Bmask]'
3dresample -rmode NN -master ${first_stats_file} -input Bmask+tlrc -prefix Bmask_epi+tlrc 
export OMP_NUM_THREADS=8

3dLMEr \
    -prefix session_by_intervention \
    -mask Bmask_epi+tlrc.HEAD \
    -jobs ${OMP_NUM_THREADS} \
    -model 'session*intervention+bmi+Index.knee+(1|site/Subj)' \
    -resid session_by_intervention_residuals  \
    -qVars bmi \
    -gltCode sham-interv           'intervention : 1*sham -0.33*tDCS -0.33*meditation -0.33*experimental' \
    -gltCode sham-tDCS             'intervention : 1*sham -1*tDCS' \
    -gltCode sham-meditationOnly   'intervention : 1*sham -1*meditation' \
    -gltCode sham-experimental     'intervention : 1*sham -1*experimental' \
    -gltCode sham-meditation       'intervention : 1*sham -0.50*meditation -0.50*experimental' \
    -gltCode sham-tDCS             'intervention : 1*sham -0.50*tDCS -0.50*experimental' \
    -gltCode knee                  'Index.knee   : 1*L -1*R' \
    -gltCode base-foll             'session : 1*ses-baseline -1*ses-followup' \
    -dataTable @lme_data_table.tsv
    
    
    
