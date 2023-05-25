#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"
ttests_task="${task}-ttests"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
TASK_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${ttests_task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}
[[ ! -d ${PIPELINE_DIR} ]] && mkdir -p ${PIPELINE_DIR}

btemplate=MNI152_2009_template_SSW.nii.gz    
tpath=$( @FindAfniDsetPath ${btemplate} )
if [[ ${tpath} == "" ]] ; then 
   echo "*** Couldn't find path to MNI152_2009_template_SSW.nii.gz template."
   echo "*** Exiting"
   exit 1
fi

subjects=( $( cd ${SOURCE_DATA} ; find  ./ -maxdepth 1 -name 'sub-[0-9][0-9][0-9]' | sed 's#\./##g' | sort ) )
if [[ -z ${subjects} ]] ; then
    error_message_ln "Found no subject directories in ${SOURCE_DATA}"
    error_message_ln "Quitting"
    exit
fi

outputDir=$( cd ${TASK_PIPELINE_DIR}/${subject[0]}; find ./ -type d -name ${task}-preprocessed-* | head -1 )
outputDir=${outputDir##*/}
## echo ${outputDir}

## sets is an associative array
declare -A sets
sets[baseline]=""
sets[followup]=""
for subject in ${subjects[*]} ; do
    if [[ -f ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/stats.${subject}_ses-baseline_REML+tlrc.HEAD ]] && \
       [[ -f ${TASK_PIPELINE_DIR}/${subject}/ses-followup/${outputDir}/stats.${subject}_ses-followup_REML+tlrc.HEAD ]] ; then
	if [[ ! -f  ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/00_DO_NOT_ANALYSE_${subject}_ses-baseline_20percent.txt  ]] && 
	   [[ ! -f  ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/00_DO_NOT_ANALYSE_${subject}_ses-followup_20percent.txt  ]] ; then 
	    sets[baseline]="${sets[baseline]} ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/stats.${subject}_ses-baseline_REML+tlrc.HEAD"
	    sets[followup]="${sets[followup]} ${TASK_PIPELINE_DIR}/${subject}/ses-followup/${outputDir}/stats.${subject}_ses-followup_REML+tlrc.HEAD"
	fi
    fi
done

# info_message_ln "Baseline set n: $( echo ${sets[baseline]} | wc -w)"
# info_message_ln "Followup set n: $( echo ${sets[followup]} | wc -w)"

(( ii=1 ))
info_message_ln "[SS]: Baseline Subject File"
for sfile in ${sets[baseline]} ; do
    msg="$( printf '[%02d] %s' ${ii} ${sfile} )"
    info_message_ln "${msg}"
    (( ii=ii+1 ))
done
(( ii=1 ))
info_message_ln "[SS]: Followup Subject File"
for sfile in ${sets[followup]} ; do
    msg="$( printf '[%02d] %s' ${ii} ${sfile} )"
    info_message_ln "${msg}"
    (( ii=ii+1 ))
done

# info_message_ln "Baseline set: ${sets[baseline]}"
# info_message_ln "Followup set: ${sets[followup]}"

cd ${PIPELINE_DIR}
ln -sf $tpath/$btemplate .
first_stats_file=$( echo ${sets[baseline]} | awk '{print $1}' )
3dbucket -prefix Bmask $tpath/$btemplate'[Bmask]' $tpath/$btemplate 
3dresample -rmode NN -master ${first_stats_file} -input Bmask+tlrc -prefix Bmask_epi+tlrc 
export export OMP_NUM_THREADS=8
3dttest++ -setA ${sets[baseline]} \
	  -setB ${sets[followup]} \
	  -labelA baseline \
	  -labelB followup \
	  -paired \
	  -mask Bmask_epi+tlrc.HEAD \
	  -prefix ${ttests_task} \
	  -Clustsim
	  
