#!/bin/bash

## set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
tasks="task-tapping resting-state"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
RAW_DATA=$ROOT/rawdata
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
ANAT_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}

subjects=( $( cd ${RAW_DATA} ; find  ./ -maxdepth 1 -name 'sub-[0-9][0-9][0-9]' | sort ) )
## subjects=( sub-105 )

progress_file=processing-progress-$( date +"%Y%m%d" ).csv
echo "subject,session,raw_dir,source_t1,source_t2,rest_echo1,rest_echo2,rest_echo3,task_run1_echo1,task_run1_echo2,task_run1_echo3,task_run2_echo1,task_run2_echo2,task_run2_echo3,freesurfer,afni_anat,afni_rest,afni_task,rest_mot_excld,task_mot_excld" > ${progress_file}

for ss in ${subjects[*]} ; do
    subject=${ss##./}
    for ses in baseline followup ; do
	raw_dir="FALSE"
	recon_t1="FALSE"
	recon_t2="FALSE"
	recon_task_run1=( "FALSE" "FALSE" "FALSE")
	recon_task_run2=( "FALSE" "FALSE" "FALSE")
	recon_rest=( "FALSE" "FALSE" "FALSE")
	
	freesurfer="FALSE"
	afni_anat="FALSE"
	afni_rest="FALSE"
	afni_task="FALSE"
	rest_mot_excld="FALSE"
	task_mot_excld="FALSE"
	
	session="ses-${ses}"
	ss=${subject}/${session}
	info_message_ln "${ss}"
	## check the raw directories exist
	if [[ -d ${RAW_DATA}/${ss} ]] ; then
	    raw_dir="TRUE"
	fi

	## check T1 and T2 anatomies exist
	if [[ -f ${SOURCE_DATA}/${ss}/anat/${subject}_${session}_rec-prescannorm_T1w.nii.gz ]] ; then
	    recon_t1="TRUE"
	fi
	if [[ -f ${SOURCE_DATA}/${ss}/anat/${subject}_${session}_rec-prescannorm_T2w.nii.gz ]] ; then
	    recon_t2="TRUE"
	fi

	# check if the rest echos exist
	for ee in 1 2 3 ; do
	    if [[ -f ${SOURCE_DATA}/${ss}/func/${subject}_${session}_task-rest_echo-${ee}_bold.nii.gz ]] ; then
		recon_rest[(( ${ee} - 1 ))]="TRUE"
	    fi
	done

	# check if the task echos exist
	rr=1
	for ee in 1 2 3 ; do
	    if [[ -f ${SOURCE_DATA}/${ss}/func/${subject}_${session}_task-tapping_run-${rr}_echo-${ee}_bold.nii.gz ]] ; then
		recon_task_run1[(( ${ee} - 1 ))]="TRUE"
	    fi
	done
	rr=2
	for ee in 1 2 3 ; do
	    if [[ -f ${SOURCE_DATA}/${ss}/func/${subject}_${session}_task-tapping_run-${rr}_echo-${ee}_bold.nii.gz ]] ; then
		recon_task_run2[(( ${ee} - 1 ))]="TRUE"
	    fi
	done

	# check is freesurfer directory exists
	if [[ -d ${DERIVATIVE_DATA}/freesurfer-7.0/${subject}_${session} ]] ; then
	    freesurfer="TRUE"
	fi


	# check if afni_anat output exists
	if [[ -f ${DERIVATIVE_DATA}/afni-anat/${ss}/anatQQ.${subject}_${session}.nii ]] ; then
	    afni_anat="TRUE"
	fi
	
	# check if afni_rest output exists
	if [[ -f ${DERIVATIVE_DATA}/afni-resting-state/${ss}/resting-state-preprocessed-polortA-NL/errts.${subject}_${session}_REML+tlrc.HEAD ]] ; then
	    afni_rest="TRUE"
	fi

	# check if afni_task output exists
	if [[ -f ${DERIVATIVE_DATA}/afni-task-tapping/${ss}/task-tapping-preprocessed-polortA-NL/stats.${subject}_${session}_REML+tlrc.HEAD ]] ; then
	    afni_task="TRUE"
	fi

	# check for too much motion in functional datasets
	if [[ -f ${DERIVATIVE_DATA}/afni-resting-state/${ss}/resting-state-preprocessed-polortA-NL/00_DO_NOT_ANALYSE_${subject}_${session}_20percent.txt ]] ; then
	    rest_mot_excld="TRUE"
	fi

	if [[ -f ${DERIVATIVE_DATA}/afni-task-tapping/${ss}/task-tapping-preprocessed-polortA-NL/00_DO_NOT_ANALYSE_${subject}_${session}_20percent.txt ]] ; then
	    task_mot_excld="TRUE"
	fi
	
	echo "${subject},${session},${raw_dir},${recon_t1},${recon_t2},$( printf "%s," ${recon_rest[*]} )$(printf "%s," ${recon_task_run1[*]})$(printf "%s," ${recon_task_run2[*]})${freesurfer},${afni_anat},${afni_rest},${afni_task},${rest_mot_excld},${task_mot_excld}" >> ${progress_file}

    done
done
