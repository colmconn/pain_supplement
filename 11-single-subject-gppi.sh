#!/bin/bash

# set -x

#exit immediately on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
ANAT_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
TTEST_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}-ttests
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

ss=${subject}/${session}
preprocessedTaskDir=${PIPELINE_DIR}/${ss}/${task}-preprocessed-polortA-NL

nruns=2

conditions="tapping"
btemplate=MNI152_2009_template_SSW.nii.gz    

tpath=$( @FindAfniDsetPath ${btemplate} )
if [[ ${tpath} == "" ]] ; then 
   echo "*** Couldn't find path to MNI152_2009_template_SSW.nii.gz template."
   echo "*** Exiting"
   exit 1
fi

motionThreshold=0.25
outlierThreshold=0.2
scriptExt="polortA-NL"
outputDir=${task}-preprocessed-${scriptExt}
preprocessingScript=${subject}_${session}_preprocess-task-tapping-polortA-NL.csh
regressorDir=${CODE_DIR}/regressors

if [[ -f ${preprocessedTaskDir}/pb04.${subject}_${session}.r01.combine+tlrc.HEAD ]] ; then
    if [[ ! -d ${PIPELINE_DIR}/${ss} ]] ; then
	mkdir -p ${PIPELINE_DIR}/${ss}/
    fi

    cd ${PIPELINE_DIR}/${ss}/

    seeds=$( ls ${TTEST_PIPELINE_DIR}/roi*+tlrc.HEAD )
    if [[ -z ${seeds} ]] ; then
	error_message_ln "Found no seeds in ${TTEST_PIPELINE_DIR}"
	exit
    fi
    
    ((seedCount=1 ))
    for seed in ${seeds} ; do
	# seedName=${seed##*/}
        # if echo ${seedName} | grep -q "nii" ; then 
	#     seedName=${seedName%%.nii*}
        # else 
	#     seedName=${seedName%%+*}
        # fi
	
        # if [[ ! -d ${seedName} ]] ; then
	#     mkdir ${seedName}
        # fi
	sc_pp=$( printf "%02d" ${seedCount} )
	seedName=$( cat ${TTEST_PIPELINE_DIR}/clusters.locations.baseline-followup.csv | \
			awk -F"," -v rr=${seedCount} '{print $rr}' | \
			tr -d '()' )

	msg=$( printf "Extracting timeseries for seed %s" ${seedName} )
	info_message_ln "${msg}"
	
	3dmaskave -mask ${seed} -quiet ${preprocessedTaskDir}/pb06.${subject}_${session}.r01.scale+tlrc.HEAD > count-${sc_pp}_run-01_seed-${seedName}.1D
	3dmaskave -mask ${seed} -quiet ${preprocessedTaskDir}/pb06.${subject}_${session}.r02.scale+tlrc.HEAD > count-${sc_pp}_run-02_seed-${seedName}.1D
	cat count-${sc_pp}_run-01_seed-${seedName}.1D count-${sc_pp}_run-02_seed-${seedName}.1D > count-${sc_pp}_seed-${seedName}.1D
	rm -f count-${sc_pp}_run-01_seed-${seedName}.1D count-${sc_pp}_run-02_seed-${seedName}.1D

	tcsh ${CODE_DIR}/ppi/cmd.ppi.2.make.regs count-${sc_pp}_seed-${seedName}.1D count-${sc_pp}_seed-${seedName}

	## copy the regressors into the directory where 3dD can find them
	cp ${regressorDir}/task-tapping.1D \
	   work.count-${sc_pp}_seed-${seedName}/p6.count-${sc_pp}_seed-${seedName}.01.tapping.rall.PPI.1D \
	   count-${sc_pp}_seed-${seedName}.1D \
	   ${preprocessedTaskDir}/stimuli

	sliceTimesFilename=$( ls ${PIPELINE_DIR}/${subject}/${session}/*.sliceTimes.txt )
	
	afni_proc.py -subj_id ${subject}_${session}									\
		     -write_3dD_script count-${sc_pp}_seed-${seedName}.proc.3dd.ppi                           \
		     -write_3dD_prefix count-${sc_pp}_seed-${seedName}.PPI.full.                                   \
		     -script ${preprocessingScript}								\
		     -out_dir ${outputDir}									\
		     -blocks despike tshift align tlrc volreg mask combine blur scale regress	       	\
		     -copy_anat ${DERIVATIVE_DATA}/afni-anat/${subject}/${session}/anatSS.${subject}_${session}.nii									\
		     -anat_follower     anat_w_skull anat ${DERIVATIVE_DATA}/afni-anat/${subject}/${session}/anatU.${subject}_${session}.nii  		      	\
		     -anat_follower_ROI aaseg        anat ${DERIVATIVE_DATA}/freesurfer-7.0/${subject}_${session}/SUMA/aparc.a2009s+aseg.nii			\
		     -anat_follower_ROI aeseg        epi  ${DERIVATIVE_DATA}/freesurfer-7.0/${subject}_${session}/SUMA/aparc.a2009s+aseg.nii			\
		     -dsets_me_echo  ${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-tapping_run-1_echo-1_bold.nii.gz ${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-tapping_run-2_echo-1_bold.nii.gz \
		     -dsets_me_echo  ${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-tapping_run-1_echo-2_bold.nii.gz ${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-tapping_run-2_echo-2_bold.nii.gz \
		     -dsets_me_echo  ${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-tapping_run-1_echo-3_bold.nii.gz ${SOURCE_DATA}/${subject}/${session}/func/${subject}_${session}_task-tapping_run-2_echo-3_bold.nii.gz 									\
		     -echo_times 13.000 26.6000 40.2000									\
		     -combine_method m_tedana_OC 								\
		     -combine_tedana_path ${HOME}/.local/bin/tedana 						\
		     -combine_opts_tedana --fittype curvefit 							\
		     -tshift_interp -wsinc9                                  					\
		     -volreg_warp_final_interp wsinc5								\
		     -tshift_opts_ts -tpattern @${sliceTimesFilename}						\
		     -tlrc_base ${tpath}/${btemplate}								\
		     -volreg_align_to first								\
		     -anat_has_skull no -volreg_align_e2a -align_opts_aea -giant_move -tlrc_NL_warp \
	    	     -tlrc_NL_warped_dsets ${DERIVATIVE_DATA}/afni-anat/${subject}/${session}/anatQQ.${subject}_${session}.nii \
		     ${DERIVATIVE_DATA}/afni-anat/${subject}/${session}/anatQQ.${subject}_${session}.aff12.1D \
		     ${DERIVATIVE_DATA}/afni-anat/${subject}/${session}/anatQQ.${subject}_${session}_WARP.nii -volreg_tlrc_warp -blur_size 4 -blur_to_fwhm -blur_opts_B2FW "-ACF"				\
		     -mask_epi_anat yes  -mask_apply group							\
		     -regress_reml_exec	 							\
		     -regress_3dD_stop										\
		     -regress_stim_times  ${regressorDir}/task-tapping.1D				\
		     -regress_stim_labels tapping								\
		     -regress_extra_stim_files \
		     	stimuli/work.count-${sc_pp}_seed-${seedName}/p6.count-${sc_pp}_seed-${seedName}.01.tapping.rall.PPI.1D \
		     	stimuli/count-${sc_pp}_seed-${seedName}.1D \
		     -regress_extra_stim_labels PPI.tapping PPI.seed         \
		     -regress_basis 'BLOCK(20, 1)'								\
		     -regress_opts_3dD -GOFORIT 99 								\
		     -regress_apply_mot_types demean deriv							\
		     -regress_censor_motion  $motionThreshold							\
		     -regress_censor_outliers $outlierThreshold						\
		     -regress_apply_mot_types demean deriv							\
		     -regress_est_blur_epits									\
		     -regress_est_blur_errts									\
		     -regress_run_clustsim no									\
		     -regress_opts_reml -GOFORIT 								\
		     -html_review_style pythonic	

	( cd ${outputDir}; \
	 tcsh  -xef ../count-${sc_pp}_seed-${seedName}.proc.3dd.ppi ; \
	 sh count-${sc_pp}_seed-${seedName}.REML_cmd -GOFORIT )

	(( seedCount=seedCount+1 ))
    done
fi
