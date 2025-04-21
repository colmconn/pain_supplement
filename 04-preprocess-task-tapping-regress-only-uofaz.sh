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
DERIVATIVE_DATA=$ROOT/derivatives
ANAT_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}

## enqueue the job for execution ; 1 means queue, zero means do not
## queue; on server a queing system, can queu system for jobs to run
## in parallel, if leave it set as zero it will not, just create
## script.  If set to 1, will submit script to queuing system so that
## it will be 1.  if running multiple subjects, can tell it to submit
## it and queue will run them in order.  1 means "make the scripts and
## run in parallel"
enqueue=0

function combineExtensions () {
    echo $( echo $@ | sed 's/[[:space:]][[:space:]]*/-/g' )
}

#this is for the command line options; on mac would have to have gnu get opt installed; probably in macports, definitely in homebrew"
GETOPT_OPTIONS=$( $GETOPT \
		      -o "a:e:m:o:h:b:t:np:qx:" \
		      --longoptions "alignto:,excessiveMotionThresholdFraction:,motionThreshold:,outlierThreshold:,threads:,lowpass:,highpass:,blur:,tcat:,nonlinear,polort:,enqueue,extension:,subject:,session:" \
		      -n ${PROGRAM_NAME} -- "$@" )
#exitStatus of getopt, getopt returns zero if had no parsing errors on
#command line
exitStatus=$?
if [ $exitStatus != 0 ] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

## enqueue the job for execution ; 1 means queue, zero means do not
## queue; on server a queing system, can queu system for jobs to run
## in parallel, if leave it set as zero it will not, just create
## script.  If set to 1, will submit script to queuing system so that
## it will be 1.  if running multiple subjects, can tell it to submit
## it and queue will run them in order.  1 means "make the scripts and
## run in parallel"
enqueue=0

## extension to use for the analysis script and the directory in which
## analysis is stored
scriptExt=""

#
# Note the quotes around `$GETOPT_OPTIONS': they are essential!
#
# shift because command line arguments. shift command tells it to pop
# them off the top so that don't get processed again.  shift 2 takes
# off "-m .2"
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-a|--alignto)
	    alignto=$2; shift ;;
	-b|--blur)
	    blur=$2; shift 2 ;;
	-c|--nechos)
	    nechos=$2; shift 2 ;;
	-e|--excessiveMotionThresholdFraction)
	    excessiveMotionThresholdFraction=$2; shift 2 ;;	
	-h|--threads)
	    threads=$2; shift 2 ;;
	-m|--motionThreshold)
	    motionThreshold=$2; shift 2 ;;
	-n|--nonlinear)
	    nonlinear=1; shift 1 ;;	
	-o|--outlierThreshold)
	    outlierThreshold=$2; shift 2 ;;	
	--lowpas)
	    lowpass=$2; shift 2 ;;	
	--highpass)
	    highpass=$2; shift 2 ;;	
	-p|--polort)
	    polort="$2"; shift 1 ;;	
	-q|--enqueue)
	    enqueue=1; shift 1 ;;	
	-t|--tcat)
	    tcat=$2; shift 2 ;;	
	-x|--extension)
	    scriptExt="$2"; shift 2 ;;
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

####################################################################################################
## Check that appropriate values are used to initialize arguments that
## control analysis if no values were provided on the command line

if [[ "x${alignto}" == "x" ]] ; then
     alignto="first"
     warn_message_ln "No value for alignto, the volume to which other EPI volumes are aligned when 'correcting' for motion, was provided. Defaulting to ${alignto}"
else
    info_message_ln "Using alignto value of ${alignto}. If this is not a valid value supported by afni_proc.py, your script will fail"
fi

## The following values are used to exclude subjects based on the
## number of volumes censored during analysis
##if no argument is provided, x will equal x.  x${var} is the value of the variable. then there is a string concatenation.  so if excessiveMotion* has been defined, x will not equal x
## x will equal "x0.2" or something like it
if [[ "x$excessiveMotionThresholdFraction" == "x" ]] ; then
    excessiveMotionThresholdFraction=0.20
    excessiveMotionThresholdPercentage=20
    warn_message_ln "No excessiveMotionThresholdFraction threshold was provided. Defaulting to $excessiveMotionThresholdFraction => ${excessiveMotionThresholdPercentage}%"
else
## here's a fraction of 0.2, convert to a percentage and make sure it's a whole number because the two versions are used below, this is integer division b/c of the bc command
## so if fraction is 0.2, then it will take" (20 + .5) / 1" and return 20, will only return the whole number component which becomes a problem later
    excessiveMotionThresholdPercentage=$( echo "(($excessiveMotionThresholdFraction*100)+0.5)/1" | bc ) 
    
    info_message_ln "Using ${excessiveMotionThresholdFraction} as the subject exclusion motion cutoff fraction"
    info_message_ln "Using ${excessiveMotionThresholdPercentage}% as subject exclusion motion cutoff percentage"
    info_message_ln "Note that these values are used to exclude subjects based on the number of volumes censored during analysis"
fi

## motionThreshold and outlierThreshold are the values passed to
## afni_proc.py and are used when deciding to censor a volume or not
if [[ "x${motionThreshold}" == "x" ]] ; then
    motionThreshold=0.35
    warn_message_ln "No motionThreshold value was provided. Defaulting to $motionThreshold"
else
    info_message_ln "Using motionThreshold of ${motionThreshold}"
fi

if [[ "x${outlierThreshold}" == "x" ]] ; then
     outlierThreshold=0.2
     warn_message_ln "No outlierThreshold value was provided. Defaulting to $outlierThreshold"
else
    info_message_ln "Using outlierThreshold of ${outlierThreshold}"
fi

if [[ "x${threads}" == "x" ]] ; then
     threads=1
     warn_message_ln "No value for the number of parallel threads to use was provided. Defaulting to $threads"
else
    info_message_ln "Using threads value of ${threads}"
fi

# if [[ "x${lowpass}" == "x" ]] ; then
#      lowpass="0.01"
#      warn_message_ln "No value for lowpass filter value to use was provided. Defaulting to $lowpass"
# else
#     info_message_ln "Using lowpass filter value of ${lowpass}"
# fi

# if [[ "x${highpass}" == "x" ]] ; then
#      highpass="0.1"
#      warn_message_ln "No value for highpass filter value to use was provided. Defaulting to $highpass"
# else
#     info_message_ln "Using highpass filter value of ${highpass}"
# fi

if [[ "x${blur}" == "x" ]] ; then
     warn_message_ln "No value for blur filter value to use was provided. Blurring will be skipped"
     blurBlock=""
     afniProcBlurArgs=""
else
    info_message_ln "Using blur filter value of ${blur}"
    blurBlock="blur"    
    afniProcBlurArgs="-blur_size ${blur} -blur_to_fwhm -blur_opts_B2FW \"-ACF\""

fi

if [[ "x${polort}" == "x" ]] ; then
     polort="A"
     warn_message_ln "No value for polort, was provided. Letting 3dDeconvolve choose automatically"
else
    info_message_ln "Using polort value of ${polort}"
    afniProcPolortArg="-regress_polort ${polort}"
fi
parentScriptExt=$( combineExtensions ${scriptExt} "polort${polort}" )
scriptExt=$( combineExtensions ${scriptExt} "polort${polort}" "mt$( echo ${motionThreshold}| sed 's/[.]/_/' )" "regress-only" )


# -eq is equals, check for equality of numbers
if [[ $nonlinear -eq 1 ]] ; then 
    info_message_ln "Using nonlinear alignment"
    scriptExt=$( combineExtensions ${scriptExt} NL )
    parentScriptExt=$( combineExtensions ${parentScriptExt} "NL" )
else 
    info_message_ln "Using affine alignment only"
    scriptExt=$( combineExtensions ${scriptExt} AFF )
    parentScriptExt=$( combineExtensions ${parentScriptExt} "NL" )
fi

info_message_ln "Final script extensions: ${scriptExt}"

extraAlignmentArgs_orig="-volreg_align_e2a -align_opts_aea -giant_move" # -deoblique on -cost lpc+ZZ"
if [[ "${subject}" == "sub-10198" ]] ; then
    extraAlignmentArgs_orig="-volreg_align_e2a -align_opts_aea -cost hel"
fi
if [[ "${subject}" == "sub-10208" ]] ; then
    if [[ "${session}" == "ses-baseline" ]] ; then
	extraAlignmentArgs_orig="-volreg_align_e2a -align_opts_aea -cost lpa"	
    else
	extraAlignmentArgs_orig="-volreg_align_e2a -align_opts_aea -cost hel"	
     fi
fi
if [[ "${subject}" == "sub-10214" ]] ; then
    extraAlignmentArgs_orig="-volreg_align_e2a -align_opts_aea -cost hel"
fi

info_message_ln "Setting extraAlignmentArgs to: ${extraAlignmentArgs_orig}"

[[ -d run ]] || mkdir run

prefix=${subject}/${session}
ss=${subject}_${session}

unprocessedDataDir=${SOURCE_DATA}/${prefix}/func
SUMA_DIR=$DERIVATIVE_DATA/freesurfer-7.0/${ss}/SUMA
regressorDir=${CODE_DIR}/regressors

info_message_ln "#################################################################################################"
info_message_ln "Generating script for Subject: $subject Session: ${session}"
declare -a epiFiles
sourceEpiFiles=( $( ls -1 ${unprocessedDataDir}/${ss}_task-tapping_run-?_bold.nii.gz | sort -t '_' -k 4.5n ) )

info_message_ln "Got the following EPI files"
info_message_ln "[RR]: RR=run"
nfiles=${#sourceEpiFiles[@]}
for (( rr=0; rr < ${#sourceEpiFiles[@]}; rr++ )) ; do
    ## printf '[%02d]: %s\n' ${rr} "${epiFiles[$rr]}"
    msg=$( printf '[%02d]: %s\n' ${rr} "${sourceEpiFiles[${rr}]}" )
    info_message_ln "${msg}"
done

anatFile=${ANAT_PIPELINE_DIR}/${prefix}/anatSS.${ss}.nii
info_message_ln "Anatomy file: ${anatFile}"
if  [[ ! -f ${anatFile} ]] ; then
    error_message_ln "Can not find anatomy file for subject ${ss}. Skipping."
    exit 256
fi

outputScriptName=${CODE_DIR}/run/${ss}_preprocess-${task}-mt$( echo ${motionThreshold}| sed 's/[.]/_/' )-regress-only.sh
extraAlignmentArgs=${extraAlignmentArgs_orig}

## do non-linear warping? If so, add the flag to the extra
## alignment args variable
if [[ $nonlinear -eq 1 ]] ; then 
    extraAlignmentArgs="${extraAlignmentArgs} -tlrc_NL_warp"
    
	##
	## the following code is useful if you want to try to use a
	## preexisting nonlinear warped anatomy
	##
	if [[ -f ${ANAT_PIPELINE_DIR}/${prefix}/anatQQ.${ss}.nii ]] & \
	   [[ -f ${ANAT_PIPELINE_DIR}/${prefix}/anatQQ.${ss}.aff12.1D ]] & \
	   [[ -f ${ANAT_PIPELINE_DIR}/${prefix}/anatQQ.${ss}_WARP.nii ]] ; then 
	    
	    info_message_ln "Supplying prexisting nonlinear warped anatomy from @SSwarper to afni_proc.py"
	    extraAlignmentArgs="${extraAlignmentArgs} \\
	    	    	-tlrc_NL_warped_dsets ${ANAT_PIPELINE_DIR}/${prefix}/anatQQ.${ss}.nii \\
			${ANAT_PIPELINE_DIR}/${prefix}/anatQQ.${ss}.aff12.1D \\
			${ANAT_PIPELINE_DIR}/${prefix}/anatQQ.${ss}_WARP.nii"
	fi
	if [[ -f ${ANAT_PIPELINE_DIR}/${prefix}/anatSS.${ss}.nii ]] ; then
	    info_message_ln "Supplying prexisting skullstripped anatomy from @SSwarper to afni_proc.py"
	    anatFile=${ANAT_PIPELINE_DIR}/${prefix}/anatSS.${ss}.nii
	    extraAlignmentArgs="-anat_has_skull no ${extraAlignmentArgs}"
	fi
fi

info_message_ln "Writing: ${outputScriptName}"

if [[ ! -d ${PIPELINE_DIR}/ ]] ; then
    mkdir -p ${PIPELINE_DIR}/
fi

sliceTimesFilename=$( ls ${PIPELINE_DIR}/${prefix}/*.sliceTimes.txt )

#the next is a here document, will take anything between them but not include them and in this case into the outputScriptName
    cat <<EOF > $outputScriptName
#!/bin/bash

set -x 

#$ -S /bin/bash

## disable compression of BRIKs/nii files
unset AFNI_COMPRESSOR
## prevent the use of pigz
export AFNI_DONT_USE_PIGZ=YES

export PYTHONPATH=$AFNI_R_DIR

## use the newer faster despiking method. comment this out to get the
## old one back
export AFNI_3dDespike_NEW=YES

# turn off anoying colorization of info/warn/error messages since they
# only result in gobbledygook
export AFNI_MESSAGE_COLORIZE=NO

## only use a single thread since we're going to run so many subjects
## in parallel
export OMP_NUM_THREADS=${threads}

if [[ ! -d ${PIPELINE_DIR}/${prefix}/ ]] ; then
    mkdir -p ${PIPELINE_DIR}/${prefix}/
fi

cd ${PIPELINE_DIR}/${prefix}

outputDir=${task}-preprocessed-${scriptExt}
if [[ ! -d \${outputDir} ]] ; then
   mkdir \${outputDir}
else
   rm -fr \${outputDir}
   mkdir \${outputDir}
fi
parent_analysis=${task}-preprocessed-${parentScriptExt}

subj=${ss}

cd \${outputDir}

for ff in ../\${parent_analysis}/pb* \\
    ../\${parent_analysis}/anat* \\
    ../\${parent_analysis}/copy_af_* \\
    ../\${parent_analysis}/dfile_r* \\
    ../\${parent_analysis}/final_epi* \\
    ../\${parent_analysis}/follow_anat* \\
    ../\${parent_analysis}/follow_ROI* \\
    ../\${parent_analysis}/full_mask* \\
    ../\${parent_analysis}/mask_anat* \\
    ../\${parent_analysis}/mask_epi_anat* \\
    ../\${parent_analysis}/mask_epi_extents* \\
    ../\${parent_analysis}/mask_group* \\
    ../\${parent_analysis}/mat.* \\
    ../\${parent_analysis}/MNI152* \\
    ../\${parent_analysis}/out.4095* \\
    ../\${parent_analysis}/out.allcostX* \\
    ../\${parent_analysis}/outcount* \\
    ../\${parent_analysis}/out.mask_ae* \\
    ../\${parent_analysis}/out.mask_at* \\
    ../\${parent_analysis}/out.pre_ss_warn.txt \\
    ../\${parent_analysis}/out.vlines.pb00.tcat.txt \\
    ../\${parent_analysis}/volumized+tlrc.* \\
    ../\${parent_analysis}/vr_base* \\
    ../\${parent_analysis}/vline* ; do
    ln -sf \${ff} 
done 

mkdir stimuli
cd stimuli
for ff in ../../\${parent_analysis}/stimuli/* ; do
    ln -sf \${ff} 
done
cd ../

runs="\$( count_afni -digits 2 1 ${nfiles} )"

motionThreshold=${motionThreshold}
outlierThreshold=${outlierThreshold}

export AFNI_3dDeconvolve_GOFORIT=YES
# ================================ regress =================================

# compute de-meaned motion parameters (for use in regression)
1d_tool.py -infile dfile_rall.1D -set_nruns ${nfiles}               \\
           -demean -write motion_demean.1D

# compute motion parameter derivatives (for use in regression)
1d_tool.py -infile dfile_rall.1D -set_nruns ${nfiles}                \\
           -derivative -demean -write motion_deriv.1D

# create censor file motion_\${subj}_censor.1D, for censoring motion 
1d_tool.py -infile dfile_rall.1D -set_nruns ${nfiles}                \\
    -show_censor_count -censor_prev_TR                                   \\
    -censor_motion \${motionThreshold} motion_\${subj}

# combine multiple censor files
1deval -a motion_\${subj}_censor.1D -b outcount_\${subj}_censor.1D         \\
       -expr "a*b" > censor_\${subj}_combined_2.1D

# note TRs that were not censored
# (apply from a text file, in case of a lot of censoring)
1d_tool.py -infile censor_\${subj}_combined_2.1D                          \\
           -show_trs_uncensored space                                    \\
           > out.keep_trs_rall.txt
ktrs="1dcat out.keep_trs_rall.txt"

# ------------------------------
# run the regression analysis
3dDeconvolve -input pb05.\$subj.r*.scale+tlrc.HEAD                        \\
    -mask mask_group+tlrc                                                \\
    -censor censor_\${subj}_combined_2.1D                                 \\
    -ortvec motion_demean.1D mot_demean                                  \\
    -ortvec motion_deriv.1D mot_deriv                                    \\
    -polort 3                                                            \\
    -num_stimts 1                                                        \\
    -stim_times_AM1 1 stimuli/task-tapping-married.1D 'dmBLOCK(1)'       \\
    -stim_label 1 tapping                                                \\
    -GOFORIT 99                                                          \\
    -fout -tout -x1D X.xmat.1D -xjpeg X.jpg                              \\
    -x1D_uncensored X.nocensor.xmat.1D                                   \\
    -fitts fitts.\$subj                                                   \\
    -errts errts.\${subj}                                                 \\
    -x1D_stop                                                            \\
    -bucket stats.\$subj


# if 3dDeconvolve fails, terminate the script
if [[ \$? != 0 ]] ; then
    echo '---------------------------------------'
    echo '** 3dDeconvolve error, failing...'
    echo '   (consider the file 3dDeconvolve.err)'
    exit
fi


# display any large pairwise correlations from the X-matrix
1d_tool.py -show_cormat_warnings -infile X.xmat.1D |& tee out.cormat_warn.txt

# display degrees of freedom info from X-matrix
1d_tool.py -show_df_info -infile X.xmat.1D |& tee out.df_info.txt

# -- execute the 3dREMLfit script, written by 3dDeconvolve --
tcsh -x stats.REML_cmd -GOFORIT

# if 3dREMLfit fails, terminate the script
if [[ \$? != 0 ]] ; then
    echo '---------------------------------------'
    echo '** 3dREMLfit error, failing...'
    exit
fi


# create an all_runs dataset to match the fitts, errts, etc.
3dTcat -prefix all_runs.\$subj pb05.\$subj.r*.scale+tlrc.HEAD

# --------------------------------------------------
# create a temporal signal to noise ratio dataset 
#    signal: if 'scale' block, mean should be 100
#    noise : compute standard deviation of errts
3dTstat -mean -prefix rm.signal.all all_runs.\$subj+tlrc"[\$ktrs]"
3dTstat -stdev -prefix rm.noise.all errts.\${subj}_REML+tlrc"[\$ktrs]"
3dcalc -a rm.signal.all+tlrc                                             \\
       -b rm.noise.all+tlrc                                              \\
       -expr 'a/b' -prefix TSNR.\$subj


# --------------------------------------------------
# compute TSNR stats for dset labels: brain
compute_ROI_stats.tcsh                                                   \\
    -out_dir    tsnr_stats_regress                                       \\
    -stats_file tsnr_stats_regress/stats_auto_brain.txt                  \\
    -dset_ROI   mask_epi_anat.\$subj+tlrc                                 \\
    -dset_data  TSNR.\$subj+tlrc                                          \\
    -rset_label brain                                                    \\
    -rval_list  1

# ---------------------------------------------------
# compute and store GCOR (global correlation average)
# (sum of squares of global mean of unit errts)
3dTnorm -norm2 -prefix rm.errts.unit errts.\${subj}_REML+tlrc
3dmaskave -quiet -mask mask_epi_anat.\$subj+tlrc                          \\
          rm.errts.unit+tlrc > mean.errts.unit.1D
3dTstat -sos -prefix - mean.errts.unit.1D\' > out.gcor.1D
echo "-- GCOR = \$( cat out.gcor.1D)"

# ---------------------------------------------------
# compute correlation volume
# (per voxel: correlation with masked brain average)
3dmaskave -quiet -mask mask_epi_anat.\$subj+tlrc                          \\
          errts.\${subj}_REML+tlrc > mean.errts.1D
3dTcorr1D -prefix corr_brain errts.\${subj}_REML+tlrc mean.errts.1D

# create ideal files for fixed response stim types
1dcat X.nocensor.xmat.1D'[8]' > ideal_tapping.1D

# --------------------------------------------------
# extract non-baseline regressors from the X-matrix,
# then compute their sum
1d_tool.py -infile X.nocensor.xmat.1D -write_xstim X.stim.xmat.1D
3dTstat -sum -prefix sum_ideal.1D X.stim.xmat.1D

# ============================ blur estimation =============================
# compute blur estimates
touch blur_est.\$subj.1D   # start with empty file

# create directory for ACF curve files
mkdir files_ACF

# -- estimate blur for each run in epits --
touch blur.epits.1D

# restrict to uncensored TRs, per run
for run in \$runs  ; do
    trs=\$( 1d_tool.py -infile X.xmat.1D -show_trs_uncensored encoded \\
                          -show_trs_run \$run )
    if [[ \$trs == "" ]] ; then
       continue
    fi
    3dFWHMx -detrend -mask mask_group+tlrc                               \\
            -ACF files_ACF/out.3dFWHMx.ACF.epits.r\$run.1D               \\
            all_runs.\$subj+tlrc"[\$trs]" >> blur.epits.1D
done

# compute average FWHM blur (from every other row) and append
blurs=( \$( 3dTstat -mean -prefix - blur.epits.1D'{0..\$(2)}'\' 2> /dev/null ) )
echo average epits FWHM blurs: \$blurs
echo "\${blurs[@]}   # epits FWHM blur estimates" >> blur_est.\$subj.1D

# compute average ACF blur (from every other row) and append
blurs=( \$( 3dTstat -mean -prefix - blur.epits.1D'{1..\$(2)}'\' 2> /dev/null ) )
echo average epits ACF blurs: \$blurs
echo "\${blurs[@]}   # epits ACF blur estimates" >> blur_est.\$subj.1D

# -- estimate blur for each run in err_reml --
touch blur.err_reml.1D

# restrict to uncensored TRs, per run
for run in \$runs ; do
    trs=\$( 1d_tool.py -infile X.xmat.1D -show_trs_uncensored encoded \\
                          -show_trs_run \$run )
    if [[ \$trs == "" ]] ; then
       continue
    fi
    3dFWHMx -detrend -mask mask_group+tlrc                          \\
            -ACF files_ACF/out.3dFWHMx.ACF.err_reml.r\$run.1D             \\
            errts.\${subj}_REML+tlrc"[\$trs]" >> blur.err_reml.1D
done

# compute average FWHM blur (from every other row) and append
blurs=( \$( 3dTstat -mean -prefix - blur.err_reml.1D'{0..\$(2)}'\' 2> /dev/null ) )
echo average err_reml FWHM blurs: \$blurs
echo "\${blurs[@]}   # err_reml FWHM blur estimates" >> blur_est.\$subj.1D

# compute average ACF blur (from every other row) and append
blurs=( \$( 3dTstat -mean -prefix - blur.err_reml.1D'{1..\$(2)}'\' 2> /dev/null ) )
echo average err_reml ACF blurs: \$blurs
echo "\${blurs[@]}   # err_reml ACF blur estimates" >> blur_est.\$subj.1D


# ---------------------------------------------------------
# QC: compute correlations with spherical ~averages
@radial_correlate -nfirst 0 -polort 3 -do_clean yes                      \\
                  -rdir radcor.pb06.regress                              \\
                  -mask mask_group+tlrc                                  \\
                  all_runs.\$subj+tlrc.HEAD errts.\${subj}_REML+tlrc.HEAD

# ========================= auto block: QC_review ==========================
# generate quality control review scripts and HTML report

# generate a review script for the unprocessed EPI data
# (all echoes of all runs)
gen_epi_review.py -script @epi_review.\$subj \\
    -dsets pb00.\$subj.r*.e*.tcat+orig.HEAD

# -------------------------------------------------
# generate scripts to review single subject results
# (try with defaults, but do not allow bad exit status)

# write AP uvars into a simple txt file
cat << END > out.ap_uvars.txt
  mot_limit          : \${motionThreshold}
  out_limit          : \${outlierThreshold}
  copy_anat          : anatSS.\${subj}+orig.HEAD
  combine_method     : m_tedana_OC
  mask_dset          : mask_group+tlrc.HEAD
  template           : MNI152_2009_template_SSW.nii.gz
  ss_review_dset     : out.ss_review.\$subj.txt
  echo_times         : 13.000 26.6000 40.2000
  max_4095_warn_dset : out.4095_warn.txt
  reg_echo           : 2
  slice_pattern      : @${sliceTimesFilename}
  vlines_tcat_dir    : vlines.pb00.tcat
END

# and convert the txt format to JSON
cat out.ap_uvars.txt | afni_python_wrapper.py -eval "data_file_to_json()" \\
  > out.ap_uvars.json

# initialize gen_ss_review_scripts.py with out.ap_uvars.json
gen_ss_review_scripts.py -exit0        \\
    -init_uvars_json out.ap_uvars.json \\
    -write_uvars_json out.ss_review_uvars.json

# ========================== auto block: finalize ==========================

# remove temporary files
\rm -f rm.*

# --------------------------------------------------
# if the basic subject review script is here, run it
# (want this to be the last text output)
if [[ -e @ss_review_basic ]] ; then
    ./@ss_review_basic |& tee out.ss_review.\$subj.txt

    # generate html ss review pages
    # (akin to static images from running @ss_review_driver)
    apqc_make_tcsh.py -review_style pythonic -subj_dir . \\
        -uvar_json out.ss_review_uvars.json
    apqc_make_html.py -qc_dir QC_\$subj

    echo "\nconsider running: \n"
    echo "   afni_open -b task-tapping-preprocessed-polortA-NL/QC_\$subj/index.html"
    echo ""
fi

##
## This script was generated on $( date ) by invoking the following command on $( uname -n ):
## ${FULL_COMMAND_LINE}
##	
echo "FINISHED:  ${FULL_COMMAND_LINE}"
EOF

chmod +x $outputScriptName
## note that the following is only relevant to the server and pass to the enqueue zero if on my mac
if [[ $enqueue -eq 1 ]] ; then
    queue=localQ
    info_message_ln "Submitting job for execution to queuing system"

    LOG_FILE=${LOG_DIR}/${ss}_preprocess-${task}.log
    info_message_ln "To see progress run: tail -f $LOG_FILE"

    rm -f ${LOG_FILE}
    sbatch --job-name ${task}_${ss}-mt$( echo ${motionThreshold}| sed 's/[.]/_/' )-regress-only \
	   --partition ${queue} \
	   --export=ALL \
	   --mail-type=NONE \
	   --chdir $( pwd ) \
	   --output ${LOG_FILE} \
	   --ntasks=1 \
	   --cpus-per-task=${threads} \
	   ${outputScriptName}
    
    info_message_ln "Running squeue"
    squeue -ar
else
    info_message_ln "Job *NOT* submitted for execution to queuing system."
    info_message_ln "Pass -q or --enqueue option to this script to do so."	
fi
