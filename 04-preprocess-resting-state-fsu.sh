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
DERIVATIVE_DATA=$ROOT/derivatives
ANAT_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

source ${HOME}/envs/tedana/bin/activate

if ! command -v tedana &> /dev/null
then
    error_message_ln "The tedana command could not be found."
    error_message_ln "Install it with pip install tedana."    
    exit
fi

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
		      -o "a:e:m:o:h:b:t:np:qx:c:" \
		      --longoptions "alignto:,excessiveMotionThresholdFraction:,motionThreshold:,outlierThreshold:,threads:,lowpass:,highpass:,blur:,tcat:,nonlinear,polort:,enqueue,extension:,nechos:,subject:,session:" \
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
    motionThreshold=0.25
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

if [[ "x${lowpass}" == "x" ]] ; then
     lowpass="0.01"
     warn_message_ln "No value for lowpass filter value to use was provided. Defaulting to $lowpass"
else
    info_message_ln "Using lowpass filter value of ${lowpass}"
fi

if [[ "x${highpass}" == "x" ]] ; then
     highpass="0.1"
     warn_message_ln "No value for highpass filter value to use was provided. Defaulting to $highpass"
else
    info_message_ln "Using highpass filter value of ${highpass}"
fi

if [[ "x${blur}" == "x" ]] ; then
     warn_message_ln "No value for blur filter value to use was provided. Blurring will be skipped"
     blurBlock=""
     afniProcBlurArgs=""
else
    info_message_ln "Using blur filter value of ${blur}"
    blurBlock="blur"    
    afniProcBlurArgs="-blur_size ${blur} -blur_to_fwhm -blur_opts_B2FW \"-ACF\""

fi

if [[ "x${nechos}" == "x" ]] ; then
    nechos="1"
    warn_message_ln "No value for the number of echos was provided. Defaulting to ${nechos}"
else
    info_message_ln "Using nechos value of ${nechos}"
fi

if [[ "x${polort}" == "x" ]] ; then
     polort="A"
     warn_message_ln "No value for polort, was provided. Letting 3dDeconvolve choose automatically"
else
    info_message_ln "Using polort value of ${polort}"
    afniProcPolortArg="-regress_polort ${polort}"
fi
scriptExt=$( combineExtensions ${scriptExt} "polort${polort}" )


# -eq is equals, check for equality of numbers
if [[ $nonlinear -eq 1 ]] ; then 
    info_message_ln "Using nonlinear alignment"
    scriptExt=$( combineExtensions ${scriptExt} NL )
else 
    info_message_ln "Using affine alignment only"
    scriptExt=$( combineExtensions ${scriptExt} AFF )    
fi

info_message_ln "Final script extensions: ${scriptExt}"

extraAlignmentArgs_orig="-volreg_align_e2a -align_opts_aea -giant_move" # -deoblique on -cost lpc+ZZ"
info_message_ln "Setting extraAlignmentArgs to: ${extraAlignmentArgs_orig}"

[[ -d run ]] || mkdir run

prefix=${subject}/${session}
ss=${subject}_${session}

unprocessedDataDir=${SOURCE_DATA}/${prefix}/func
SUMA_DIR=$DERIVATIVE_DATA/freesurfer-7.0/${ss}/SUMA
## regressorDir=${DERIVATIVE_DATA}/${subject}_${session}/afni/beh

info_message_ln "#################################################################################################"
info_message_ln "Generating script for Subject: $subject Session: ${session}"
declare -a epiFiles
if (( ${nechos} == 1 )) ; then
    sourceEpiFiles[0]=$( cd ${unprocessedDataDir}; ls -1 ${ss}_task-rest_bold.nii.gz | sort -t '_' -k 4.5n )
else
    for ee in $( seq 1 ${nechos} ) ; do
	sourceEpiFiles[${ee}]=$( cd ${unprocessedDataDir}; ls -1 ${ss}_task-rest_echo-${ee}_bold.nii.gz | sort -t '_' -k 4.5n -k 5.6n )
    done
fi

for (( ee=1; ee <= ${nechos}; ee++ )) ; do
    for ff in ${sourceEpiFiles[$ee]} ; do 
	if [[ ! -f ${unprocessedDataDir}/$ff ]] ; then
	    warn_message_ln "No such file: $ff"
	    warn_message_ln "Cannot find one of the ${task} EPI files for ${ss}. Skipping subject."
	    continue
	else
	    epiFiles[$ee]="${epiFiles[$ee]} ${unprocessedDataDir}/$ff"
	fi
    done
done

info_message_ln "Got the following EPI files"
info_message_ln "[EE][RR]: EE=echo RR=run"
for (( ee=1; ee <= ${nechos}; ee++ )) ; do
    ## printf '[%02d]: %s\n' ${ee} "${epiFiles[$ee]}"
    nfiles=$( echo  ${epiFiles[$ee]} | wc -w )
    nf=0
    for ff in ${epiFiles[$ee]} ; do 
	msg=$( printf '[%02d][%02d]: %s\n' ${ee} ${nf} "${ff}" )
	info_message_ln "${msg}"
	(( nf=nf+1 ))
    done
done

anatFile=${ANAT_PIPELINE_DIR}/${prefix}/anatSS.${ss}.nii
info_message_ln "Anatomy file: ${anatFile}"
if  [[ ! -f ${anatFile} ]] ; then
    error_message_ln "Can not find anatomy file for subject ${ss}. Skipping."
    exit 256
fi

outputScriptName=${CODE_DIR}/run/${ss}_preprocess-${task}.sh
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

info_message_ln "Writing script: $outputScriptName"

# now get the echo times
info_message_ln "Getting echo times from the first 3 EPI files" 
declare -a echos
for (( ee=1; ee <= ${nechos}; ee++ )) ; do
    ff=$( echo ${epiFiles[$ee]} | awk '{print $1}' )
    echos[$ee]=$( $CODE_DIR/get_bids_key_value.r -k EchoTime -j ${ff%.nii*}.json )
    ## convert echo times from the JSON sidecar in seconds to
    ## milliseconds
    echos[$ee]=$( echo "${echos[$ee]} * 1000" | bc )
    msg=$( printf 'Echo [%02d]: %s\n' ${ee} ${echos[$ee]} )
    info_message_ln "${msg}"
    if [[ ! -z ${dsets_me_run_arg} ]] ; then 
	dsets_me_run_arg+="${epiFiles[$ee]} "
    else
	dsets_me_run_arg="-dsets_me_run ${epiFiles[$ee]}"
    fi
done
## info_message_ln "${dsets_me_run_arg}"
info_message_ln "Writing: ${outputScriptName}"

if [[ ! -d ${PIPELINE_DIR}/ ]] ; then
    mkdir -p ${PIPELINE_DIR}/
fi

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

excessiveMotionThresholdFraction=$excessiveMotionThresholdFraction
excessiveMotionThresholdPercentage=$excessiveMotionThresholdPercentage

if [[ ! -d ${PIPELINE_DIR}/${prefix}/ ]] ; then
    mkdir -p ${PIPELINE_DIR}/${prefix}/
fi

cd ${PIPELINE_DIR}/${prefix}

preprocessingScript=${ss}_preprocess-${task}-${scriptExt}.csh
rm -f \${preprocessingScript}

outputDir=${task}-preprocessed-${scriptExt}
rm -fr \${outputDir}

motionThreshold=${motionThreshold}
outlierThreshold=${outlierThreshold}

## -tlrc_opts_at -init_xform AUTO_CENTER \\
source ${HOME}/envs/tedana/bin/activate
if ! command -v tedana &> /dev/null
then
    echo "The tedana command could not be found."
    echo "Install it with pip install tedana."    
    exit
else
    TEDANA=\$( which tedana )
fi

btemplate=MNI152_2009_template_SSW.nii.gz    
tpath=\$( @FindAfniDsetPath \${btemplate} )
if [[ \${tpath} == "" ]] ; then 
   echo "*** Couldn't find path to MNI152_2009_template_SSW.nii.gz template."
   echo "*** Exiting"
   exit 1
fi

epiFiles=( ${epiFiles[*]} )

dsetFile=\$( echo \${epiFiles} | awk '{print \$1}' )
jsonFile=\${dsetFile%.nii*}.json
sliceTimesFile=\$( basename \${dsetFile%.nii*}.sliceTimes.txt )

${CODE_DIR}/get_bids_key_value.r -k SliceTiming -j \${jsonFile} > \${sliceTimesFile}

sliceTimesFilename=\$( ls \$(pwd)/*.sliceTimes.txt )

export AFNI_3dDeconvolve_GOFORIT=YES

afni_proc.py -subj_id ${ss}									\\
             -script \${preprocessingScript}								\\
	     -out_dir \${outputDir}									\\
	     -blocks despike tshift align tlrc volreg mask combine ${blurBlock} scale regress	       	\\
	     -copy_anat $anatFile									\\
	     -anat_follower     anat_w_skull anat ${anatFile%/*}/anatU.${ss}.nii  		      	\\
             -anat_follower_ROI aaseg        anat ${SUMA_DIR}/aparc.a2009s+aseg.nii			\\
             -anat_follower_ROI aeseg        epi  ${SUMA_DIR}/aparc.a2009s+aseg.nii			\\
             -anat_follower_ROI FSvent       epi  ${SUMA_DIR}/fs_ap_latvent.nii.gz 			\\
             -anat_follower_ROI FSWe         epi  ${SUMA_DIR}/fs_ap_wm.nii.gz      			\\
             -anat_follower_erode FSvent FSWe                          					\\
	     ${dsets_me_run_arg}									\\
             -echo_times ${echos[*]}									\\
	     -combine_method m_tedana_OC 								\\
	     -combine_tedana_path \${TEDANA}		 						\\
	     -combine_opts_tedana --fittype curvefit 							\\
             -tshift_interp -wsinc9                                  					\\
	     -volreg_warp_final_interp wsinc5								\\
	     -tshift_opts_ts -tpattern @\${sliceTimesFilename}						\\
	     -tlrc_base \${tpath}/\${btemplate}								\\
	     -volreg_align_to ${alignto}								\\
	     ${extraAlignmentArgs} -volreg_tlrc_warp ${afniProcBlurArgs}				\\
	     -mask_epi_anat yes  -mask_apply group							\\
	     -regress_reml_exec	 ${afniProcPolortArg}							\\
	     -regress_3dD_stop										\\
	     -regress_opts_3dD -GOFORIT 99 								\\
             -regress_ROI_PC FSvent 3                                  					\\
             -regress_ROI_PC_per_run FSvent                            					\\
             -regress_make_corr_vols aeseg FSvent                      					\\
             -regress_anaticor_fast                                    					\\
             -regress_anaticor_label FSWe                              					\\
	     -regress_apply_mot_types demean deriv							\\
             -regress_censor_motion  \$motionThreshold							\\
             -regress_censor_outliers \$outlierThreshold						\\
             -regress_apply_mot_types demean deriv							\\
             -regress_est_blur_epits									\\
             -regress_est_blur_errts									\\
	     -regress_run_clustsim no									\\
	     -regress_opts_reml -GOFORIT 								\\
	     -html_review_style pythonic

## the next will only be true if afni proc completed correctly, then will execute the script using tcsh language
if [[ -f \${preprocessingScript} ]] ; then 
   tcsh -xef \${preprocessingScript}

## X.xmat.1D is produced by 3ddeconvolve
    cd \${outputDir}
    xmat_regress=X.xmat.1D 

    if [[ -f \$xmat_regress ]] ; then 
##fraction censored... if have 100 volumes and 20 censored, .2 are censored.  the command reads the file and says how many are censored
        fractionOfCensoredVolumes=\$( 1d_tool.py -infile \$xmat_regress -show_tr_run_counts frac_cen )
        numberOfCensoredVolumes=\$( 1d_tool.py -infile \$xmat_regress -show_tr_run_counts trs_cen | xargs | tr ' ' '+' | bc)
        totalNumberOfVolumes=\$( 1d_tool.py -infile \$xmat_regress -show_tr_run_counts trs_no_cen | xargs | tr ' ' '+' | bc )

        ## rounding method from http://www.alecjacobson.com/weblog/?p=256, note that gt is greater than
        cutoff=\$( echo "((\$excessiveMotionThresholdFraction*\$totalNumberOfVolumes)+0.5)/1" | bc )
	if [[ \$numberOfCensoredVolumes -gt \$cutoff ]] ; then 

	    echo "*** A total of \$numberOfCensoredVolumes of
	    \$totalNumberOfVolumes volumes were censored which is
	    greater than \$excessiveMotionThresholdFraction
	    (n=\$cutoff) of all total volumes of this subject" > \\
		00_DO_NOT_ANALYSE_${ss}_\${excessiveMotionThresholdPercentage}percent.txt

	    echo "*** WARNING: $subject will not be analysed due to having more than \${excessiveMotionThresholdPercentage}% of their volumes censored."
	fi
    else
        # if xmatrix file doesn't exist, deconvolution could not be accomplished,touch is a command to actually create a file (above the xmatrix exists but too much motion)
        #if file exists, updates its time stamp, if not, creates the file and sets its time stamp
	touch 00_DO_NOT_ANALYSE_${ss}_\${excessiveMotionThresholdPercentage}percent.txt
    fi
    echo "Compressing BRIKs and nii files"
    #next says find any files ending in BRIK or nii and compress
    find ./ \( -name "*.BRIK" -o -name "*.nii" \) -print0 | xargs -0 pigz -p ${threads}
else
    echo "*** No such file \${preprocessingScript}"
    echo "*** Cannot continue"
    exit 1
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
    sbatch --job-name ${task}_${ss} \
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
