#!/bin/bash

# set -x

#exit immediately on an error
set -e 

exit_cleanup() {
    if [[ ! -z ${xvfb_pid} ]] ; then
	kill -9 ${xvfb_pid}
    fi

    if [[ ! -z ${display_number_log} ]] ; then 
	rm -f ${display_number_log}
    fi
    # check if fd 6 is open, if so close it
    if command >&6 ; then
	# close fd 6
	exec 6>&-
    fi
}

trap exit_cleanup SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

## font family used by convert from ImageMagick to annotate the images created by AFNI
FONT_FAMILY="FreeMono"

# options ot provide to Xvfb
XVFB_OPTIONS="-nolisten tcp"

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}

# this is for the command line options; on mac would have to have gnu
# get opt installed; probably in macports, definitely in homebrew"
GETOPT_OPTIONS=$( $GETOPT \
		      -o "c:e:r:s:t:" \
		      --longoptions "nechos:,runs:,session:,subject:,task:" \
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
	-c|--nechos)
	    nechos=${2}; shift 2 ;;	
	-e|--session)
	    session=${2}; shift 2 ;;
	-r|--runs)
	    runs=${2}; shift 2 ;;
	-s|--subject)
	    subject=${2}; shift 2 ;;
	-t|--task)
	    task=${2}; shift 2 ;;
	--) 
	    shift ; break ;;
	
	*) 
	    error_message_ln "${PROGRAM_NAME}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

if [ -z ${subject} ] ; then 
    error_message_ln "ERROR: The subject ID was not provided."
    exit
fi

if [ -z ${session} ] ; then 
    error_message_ln "ERROR: The subject's session was not provided."
    exit
fi

if [ -z ${task} ] ; then 
    error_message_ln "ERROR: The task name was not provided."
    exit
fi

if [[ ${session:0:4} != "ses-" ]] ; then
    session="ses-${session}"
fi

if [[ ${subject:0:4} != "sub-" ]] ; then
    subject="sub-${subject}"
fi

PIPELINE_DIR=${DERIVATIVE_DATA}/movies-${task}
if [[ ! -d ${PIPELINE_DIR}/ ]] ; then
    mkdir -p ${PIPELINE_DIR}/
fi

prefix=${subject}/${session}
ss=${subject}_${session}

unprocessedDataDir=${SOURCE_DATA}/${prefix}/func

## the following code enumerates echos within runs
info_message_ln "#################################################################################################"
info_message_ln "Generating movie for Subject: $subject Session: ${session} Task: ${task}"
declare -a sourceEpiFiles
if [[ -z ${runs} ]] ; then
    if (( ${nechos} == 1 )) ; then
	# 1 run 1 echo
	sourceEpiFiles[0]=$( cd ${unprocessedDataDir}; ls -1 ${ss}_task-${task}_bold.nii.gz | sort -t '_' -k 4.5n )
    else
	# 1 run > 1 echoes
	sourceEpiFiles=$( cd ${unprocessedDataDir}; ls -1 ${ss}_task-${task}_echo-?_bold.nii.gz | sort -t '_' -k 4.5n -k 5.6n )
    fi
else
    if (( ${nechos} == 1 )) ; then
	# > 1 run 1 echo
	sourceEpiFiles[0]=$( cd ${unprocessedDataDir}; ls -1 ${ss}_task-${task}_run-?_bold.nii.gz | sort -t '_' -k 4.5n )
    else
	# > 1 run > 1 echoes
	for (( rr=0; rr< ${runs}; rr++ )) ; do
	    (( run=rr + 1 ))
	    sourceEpiFiles[${rr}]=$( cd ${unprocessedDataDir}; ls -1 ${ss}_task-${task}_run-${run}_echo-?_bold.nii.gz | sort -t '_' -k 4.5n -k 5.6n )
	done
    fi
fi

declare -a epiFiles
for (( rr=0; rr < ${#sourceEpiFiles[@]}; rr++ )) ; do
    ff=${sourceEpiFiles[${rr}]} ## one or more echo files per run
    for ef in ${ff} ; do
	if [[ ! -f ${unprocessedDataDir}/$ef ]] ; then
	    warn_message_ln "No such file: $ef"
	    warn_message_ln "Cannot find one of the ${task} EPI files for ${ss}. Skipping subject."
	    continue
	else
	    epiFiles[$rr]="${epiFiles[$rr]} ${unprocessedDataDir}/${ef}"
	fi
    done
done

info_message_ln "Got the following EPI files"
info_message_ln "[RR][EE]: EE=echo RR=run"
for (( rr=0; rr < ${#epiFiles[@]}; rr++ )) ; do
    ## printf '[%02d]: %s\n' ${rr} "${epiFiles[$rr]}"
    nf=0
    for ff in ${epiFiles[$rr]} ; do 
	msg=$( printf '[%02d][%02d]: %s\n' $(( rr + 1 )) $(( nf + 1 )) "${ff}" )
	info_message_ln "${msg}"
	(( nf=nf+1 ))
    done
done
nruns=${#epiFiles[@]} #=$( echo  ${epiFiles[1]} | wc -w )

####################################################################################################


export AFNI_LAYOUT_FILE=elvis
# GUI settings for simplicity
# taken from @chauffeur_afni
export AFNI_NOSPLASH=YES
export AFNI_SPLASH_MELT=NO
export AFNI_ENVIRON_WARNINGS=NO
export AFNI_COMPRESSOR=NONE
#export AFNI_IMAGE_GLOBALRANGE= VOLUME
export AFNI_NEVER_SAY_GOODBYE=YES
export AFNI_STARTUP_WARNINGS=NO
export AFNI_MOTD_CHECK=NO
export AFNI_NIFTI_TYPE_WARN=NO
export AFNI_CROSSHAIR_LINES=NO

if [[ ! -t 1 ]] ; then
    ## stdout is not a terminal so don't colorize output from AFNI
    export AFNI_MESSAGE_COLORIZE=NO
fi 

PIF=${subject}_${session}_task-${task}    # A string identifying programs launched by this script
# Get a free line and tag programs from this script
#NPB="-npb `afni -available_npb_quiet` -pif $PIF -echo_edu" 

#@Quiet_Talkers -pif $PIF > /dev/null 2>&1   #Quiet previously launched programs

IMAGES_DIR=${PIPELINE_DIR}/${prefix}/images
if [[ ! -d ${IMAGES_DIR} ]] ; then
    mkdir -p ${IMAGES_DIR}
fi

cd ${unprocessedDataDir}

display_number_log=$( mktemp /tmp/xvfb_display.XXXXXX )
exec 6>${display_number_log}
Xvfb ${XVFB_OPTIONS} -displayfd 6 -screen 0 1024x768x24 &
xvfb_pid=$!
sleep 1
DISPLAY=":$( cat ${display_number_log} )"
info_message_ln "Found first available X11 display number: ${DISPLAY}"

export DISPLAY

for (( rr=0; rr < ${nruns}; rr++ )) ; do
    ## printf '[%02d]: %s\n' ${ee} "${epiFiles[$ee]}"
    ee=1
    for ff in ${epiFiles[$rr]} ; do

	if [[ ${ee} == 1 ]] ; then
	    # 3dBrickStat returns two numbers. The first is the
	    # requested percentile and the second is the value of that
	    # percentile in the dataset ${ff}
	    echo_one_min=( $( 3dBrickStat -slow -percentile  2 1  2 ${ff}'[0]' 2> /dev/null ) )
	    echo_one_max=( $( 3dBrickStat -slow -percentile 98 1 98 ${ff}'[0]' 2> /dev/null ) )	    
	fi
       
	info_message_ln "Making images for the following EPI file"
	info_message_ln "[RR][EE]: EE=echo RR=run"
	msg=$( printf '[%02d][%02d]: %s\n' $(( rr + 1)) ${ee} "${ff}" )
	info_message_ln "${msg}"

	 afni -q ${NPB} -noplugins -no_detach -yesplugouts  \
	     -com "SWITCH_UNDERLAY ${ff}" \
	     -com "OPEN_WINDOW A.axialimage    opacity=9" \
	     -com "OPEN_WINDOW A.sagittalimage opacity=9" \
	     -com "OPEN_WINDOW A.coronalimage  opacity=9" \
	     -com "SET_XHAIRS OFF" & ## 2> /dev/null &
	sleep 5
	
	## nTimesteps=$( env AFNI_NO_OBLIQUE_WARNING=YES 3dnvals ${ff} )
	nTimesteps=5

	# -com "SWITCH_OVERLAY  sub-${subjectId}_task-noboot_gre+orig.HEAD" \
	    # -com "SET_PBAR_ALL -99 1.0 gray_circle" \
	    # -com 'SEE_OVERLAY +'                               \
	    # -com "SET_THRESHNEW 0" \
    
	# plugout_drive ${NPB} \
	# 	      -com "SWITCH_UNDERLAY ${ff}" \
	# 	      -com "OPEN_WINDOW A.axialimage    opacity=9" \
	# 	      -com "OPEN_WINDOW A.sagittalimage opacity=9" \
	# 	      -com "OPEN_WINDOW A.coronalimage  opacity=9" \
	# 	      -com "SET_XHAIRS OFF"                        \
	# 	      -quit

	for index in $( seq 0 $(( nTimesteps -1 )) ) ; do
	    ## for index in $( seq 0 100 ) ; do
	    imageFilename=${ff##*/}
	    indexFormatted=$( printf %04d ${index} )
	    imageFilenameAx=${imageFilename%%_bold.nii.gz}_orient-ax_img-${indexFormatted}_bold.jpg
	    imageFilenameSag=${imageFilename%%_bold.nii.gz}_orient-sag_img-${indexFormatted}_bold.jpg
	    imageFilenameCor=${imageFilename%%_bold.nii.gz}_orient-cor_img-${indexFormatted}_bold.jpg	    
	    info_message_ln "Saving image $( printf %04d ${index} ) of $( printf %04d ${nTimesteps} )"
	    info_message_ln "Saving ${imageFilenameAx}"
	    info_message_ln "Saving ${imageFilenameSag}"
	    info_message_ln "Saving ${imageFilenameCor}"

	    ## -com "SET_ULAY_RANGE A.all ${echo_one_min[1]} ${echo_one_max[1]}"  \

	    plugout_drive ${NPB} \
			  -com "SET_SUB_BRICKS ${index} -1 -1"                               \
			  -com "SAVE_JPEG A.axialimage    ${IMAGES_DIR}/${imageFilenameAx}"  \
			  -com "SAVE_JPEG A.sagittalimage ${IMAGES_DIR}/${imageFilenameSag}" \
			  -com "SAVE_JPEG A.coronalimage  ${IMAGES_DIR}/${imageFilenameCor}" \
			  -quit
	    convert ${IMAGES_DIR}/${imageFilenameAx} \
		    -resize 400% \
		    -font ${FONT_FAMILY} \
		    -background white label:"Echo ${ee}" \
		    -gravity Center \
		    -append ${IMAGES_DIR}/${imageFilenameAx%%.jpg}_lrg.png
	    convert ${IMAGES_DIR}/${imageFilenameSag} \
		    -resize 400% \
		    -font ${FONT_FAMILY} \
		    -background white label:"Echo ${ee}" \
		    -gravity Center \
		    -append ${IMAGES_DIR}/${imageFilenameSag%%.jpg}_lrg.png
	    convert ${IMAGES_DIR}/${imageFilenameCor} \
		    -resize 400% \
		    -font ${FONT_FAMILY} \
		    -background white label:"Echo ${ee}" \
		    -gravity Center \
		    -append ${IMAGES_DIR}/${imageFilenameCor%%.jpg}_lrg.png

	done
	plugout_drive ${NPB} \
		      -com 'QUITT' \
		      -quit

	(( ee=ee+1 ))
    done
    # for index in $( seq 0 $(( nTimesteps -1 )) ) ; do
    # 	indexFormatted=$( printf %04d ${index} )
    # 	convert ${IMAGES_DIR}/${subject}_${session}_task-${task}_run-$(( rr+1 ))_echo-1_orient-ax_img-${indexFormatted}_bold_lrg.png \
    # 		${IMAGES_DIR}/${subject}_${session}_task-${task}_run-$(( rr+1 ))_echo-2_orient-ax_img-${indexFormatted}_bold_lrg.png \
    # 		${IMAGES_DIR}/${subject}_${session}_task-${task}_run-$(( rr+1 ))_echo-3_orient-ax_img-${indexFormatted}_bold_lrg.png \
    # 		+append ${IMAGES_DIR}/${subject}_${session}_task-${task}_run-$(( rr+1 ))_echo-all_orient-ax_img-${indexFormatted}_bold_lrg.png
    # done
done
#fi

if [[ 1 == 0 ]] ; then 
cd ${IMAGES_DIR}
for ori in ax cor sag; do
    for (( rr=0; rr < ${nruns}; rr++ )) ; do
	run=$(( rr + 1 ))

	info_message_ln "Making movie"
	info_message_ln "[RR]: RR=run"
	msg=$( printf '[%02d]: Orientation: %s\n' ${run} "${ori}" )
	info_message_ln "${msg}"
	imagePatternPrefix="${subject}_${session}_task-${task}_run-${run}_orient-${ori}"
	for (( ee=1; ee <= ${nechos}; ee++ )) ; do
	    ffmpegInputs[$ee]="-pattern_type sequence -start_number 0 -framerate 10 -i ${subject}_${session}_task-${task}_run-${run}_echo-${ee}_orient-${ori}_img-%04d_bold_lrg.png"
	done

	fil=""
	for (( ee =0; ee < ${nechos}; ee++ )) ; do
	    fil="${fil}[${ee}:v]"
	done
	
	## echo "${ffmpegInputs[*]}"
	ffmpeg  -loglevel debug -y  \
		${ffmpegInputs[*]} \
		-filter_complex "${fil}hstack=inputs=${nechos}" \
		-metadata:g artist="Colm G. Connolly" -metadata:g title="${imagePatternPrefix} Movie" \
		-metadata:g year="$( date +'%Y')" -metadata:g comment="Ahn R01 Pain Supplement Subject EPI Movie" \
		-c:v libx264 -pix_fmt yuv420p -r 10 ../${imagePatternPrefix}_bold_lrg.mp4
    done
done
fi

info_message_ln "Killing Xvfb process is: ${xvfb_pid}"
kill -9 ${xvfb_pid}
## convert -coalesce -delay 100 -resize 200% sub-${subjectId}_task-noboot_gre_*.jpg sub-${subjectId}_task-noboot_gre.gif 

# close fd 6
exec 6>&-
rm -f ${display_number_log}
