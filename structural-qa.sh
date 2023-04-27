#!/bin/bash

#set -x 

# if ctrl-c is typed exit immediatly
trap exit SIGHUP SIGINT SIGTERM

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

# export AFNI_LAYOUT_FILE=elvis
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
# export AFNI_CROSSHAIR_LINES=NO

#export AFNI_LAYOUT_FILE=elvis
#subjects="311_A"

## subjects="$*"

# PIF=StructuralQa    #A string identifying programs launched by this script
#                             #Get a free line and tag programs from this script
# NPB="-npb `afni -available_npb_quiet` -pif $PIF -echo_edu" 

# @Quiet_Talkers -pif $PIF > /dev/null 2>&1   #Quiet previously launched programs

# afni $NPB -niml -yesplugouts $adir/afni  >& /dev/null &

# plugout_drive  $NPB     

subjects=$( find ${SOURCE_DATA} -maxdepth 1 -type d -name 'sub-[0-9][0-9][0-9]' | sed -e "s#${SOURCE_DATA}/##g" | sort ) 

header="subject,session,good,above,below"
structuralQaFile=$(pwd)/structural-qa-$( date +"%Y%m%d" ).csv
echo "${header}" > ${structuralQaFile}

#for subject in $subjects ; do
for subject in ${subjects}; do
    for session in ses-baseline ses-followup ; do
	if [[ -d ${DERIVATIVE_DATA}/afni-anat/${subject}/${session} ]] ; then
	    
	    ## can try derivative/afni-anat/${subject}/${session}
	    
	    echo "####################################################################################################"
	    echo "### Subject: $subject Session: ${session}"
	    echo "####################################################################################################"

	    cd ${DERIVATIVE_DATA}/afni-anat/${subject}/${session}

	    afni -q ${NPB} -noplugins -no_detach -yesplugouts  \
		 -com "SWITCH_UNDERLAY anatSS.${subject}_${session}.nii" \
		 -com "OPEN_WINDOW A.axialimage    opacity=9" \
		 -com "OPEN_WINDOW A.sagittalimage opacity=9" \
		 -com "OPEN_WINDOW A.coronalimage  opacity=9" &
	    ## -com "SET_XHAIRS OFF" & ## 2> /dev/null &
	    sleep 5

	    echo "Is the skull stripped anatomy of good quality (Y/N)?"
	    read quality
	    quality=$( echo ${quality} | tr '[:lower:]' '[:upper:]' )

	    if [[ ${quality} == "N" ]] ; then
		plugout_drive $NPB \
			      -com "SWITCH_UNDERLAY anat_cp.${subject}_${session}.nii" \
			      -quit
		
		echo "Enter clipping plane at the top of the brain"
		read above
		echo "Enter clipping plane at bottom of the brain"
		read below

		qaline="${subject},${session},${quality},${above},${below}"
		## @clip_volume -input $subject.anat+orig.HEAD -above $above -below $bottom

	    else
		qaline="${subject},${session},${quality},NA,NA"
	    fi
	    plugout_drive $NPB \
			  -com "QUITT" \
			  -quit
	    echo
	else
	    qaline="${subject},${session},NO_SESSION,NA,NA"
	fi
	echo "${qaline}"  >> ${structuralQaFile}
    done
done
echo "" >> ${structuralQaFile}

#     #echo "Press enter to quit afni and go to the next subject"
#     #read
#     plugout_drive $NPB \
# 	-com "QUIT" \
# 	-quit
#     echo
#     ## echo "Enter subject ID: "
# done

echo "####################################################################################################"
echo "### All done!"
echo "####################################################################################################"
