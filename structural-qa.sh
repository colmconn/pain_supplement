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
DERIVATIVE_DATA=$ROOT/derivatives
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

export AFNI_LAYOUT_FILE=${CODE_DIR}/struct-qc-layout
#subjects="311_A"

## subjects="$*"

# PIF=StructuralQa    #A string identifying programs launched by this script
#                             #Get a free line and tag programs from this script
# NPB="-npb `afni -available_npb_quiet` -pif $PIF -echo_edu" 

# @Quiet_Talkers -pif $PIF > /dev/null 2>&1   #Quiet previously launched programs

# afni $NPB -niml -yesplugouts $adir/afni  >& /dev/null &

# plugout_drive  $NPB     

## subjects=$( find ${SOURCE_DATA} -maxdepth 1 -type d -name 'sub-[0-9]{3,5}' -o -name 'sub-[0-9][0-9][0-9][0-9][0-9]' | sed -e "s#${SOURCE_DATA}/##g" | sort ) 
## sss=$( find  ${SOURCE_DATA} -maxdepth 2 -type d -name 'ses-*' | sed -e "s#${SOURCE_DATA}/##g" | sort )
sc=$( echo ${sss} | wc -w )
sss="sub-173/ses-baseline sub-173/ses-followup"

header="subject,session,orig_anat,brain_mask,MNI_brain"
structuralQaFile=${DERIVATIVE_DATA}/qc-inspection/structural-qc-$( date +"%Y%m%d" ).csv
#echo "${header}" > ${structuralQaFile}

#for subject in $subjects ; do

count=1
for ss in ${sss} ; do
    subject=${ss%%/*}
    session=${ss##*/}
    
    ## can try derivative/afni-anat/${subject}/${session}
    
    echo "####################################################################################################"
    echo $( printf "### %03d of %03d | Subject: %s Session: %s" ${count} ${sc} ${subject} ${session} )
    echo "####################################################################################################"
    
    cd ${DERIVATIVE_DATA}/afni-anat/${subject}/${session}
    
    afni -q ${NPB} -noplugins -no_detach -yesplugouts  \
	 -com "SWITCH_UNDERLAY anatU.${subject}_${session}.nii " \
	 -com "OPEN_WINDOW A.axialimage    mont=3x3:15 ifrac=0.9 opacity=4" \
	 -com "OPEN_WINDOW A.sagittalimage mont=3x3:10 ifrac=0.9 opacity=4" \
	 -com "OPEN_WINDOW A.coronalimage  mont=3x3:10 ifrac=0.9 opacity=4" \
	 -com "SET_XHAIRS OFF" 2> /dev/null  &
    ## -com "SET_XHAIRS OFF" & ## 2> /dev/null &
    sleep 5


    if command -v wmctrl &> /dev/null ; then
	wmctrl -i -a $( wmctrl -l | grep colmconn@med51hp9t2 | awk '{print $1}' )
    fi
    
    echo "Is the unifized anatomy of good quality (Y/N)?"
    read anat_quality
    quality=$( echo ${quality} | tr '[:lower:]' '[:upper:]' )

    plugout_drive ${NPB} \
		  -com "SWITCH_OVERLAY anatSS.${subject}_${session}.nii" \
		  -com "SEE_OVERLAY +"                                \
		  -quit 2> /dev/null
    echo "Is the skull stripped anatomy of good quality (Y/N)?"
    read ss_quality
    quality=$( echo ${quality} | tr '[:lower:]' '[:upper:]' )


    plugout_drive ${NPB} \
		  -com "SWITCH_UNDERLAY anatQQ.${subject}_${session}.nii " \
		  -com "SEE_OVERLAY -"                                \
		  -quit 2> /dev/null
    echo "Is the MNI anatomy of good quality (Y/N)?"
    read mni_quality
    quality=$( echo ${quality} | tr '[:lower:]' '[:upper:]' )

    
    # if [[ ${quality} == "N" ]] ; then
    #     plugout_drive $NPB \
	# 		  -com "SWITCH_UNDERLAY anat_cp.${subject}_${session}.nii" \
	# 		  -quit
    
    #     echo "Enter clipping plane at the top of the brain"
    #     read above
    #     echo "Enter clipping plane at bottom of the brain"
    #     read below
    
    #     qaline="${subject},${session},${quality},${above},${below}"
    #     ## @clip_volume -input $subject.anat+orig.HEAD -above $above -below $bottom
    
    # else
    #     qaline="${subject},${session},${quality},NA,NA"
    # fi
    plugout_drive $NPB \
		  -com "QUITT" \
		  -quit
    echo
    ## else
    qaline="${subject},${session},${anat_quality},${ss_quality},${mni_quality}"

    #echo "${qaline}"  >> ${structuralQaFile}
    (( count=count + 1 ))
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
