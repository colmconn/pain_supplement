#!/bin/bash

# set -x

#exit immediatly on an error
set -e

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

function extractRois {
    local input=$1
    local output=$2
    local roiFile=$3

    template=/tmp/${roiFile##*/}.XXX
    tmp=$(mktemp ${template} )

    sort -k1n ${roiFile} > ${tmp}
    info_message "Extracting Roi: "
    while IFS=' ' read -ra line ; do
	echo -n "${line[0]} "
	3dcalc -a ${input} -prefix ${input%%.nii.gz}_roi${line[0]}_rm.nii -expr "a*equals(a, ${line[0]})" 2> /dev/null
    done < ${tmp}
    echo

    info_message_ln "Merging separated ROIs"
    3dMean -sum -prefix ${output} ${input%%.nii.gz}_roi*_rm.nii
    
    rm -f r${tmp}  ## ${input%%.nii.gz}_roi*_rm.nii*
}

function chunk {
    local chunkSize=$1
    local fileToChunk=$2
    local prefix="$3"
    local suffix="$4"
    local op="$5"

    fileLineCount=$( cat ${fileToChunk} | wc -l )
    chunks=$(( fileLineCount / chunkSize ))
    remainder=$(( fileLineCount - (chunkSize * chunks ) ))
    template=/tmp/${fileToChunk##*/}.XXX
    tmp=$(mktemp ${template} )

    sort -k1n ${fileToChunk} > ${tmp}
    
    # info_message_ln "File                             : ${fileToChunk##*/}"
    # info_message_ln "File line count                  : ${fileLineCount}"
    # info_message_ln "Chunks length                    : ${chunks}"    
    # info_message_ln "Number of chunks                 : ${chunks}"
    # info_message_ln "Remaining lines in a final chunk : ${remainder}"
    # info_message_ln "Prefix                           : ${prefix}"
    # info_message_ln "Suffix                           : ${suffix}"
    # info_message_ln "Operator                         : ${op}"        

    arrayIndex=0
    lineCount=0
    declare -a chunkArray
    while IFS=' ' read -ra line ; do
	## check if the variable (i.e.,, array element)
	## chunkArray[${arrayIndex}] exists
	if [[ -v "chunkArray[${arrayIndex}]" ]] ; then 
	    ### chunkArray[${arrayIndex}]="${chunkArray[$arrayIndex]},${line[0]}"
	    chunkArray[${arrayIndex}]="${chunkArray[$arrayIndex]},equals(a,${line[0]})"	    
	    ## chunkArray[${arrayIndex}]="${chunkArray[$arrayIndex]},$( printf "%03d" ${lineCount} )"
	else
	    ## chunkArray[${arrayIndex}]="${line[0]}"
	    chunkArray[${arrayIndex}]="equals(a, ${line[0]})"
	    ## chunkArray[${arrayIndex}]="$( printf "%03d" ${lineCount} )"
	fi

	(( lineCount+=1 ))
	if (( lineCount % chunkSize == 0 )) ; then
	    ## echo incr arrayIndex
	    (( arrayIndex+=1 ))
	fi
	##    done < ${fileToChunk}
    done < ${tmp}    

    ## echo "Array size=${#chunkArray[@]}"
    retVal=""
    for (( i=0; i<${#chunkArray[@]}; i+=1 )) ; do
	## echo "$(( i + 1 )) --> ${chunkArray[$i]}"
	chunkText="${chunkArray[$i]}"
	if [[ ${i} == 0 ]] ; then
	    retVal="${prefix}${chunkText}${suffix}"
	else
	    retVal="${retVal} ${op} ${prefix}${chunkText}${suffix}"
	fi
    done

    rm -f r${tmp}
    
    echo "${retVal}"
}


studyName=pain_supplement
task="resting-state"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
ANAT_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}

#this is for the command line options; on mac would have to have gnu get opt installed; probably in macports, definitely in homebrew"
# GETOPT_OPTIONS=$( $GETOPT \
# 		      -o "a:e:l:m:r:s:" \
# 		      --longoptions "atlas:,session,list:,mask:,rois:,subject:" \
# 		      -n ${PROGRAM_NAME} -- "$@" )
GETOPT_OPTIONS=$( $GETOPT \
		      -o "e:s:" \
		      --longoptions "session:,subject:" \
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
	# -a|--atlas)
	#     atlas=$2; shift 2 ;;	
	-e|--session)
	    session=$2; shift 2 ;;	
	# -l|--list)
	#     roiList=1; shift 1 ;;	
	# -m|--mask)
	#     mask=$2; shift 2 ;;
	# -r|--rois)
	#     rois=$2; shift 2 ;;	
	-s|--subject)
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

# if [[ "x${atlas}" == "x" ]] ; then
#     error_message_ln "No value for atlas. Canot continue."
#     exit 1
# else
#     info_message_ln "Using atlas: ${atlas}"
# fi

# if [[ "x${list}" == "x" ]] ; then
#     error_message_ln "No value for list of atlas ROIs. Canot continue."
#     exit 1
# else
#     info_message_ln "Using list of atlas ROIs: ${list}"
# fi

# if [[ "x${mask}" == "x" ]] ; then
#     error_message_ln "No value for mask. Canot continue."
#     exit 1
# else
#     info_message_ln "Using mask: ${mask}"
# fi

# if [[ "x${rois}" == "x" ]] ; then
#     error_message_ln "No value for the atlas ROIs file. Canot continue."
#     exit 1
# else
#     info_message_ln "Using this file for atlas ROIs: ${rois}"
# fi

prefix=${subject}/${session}
ss=${subject}_${session}

SUMA_DIR=$DERIVATIVE_DATA/freesurfer-7.0/${ss}/SUMA
RESTING_STATE_DIR=$DERIVATIVE_DATA/afni-resting-state/${prefix}/resting-state-preprocessed-polortA-NL/

if [[ ! -d ${SUMA_DIR} ]] ; then
    error_message_ln "No such directory: ${SUMA_DIR}"
    error_message_ln "@SUMA_Make_Spec_FS has not been run for this subject."
    exit 1
fi

if [[ ! -d ${RESTING_STATE_DIR} ]] ; then
    error_message_ln "No such directory: ${RESTING_STATE_DIR}"
    error_message_ln "04-preprocess-resting-state.sh has not been run for this subject."
    exit 1
fi

## TODO: dk.scgm.txt dkt.scgm.txt; do
if [[ -f ${HOME}/src/mri_library/atlases/destrieux.scgm.txt ]] ; then
    info_message_ln "Found \${HOME}/src/mri_library/atlases/destrieux.scgm.txt. Extracting ROIs. "
    
    if [[ ! -f ${SUMA_DIR}/aparc.a2009s+aseg_brainGraph.nii ]] ; then 
    ## expression='a*('"$( chunk 10 ${HOME}/src/mri_library/atlases/destrieux.scgm.txt 'amongst(a,' ')' '+' )"')'
    ## expression='a*or('"$( chunk 10 ${HOME}/src/mri_library/atlases/destrieux.scgm.txt 'amongst(a,' ')' ',' )"')'
    #expression='a*or('"$( chunk 10 ${HOME}/src/mri_library/atlases/destrieux.scgm.txt 'or(' ')' ',' )"')'        
    #echo ${expression}
    ## exit
	
	# 3dcalc -a ${SUMA_DIR}/aparc.a2009s+aseg.nii.gz \
	#        -expr "${expression}" \
	#        -prefix ${SUMA_DIR}/aparc.a2009s+aseg_brainGraph.nii

	extractRois ${SUMA_DIR}/aparc.a2009s+aseg.nii.gz  ${SUMA_DIR}/aparc.a2009s+aseg_brainGraph.nii ${HOME}/src/mri_library/atlases/destrieux.scgm.txt
	# exit
    fi
    info_message_ln "Done extracting ROIs "
    
    ( cd ${RESTING_STATE_DIR};
      if [[ -d destrieux.scgm ]] ; then rm -rf destrieux.scgm ; fi
      mkdir destrieux.scgm
      cd destrieux.scgm
      
      3dNwarpApply -source ${SUMA_DIR}/aparc.a2009s+aseg_brainGraph.nii    \
		   -master ../anat_final.${ss}+tlrc                \
		   -ainterp NN -nwarp ../anatQQ.${ss}_WARP.nii     \
		   ../anatQQ.${ss}.aff12.1D                        \
		   -prefix destrieux.scgm_aaseg
      # and copy its label table to the warped result
      3drefit -copytables ${SUMA_DIR}/aparc.a2009s+aseg.nii.gz destrieux.scgm_aaseg+tlrc
      3drefit -cmap INT_CMAP destrieux.scgm_aaseg+tlrc

     # warp follower dataset copy_af_aeseg+orig
      3dNwarpApply -source ${SUMA_DIR}/aparc.a2009s+aseg_brainGraph.nii    \
		   -master ../pb03.${ss}.r01.e02.volreg+tlrc       \
		   -ainterp NN -nwarp ../anatQQ.${ss}_WARP.nii     \
		   ../anatQQ.${ss}.aff12.1D                        \
		   -prefix destrieux.scgm_aeseg
      
      # and copy its label table to the warped result
      3drefit -copytables ${SUMA_DIR}/aparc.a2009s+aseg.nii.gz  destrieux.scgm_aeseg+tlrc
      3drefit -cmap INT_CMAP destrieux.scgm_aeseg+tlrc
    )
      
fi

if [[ -f ${HOME}/src/mri_library/atlases/dk.scgm.txt ]] ; then
    info_message_ln "Found \${HOME}/src/mri_library/atlases/dk.scgm.txt. Extracting ROIs. "

    if [[ ! -f ${SUMA_DIR}/aparc+aseg_brainGraph.nii ]] ; then 
	# expression='a*('"$( chunk 10 ${HOME}/src/mri_library/atlases/dk.scgm.txt 'amongst(a,' ')' '+' )"')'
	
	# 3dcalc -a ${SUMA_DIR}/aparc+aseg.nii.gz \
	#        -expr "${expression}" \
	#        -prefix ${SUMA_DIR}/aparc+aseg_brainGraph.nii
	extractRois ${SUMA_DIR}/aparc+aseg.nii.gz  ${SUMA_DIR}/aparc+aseg_brainGraph.nii ${HOME}/src/mri_library/atlases/dk.scgm.txt		
    fi
    info_message_ln "Done extracting ROIs"
    
    ( cd ${RESTING_STATE_DIR};
      if [[ -d dk.scgm ]] ; then rm -rf dk.scgm ; fi
      mkdir dk.scgm
      cd dk.scgm
      
      3dNwarpApply -source ${SUMA_DIR}/aparc+aseg_brainGraph.nii    \
		   -master ../anat_final.${ss}+tlrc                \
		   -ainterp NN -nwarp ../anatQQ.${ss}_WARP.nii     \
		   ../anatQQ.${ss}.aff12.1D                        \
		   -prefix dk.scgm_aaseg
      # and copy its label table to the warped result
      3drefit -copytables ${SUMA_DIR}/aparc+aseg.nii.gz dk.scgm_aaseg+tlrc
      3drefit -cmap INT_CMAP dk.scgm_aaseg+tlrc

     # warp follower dataset copy_af_aeseg+orig
      3dNwarpApply -source ${SUMA_DIR}/aparc+aseg_brainGraph.nii    \
		   -master ../pb03.${ss}.r01.e02.volreg+tlrc       \
		   -ainterp NN -nwarp ../anatQQ.${ss}_WARP.nii     \
		   ../anatQQ.${ss}.aff12.1D                        \
		   -prefix dk.scgm_aeseg
      
      # and copy its label table to the warped result
      3drefit -copytables ${SUMA_DIR}/aparc+aseg.nii.gz  dk.scgm_aeseg+tlrc
      3drefit -cmap INT_CMAP dk.scgm_aeseg+tlrc
    )
      
fi

