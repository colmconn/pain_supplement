#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement

programName=`basename $0`

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
RAW_DATA=$ROOT/rawdata
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative

CODE_DIR=${ROOT}/code

GETOPT_OPTIONS=$( $GETOPT  -o "e:s:" --longoptions "session:,subject:" -n ${programName} -- "$@" )
exitStatus=$?
if [ $exitStatus != 0 ] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

# Note the quotes around `$GETOPT_OPTIONS': they are essential!
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-s|--subject)
	    subjectNumber=$2; shift 2 ;;
	-e|--session)
	    session=$2; shift 2 ;;
	--) 
	    shift ; break ;;

	*) 
	    error_message_ln "${programName}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

if [ -z $subjectNumber ] ; then 
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

if [[ ${subjectNumber:0:4} != "sub-" ]] ; then
    subjectNumber="sub-${subjectNumber}"
fi

function reconstructWithDcm2niix {

    prefix=${subjectNumber}/${session}
    ## prefix=${subjectNumber}
    subjectDicomContainerDir=$( ls -1d $RAW_DATA/${prefix}/*/H* | head -1 )

    ## prefix=${prefix}/unprocessed
    
    if [[ -d $subjectDicomContainerDir ]] ; then
	if [[ ! -d $SOURCE_DATA/${prefix}/dcm2nii ]] ; then
	    info_message_ln "Making $SOURCE_DATA/${prefix}/dcm2nii"
	    mkdir -p $SOURCE_DATA/${prefix}/dcm2nii
	fi
	( cd $subjectDicomContainerDir/../ ; \
	  dcm2niix -o $SOURCE_DATA/${prefix}/dcm2nii -z y ${subjectDicomContainerDir##*/}
	)

	info_message_ln "Renaming all directories in $SOURCE_DATA/${prefix}/dcm2nii to be lower case"
	( cd   $SOURCE_DATA/${prefix}/dcm2nii;
	    for dd in $( find ./ -mindepth 1 -maxdepth 1 -type d | grep '[[:upper:]]' ) ; do 
		mv -f $dd $( echo $dd | tr '[:upper:]' '[:lower:]' )
	    done
	)
	
	info_message_ln "Renaming all NIfTI files in $SOURCE_DATA/${prefix}/dcm2nii to be lower case"
	( cd  $SOURCE_DATA/${prefix}/dcm2nii ;
	    find ./ -type f | grep '[[:upper:]]' | \
		while read ff ; do
		    dd=$( dirname $ff )
		    fn=$( basename $ff )
		    ( cd $dd; mv -f $fn  $( echo $fn | tr '[:upper:]' '[:lower:]' ) )
		done
	)
	# ( cd  $SOURCE_DATA/${prefix}/dcm2nii;
	#   ## the last number before the extension of the file is the
	#   ## order in which the sequences were run on the scanner so
	#   ## we can use that to sort list of files into the correct
	#   ## order of acquisition
	#   ls -1 |\
	#       gawk '{sq=gensub(/^(.*_)([0-9][0-9]*)(_[ie][0-9]*)?(_ph)?\.(json|nii\.gz|bvec|bval)$/, "\\2", "g", $1);  print sq, $1 ;}' |\
	#       sort -t ' ' -k 1n,1n > sequence_order.txt
	# )
    fi
}

function linkAnatomyFiles {
    scanType="$1"
    dicomTask="$2"

    dcm2niiDir=$SOURCE_DATA/${subjectNumber}/${session}/dcm2nii    
    dataDir=$SOURCE_DATA/${subjectNumber}/${session}/anat
    ## dcm2niiDir=$DERIVATIVE_DATA//${subjectNumber}/unprocessed/dcm2nii    
    ## dataDir=$DERIVATIVE_DATA//${subjectNumber}/unprocessed/anat
    
    if [[ ! -d $dataDir ]] ; then
	info_message_ln "Making $dataDir"
	mkdir -p $dataDir
    fi
    # get a list of unique anat file name roots (minus json or nii.gz extensions
    mris=( $( find $dcm2niiDir \
		   -type f -a \( -name "*${dicomTask}*.json" -o -name "*${dicomTask}*.nii*" \) |\
		  sed -e 's/\.\(json\|nii\|nii\.gz\)//g' |\
		  sort -u ) )

    if [[ ${#mris[@]} -eq 0 ]] ; then
	info_message_ln "No images found for scan type: ${scanType}"
    fi
    
    for (( ii=0 ; ii<${#mris[@]}; ii++ )) ; do
	prefix="${subjectNumber}_${session}"
	## prefix="${subjectNumber}"	
    
	case $scanType in
	    T1w) ;&
	    T2w)
		if python ${CODE_DIR}/get_json_value.py ImageType ${mris[$ii]}.json | grep -q NORM ; then
		    rec="prescannorm"
		else
		    rec="noprescannorm"
		fi
		prefix="${prefix}_rec-${rec}_${scanType}"
		;;
	    dwi)
		prefix="${prefix}_task-dir-${dir}_${task}_dwi"
		;;
	    asl)
		prefix="${prefix}_task-${task}_asl"
		;;
	    *)
		error_message_ln "Unknown scan type: $scanType. Exiting"
		exit
		;;
	esac

	info_message_ln "Linking ${mris[${ii}]}.json -> $dataDir/${prefix}.json"
	ln -sf -sf ${mris[${ii}]}.json $dataDir/${prefix}.json
	info_message_ln "Linking ${mris[${ii}]}.nii.gz -> $dataDir/${prefix}.nii.gz"
	ln -sf -sf ${mris[${ii}]}.nii.gz $dataDir/${prefix}.nii.gz
	
    done
}

function linkDwiOrAslOrFuncFiles {
    scanType="$1"
    dicomTask="$2"
    tasks="$3"
    nTasks=$( echo "${tasks}" | wc -w )
    tasks=($tasks)
    info_message_ln "Task(s): ${tasks[*]}"
    info_message_ln "Number of task(s): ${nTasks}"
    
    echosPerScan=${4:-1} ## set echosPerScan to the 4th positional
			 ## argument or default it to 1 if it is not
                         ## provided
    info_message_ln "Number of echos per scan: ${echosPerScan}"

    expectedVolumes="$5"
    info_message_ln "Number of expected volumns per scan: ${expectedVolumes}"    

    runCounter=1
    addRunIndex=0
    
    dcm2niiDir=$SOURCE_DATA/${subjectNumber}/${session}/dcm2nii    
    dataDir=$SOURCE_DATA/${subjectNumber}/${session}/$scanType
    ## dcm2niiDir=$DERIVATIVE_DATA/${subjectNumber}/unprocessed/dcm2nii    
    ## dataDir=$DERIVATIVE_DATA/${subjectNumber}/unprocessed/$scanType
    
    if [[ ! -d $dataDir ]] ; then
	info_message_ln "Making $dataDir"
	mkdir $dataDir
    fi
    # get a list of unique anat file name roots (minus json or nii.gz extensions
    mris=( $( find $dcm2niiDir \
		   -type f -a -name "*${dicomTask}*.nii*" |\
		  sed -e 's/\.\(json\|nii\|nii\.gz\)//g' |\
		  sort -t '_' -k 14n,14n ) )
    for (( ii=0 ; ii<${#mris[@]}; ii++ )) ; do
	msg="$( printf 'EPI File [%03d]: %s' ${ii} ${mris[${ii}]} )"
	info_message_ln "${msg}"
    done

    ## info_message_ln ${mris[@]}
    info_message_ln "Echos per file times nTasks: $(( echosPerScan * nTasks ))"
    info_message_ln "Number of EPI files: ${#mris[@]}"

    # if (( $(( echosPerScan * nTasks )) != ${#mris[@]} )) ; then
    # 	error_message_ln "Found more EPI task files than there are number of tasks times number of echos per file. Cannot continue."
    # 	exit 255
    # fi

    if [[ ${echosPerScan}  -ne ${#mris[@]} ]] ; then
	info_message_ln "More than one run found. Adding run index to filename"
	addRunIndex=1
    fi

    # the sort -t '_' -k 14n,14n splits the file name at field
    # seperators (_) and sorts on the 14th field.  so a filename such
    # as
    # heads_martin_lab_20211119_102020_304000_mb_3echo_bold_2.5_iso_cmmr_20211119102020_4_e1.nii.gz
    # will be sorted based on the digit 4 just before _e1.nii.gz
    
    # echo "MRIS: ${mris[*]}"
    if [[ ${#mris[@]} -eq 0 ]] ; then
	info_message_ln "No images found for scan type: $( echo $scanType | tr '[:lower:]' '[:upper:]' )"
	return 0
    fi

    info_message_ln "Linking in $( echo $scanType | tr '[:lower:]' '[:upper:]' ) images"
    task=${tasks[0]}
    for (( ii=0 ; ii<${#mris[@]}; ii++ )) ; do

	nVolumes=$( 3dinfo -nt ${mris[${ii}]}.nii.gz 2> /dev/null )
	if [[ ${nVolumes} -lt ${expectedVolumes} ]] ; then
	    warn_message_ln "Expecting ${expectedVolumes} volumes in ${mris[${ii}]}"
	    warn_message_ln "Got ${nVolumes}. Skipping this file"

	    continue
	fi
	
	## echo $( printf "%02d" ${ii}): ${mris[${ii}]}
	prefix="${subjectNumber}_${session}"
	# prefix="${subjectNumber}"	
	if [[ ! -z $task ]] ; then
	    prefix="${subjectNumber}_${session}_task-${task}"
	    ## prefix="${subjectNumber}_task-${task}"
	fi

	if [[ ${addRunIndex} -gt 0 ]] ; then
	    prefix="${prefix}_run-${runCounter}"
	fi

	if [[ ${mris[${ii}]} =~ ^.*(e([0-9]))$ ]] ; then
	    echoNumber=${BASH_REMATCH[2]}
	    prefix="${prefix}_echo-${echoNumber}"
	fi

	if [[ "$scanType" = "func" ]] ; then
	    prefix="${prefix}_bold"
	fi
	
	## info_message_ln "Prefix: ${prefix}"

	info_message_ln "Linking ${mris[${ii}]}.json -> $dataDir/${prefix}.json"
	ln -sf ${mris[${ii}]}.json $dataDir/${prefix}.json
	info_message_ln "Linking ${mris[${ii}]}.nii.gz -> $dataDir/${prefix}.nii.gz"
	ln -sf ${mris[${ii}]}.nii.gz $dataDir/${prefix}.nii.gz

	if [[ $scanType == "func" ]] ; then
	    info_message_ln "Getting slice timings from BIDS sidecar"
	    sliceTimes=$($CODE_DIR/get_bids_key_value.r -k "SliceTiming" -j  $dataDir/${prefix}.json )
	    if [[ ! -z ${sliceTimes} ]] ; then
		info_message_ln "Setting slice timings with 3drefit"
		## info_message_ln "Slice times: ${sliceTimes}"

		info_message_ln "Run: ${runCounter} Echo: ${echoNumber} Slice Times: ${sliceTimes}"
		
		## BIDS stores slice timings in seconds and 3drefit
		## expects them to be in seconds (unless a scale is
		## specified) so no conversion is needed
		##
		## 3drefit -Tslices ${sliceTimes} $dataDir/${prefix}.nii.gz

		## 2019-09-26: We could run 3drefit here but it's more
		## efficient to extract the slice times from the BIDS
		## sidecar and supply a filename containing them as a
		## argument to afni_proc.py
	    else
		warn_message_ln "No slice timings found in BIDS sidecar"
	    fi
	fi
	
	# if [[ "$scanType" = "dwi" ]] ; then
	#     ln -sf ${mris[${ii}]}.bval $dataDir/${prefix}.bval
	#     ln -sf ${mris[${ii}]}.bvec $dataDir/${prefix}.bvec
	# fi

	if (( $(( ii + 1 )) % ${echosPerScan} == 0 )) ; then
	    (( runCounter=runCounter+1 ))
	    task=${tasks[$(( runCounter - 1 ))]}
	fi
    done
}

####################################################################################################
noDataDir=""
existingDataDirs=""

info_message_ln "####################################################################################################"
info_message_ln "### Converting DICOMS for $subjectNumber/${session}"


# if [[ ! -d $DERIVATIVE_DATA/$group/${subjectNumber} ]] ; then
#     info_message_ln "Making the subject's directory in the study data folder: $DERIVATIVE_DATA/$group/${subjectNumber}"
#     mkdir -p $DERIVATIVE_DATA/$group/${subjectNumber}
# fi

info_message_ln "****************************************************************************************************"
info_message_ln "Using dcm2niix for conversion"

prefix=${subjectNumber}/${session}
## prefix=${subjectNumber}
rm -fr $DERIVATIVE_DATA/$prefix
# mkdir  $DERIVATIVE_DATA/$prefix

reconstructWithDcm2niix ## $subjectNumber "test"


info_message_ln "****************************************************************************************************"
info_message_ln "Linking T1 and T2 anatomy files"
linkAnatomyFiles "T1w" "t1_mprage_sag_iso_.8mm"
linkAnatomyFiles "T2w" "t2_space_sag_p4_iso"


info_message_ln "****************************************************************************************************"
info_message_ln "Linking REST task files"
echosPerScan=3
volumesPerRun=260
linkDwiOrAslOrFuncFiles "func" "mb_3echo_rest_2.5_iso_cmmr" "rest" ${echosPerScan} ${volumesPerRun}

info_message_ln "Linking TAPPING task files"
volumesPerRun=263
linkDwiOrAslOrFuncFiles "func" "mb_3echo_run?_2.5_iso_cmmr" "tapping tapping" ${echosPerScan} ${volumesPerRun}


