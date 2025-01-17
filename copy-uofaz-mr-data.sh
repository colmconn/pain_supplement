#!/bin/bash

# set -x

#exit immediately on an error
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

BOX_DIR="${HOME}/mnt/uofazbox/MRI data"

subjects=( $( cd "${BOX_DIR}" ; find  ./ -maxdepth 1 -name 'ID[0-9][0-9][0-9][0-9][0-9]_[0-9]' | sed 's#\./##g' | sort ) )
##subjects="ID10207_1"

for sub in ${subjects[@]} ; do
    # if [[ ${sub} == "ID10207_1" ]] ; then
    # 	warn_message_ln "Skipping ID10207_1"
    # 	continue
    # fi
    
    subject=${sub##ID}
    subject=${subject%%_*}
    session=${sub##*_}
    if [[ ${session} == "0" ]]; then
	session="baseline"
    elif [[ ${session}="1" ]]; then
	session="followup"
    else
	session="unknown"
    fi

    ./00-make-new-subject-session-dirs.sh --subject ${subject} --session ${session}
    info_message_ln "Copying ${ROOT}/uofaz_data/${sub}/. to ${RAW_DATA}/sub-${subject}/ses-${session}"
    cp -ra "${ROOT}/uofaz_data/${sub}/." ${RAW_DATA}/sub-${subject}/ses-${session}

    if [[ ${subject} == "10186" ]] && [[ ${session} == "followup" ]] ; then
	info_message_ln "Cleaning sub-${subject} ses-${session}"
	( cd ${RAW_DATA}/sub-${subject}/ses-${session}/AHN* ;
	  rm -fr *0007
	)
    elif [[ ${subject} == "10189" ]] && [[ ${session} == "baseline" ]] ; then
	info_message_ln "Cleaning sub-${subject} ses-${session}"	
	( cd ${RAW_DATA}/sub-${subject}/ses-${session}/AHN* ;
	  rm -fr *0009 *0010
	)
    elif [[ ${subject} == "10217" ]] && [[ ${session} == "baseline" ]] ; then
	info_message_ln "Cleaning sub-${subject} ses-${session}"	
	( cd ${RAW_DATA}/sub-${subject}/ses-${session}/AHN* ;
	  rm -fr *0009
	)
    fi
    
    ( cd ${RAW_DATA}/sub-${subject}/ses-${session}/AHN* ;
      info_message_ln "Renaming task EPI directories for sub-${subject} ses-${session}"
      ## the sed 1d deletes the first line as it will always be rest
      ## and does not need to be edited
      tasks=$( ls -1 | awk -F '_' '{print $NF,$0}' | sort -k1n | cut -d' ' -f2- | grep MB_3ECHO | sed '1d' )
      nn=1
      for tt in ${tasks} ; do
	  nrun="run${nn}"
	  newname=$( echo ${tt} | sed "s/REST/run${nn}/" )
	  mv -f ${tt} ${newname}
    	  ## We need to modify the DICOMs so that the "Series
	  ## Description" tag matches what is should for the task
	  ## runs if they were named properly.  This makes using
	  ## dcm2bids much easier as it matches on DICOM tags not
	  ## the name of the directory containing the DISOM files
	  ( cd ${newname};
	    info_message_ln "Editing DICOM files for run ${nn}"
	    old_series_desc=$( dicom_hinfo -tag 0008,103e -no_name $( ls -1 | head -1 ) )
	    new_series_desc=$( echo ${old_series_desc} | sed "s/rest/run${nn}/")
	    dcmodify --no-backup --modify "(0008,103e)=${new_series_desc}" *.IMA
	  )
	  (( nn=nn+1 ))
      done
    )
done
