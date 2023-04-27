#!/bin/bash

#set -x 

# if ctrl-c is typed exit immediatly
trap exit SIGHUP SIGINT SIGTERM

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


structuralQaFile=$(pwd)/structural-qa-$( date +"%Y%m%d" ).csv

if [[ -f ${structuralQaFile} ]] ; then
    tail +2 ${structuralQaFile} | \
	while read qaline ; do
	    IFS=', ' read -r -a line <<< "${qaline}"
	    subject=${line[0]}
	    session=${line[1]}
	    above=${line[3]}
	    below=${line[4]}
	    if [[ ${line[2]} == "N" ]] ; then
		info_message_ln "Subject: ${subject} Session: ${session}"
		info_message_ln "Above  : ${above} Below  : ${below}"

		for rec in noprescannorm prescannorm ; do
		    bids_link=${SOURCE_DATA}/${subject}/${session}/anat/${subject}_${session}_rec-${rec}_T1w.nii.gz
		    orig_file=$( readlink -f ${bids_link} )
		    
		    (
			cd $( dirname ${orig_file} )
			## @clip_volume -input ${orig_file} -above ${above} -below ${below}
			## 
			## @clip_volume doesn't handle NIfTI files so
			## run the 3dcalc command here instead
			3dcalc -a ${orig_file} \
			       -expr "a * step(step((${above}) - z) * step(z - (${below})))" \
			       -rai \
			       -prefix ${orig_file%%.nii.gz}_clp.nii
			if [[ -f ${orig_file%%.nii.gz}_clp.nii ]] ; then
			    gzip -f ${orig_file%%.nii.gz}_clp.nii
			fi
			ln -sf $( basename ${orig_file%%.nii.gz}.json ) ${orig_file%%.nii.gz}_clp.json
			cd ../anat
			rm -f ${subject}_${session}_rec-${rec}_T1w.nii.gz
			rm -f ${subject}_${session}_rec-${rec}_T1w.json

			ln -sf ${orig_file%%.nii.gz}_clp.nii.gz ${subject}_${session}_rec-${rec}_T1w.nii.gz
			ln -sf ${orig_file%%.nii.gz}_clp.json  ${subject}_${session}_rec-${rec}_T1w.json			
		    )

		done
	    fi
	done
fi

