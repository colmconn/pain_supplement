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

UOFAZ_DIR="${ROOT}/uofaz_data"

tasks="initialPainRating kneeTapping"

for task in ${tasks} ; do
    info_message_ln "################################################################################"
    info_message_ln "Copying behavioral data for the ${task} task"
    
    (
	missing=""
	cd "${UOFAZ_DIR}/Nursing Pain R01 supplement UofAz/${task}/data"
	for ff in sub-* ; do
	    echo $ff
	    ## sub-10205_1_ses-week 2_
	    if [[ ${ff} =~ ^(sub-10[0-9][0-9][0-9])_(([01])_)?(ses-[ a-zA-Z0-9]*)(.*) ]] ; then
		subject=${BASH_REMATCH[1]}
		if [[ "${BASH_REMATCH[4]}" == "ses-baseline" ]] ; then
		    info_message_ln "BASELINE session"
		    session="ses-baseline"		    
		else
		    info_message_ln "FOLLOWUP session"
		    session="ses-followup"		    
		fi
		rest=${BASH_REMATCH[5]}
		if [[ -d ${RAW_DATA}/${subject}/${session} ]] ; then
		    echo cp "${ff}" ${RAW_DATA}/${subject}/${session}/beh/${task}/${subject}_${session}${rest}
		    cp "${ff}" ${RAW_DATA}/${subject}/${session}/beh/${task}/${subject}_${session}${rest}		    
		else
		    warn_message_ln "No such directory: ${RAW_DATA}/${subject}/${session}"
		    missing="${missing} ${subject}/${session}"
		fi
		# info_message_ln "Matched!"
		# for ii in 1 2 3 4 5; do
		#     mm=$( printf "BASH_REMATCH[%2d]=%s" ${ii} "${BASH_REMATCH[${ii}]}" )
		#     info_message_ln "${mm}"
		# done
		
	    else
		warn_message_ln "NO Match!"
	    fi
	done
	missing=$( echo ${missing} | tr ' ' '\n' | uniq | tr '\n' ' ' )
	warn_message_ln "Missing: ${missing}"	
    )
done
