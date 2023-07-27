#!/bin/bash

##if [[ 1 == 0 ]] ; then 

# subject=176
# session=baseline
# echo ${subject}
# echo ${session}

# ./00-make-new-subject-session-dirs.sh -s ${subject} -e ${session}
# ./01-convertDicoms.sh -s ${subject} -e ${session}

# ./02-run-freesurfer.sh -s ${subject} -e ${session} -h 4 -q
# ./03-preprocess-anat-afni.sh -s ${subject} -e ${session} -h 4 -q

# ## wait for the above commands to complete before running EPI procesing
# ./04-preprocess-resting-state.sh --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q
# ./04-preprocess-task-tapping.sh --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q


subjects=$( cd ../sourcedata; ls -1d sub-1* )

for subject in ${subjects} ; do
    for ses in baseline followup ; do
	session=ses-${ses}
	if [[ -d ../sourcedata/${subject}/${session} ]] ; then
	    ## ./04-preprocess-resting-state.sh --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q
	    ## ./04-preprocess-task-tapping.sh  --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q
	    ./04-preprocess-task-tapping-basis-tent.sh --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q
	fi
    done
done
