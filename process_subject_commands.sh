#!/bin/bash

# set -x

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

uofaz_regex="(sub-[0-9]{5})"
subjects=$( cd ../sourcedata; ls -1d sub-* | grep -v test )
echo $subjects
sessions="baseline followup"
for subject in ${subjects} ; do
    for ses in ${sessions} ; do
	session=ses-${ses}
	if [[ -d ../sourcedata/${subject}/${session} ]] ; then
	    ## ./04-preprocess-resting-state.sh --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q
	    ## ./04-preprocess-task-tapping.sh  --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q
	    if [[ ${subject} =~ ${uofaz_regex} ]] ; then
		echo "Subject: ${subject} Site:UA"
		./04-preprocess-task-tapping-regress-only-uofaz.sh  --subject ${subject} --session ${session} -h 4 -n -b 4 -q
	    else
		echo "Subject: ${subject} Site: FSU"		
		./04-preprocess-task-tapping-regress-only-fsu.sh --subject ${subject}  --session ${session} -h 4 -n -c 3 -b 4 -q
	    fi
	    ## ./04-preprocess-task-tapping-basis-tent.sh --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q
	   ## ./04-run-freesurfer-long.sh --subject ${subject} --session ${session} -h 2 -q 
	fi
    done
done

# for subject in ${subjects} ; do
#     ./03-run-freesurfer-base.sh -s ${subject} -h 2 -q
# done
