
subject=176
session=baseline
echo ${subject}
echo ${session}
			    
./00-make-new-subject-session-dirs.sh -s ${subject} -e ${session}
./01-convertDicoms.sh -s ${subject} -e ${session}

./02-run-freesurfer.sh -s ${subject} -e ${session} -h 4 -q
./03-preprocess-anat-afni.sh -s ${subject} -e ${session} -h 4 -q

## wait for the above commands to complete before running EPI procesing
./04-preprocess-resting-state.sh --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q
./04-preprocess-task-tapping.sh --subject ${subject} --session ${session} -h 4 -n -c 3 -b 4 -q

