#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

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
PIPELINE_DIR=${DERIVATIVE_DATA}/fsl-6.0-vbm
ANAT_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}
[[ ! -d ${PIPELINE_DIR} ]] && mkdir -p ${PIPELINE_DIR}

taskFile=$CODE_DIR/run/fslvbm-prep.taskfile
info_message_ln "List of tasks to be executed is stored in $taskFile"
cat /dev/null > $taskFile

subjects=( $( cd ${SOURCE_DATA} ; find  ./ -maxdepth 1 -name 'sub-[0-9][0-9][0-9]' | sed 's#\./##g' | sort ) )

if [[ -z ${subjects} ]] ; then
    error_message_ln "Found no subject directories in ${SOURCE_DATA}"
    error_message_ln "Quitting"
    exit
fi

if [[ ! -d ${PIPELINE_DIR}/struc ]] ; then
    mkdir -p ${PIPELINE_DIR}/struc
fi

for subject in ${subjects[@]} ; do

    if [[ ! -d ${ANAT_PIPELINE_DIR}/${subject}/ses-baseline ]] ||
       [[ ! -d ${ANAT_PIPELINE_DIR}/${subject}/ses-followup ]] ; then
	continue
    fi
    for session in ses-baseline ses-followup ; do
	info_message_ln "Linking ${subject} : ${session}"

	# ( cd ${PIPELINE_DIR};
	#   ln -sf ${ANAT_PIPELINE_DIR}/${subject}/${session}/anat_cp.${subject}_${session}.nii ${subject}_${session}.nii )
	## ln -sf ${ANAT_PIPELINE_DIR}/${subject}/${session}/anatSS.${subject}_${session}.nii  ${subject}_${session}_brain.nii )
	# ( cd ${PIPELINE_DIR}/struc;
	#   ln -sf ../${subject}_${session}.nii . ) 
	echo "$FSLDIR/bin/bet ${subject}_${session}_struc.nii.gz ${subject}_${session}_struc_brain -f 0.4 -B" >> ${taskFile}
    done
done


## jobname
#$ -N $taskName

## queue
#$ -q all.q

## binary? 
#$ -b y

## rerunnable?
#$ -r y

## merge stdout and stderr?
#$ -j y

## send no mail
#$ -m n

## execute from the current working directory
#$ -cwd

## use a shell to run the command
#$ -shell yes 

## set the shell
#$ -S /bin/bash

## preserve environment
#$ -V 

[[ ! -d $LOG_DIR ]] && mkdir $LOG_DIR
queue=parallel.q
nTasks=$( cat $taskFile | wc -l )
sge_command="qsub -N fslvbm-prep -q ${queue} -j y -m n -V -wd ${PIPELINE_DIR}/struc -o ${LOG_DIR} -t 1-${nTasks} -v OMP_NUM_THREADS=2" 
echo $sge_command
( exec $sge_command <<EOF
#!/bin/bash

#$ -S /bin/bash

command=\`sed -n -e "\${SGE_TASK_ID}p" $taskFile\`

exec /bin/sh -c "\$command"
EOF
)

echo "Running qstat"
qstat -q ${queue}
