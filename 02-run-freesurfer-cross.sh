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
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}

## enqueue the job for execution ; 1 means queue, zero means do not
## queue; on server a queing system, can queu system for jobs to run
## in parallel, if leave it set as zero it will not, just create
## script.  If set to 1, will submit script to queuing system so that
## it will be 1.  if running multiple subjects, can tell it to submit
## it and queue will run them in order.  1 means "make the scripts and
## run in parallel"
enqueue=0

GETOPT_OPTIONS=$( $GETOPT  -o "e:h:qs:" --longoptions "session::,threads::,enqueue,subject::" -n ${PROGRAM_NAME} -- "$@" )
exitStatus=$?
if [ $exitStatus != 0 ] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

# Note the quotes around `$GETOPT_OPTIONS': they are essential!
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-e|--session)
	    session=$2; shift 2 ;;
	-h|--threads)
	    threads=$2; shift 2 ;;
	-q|--enqueue)
	    enqueue=1; shift 1 ;;
	-s|--subject)
	    subjectNumber=$2; shift 2 ;;
	
	--) 
	    shift ; break ;;

	*) 
	    error_message_ln "${PROGRAM_NAME}: ${1}: invalid option" >&2
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

if [[ "x${threads}" == "x" ]] ; then
     threads=1
     warn_message_ln "No value for the number of parallel threads to use was provided. Defaulting to $threads"
else
    info_message_ln "Using threads value of ${threads}"
fi

prefix=${subjectNumber}/${session}
ss=${subjectNumber}_${session}
t1w=${SOURCE_DATA}/${prefix}/anat/${ss}_rec-prescannorm_T1w.nii.gz
t2w=${SOURCE_DATA}/${prefix}/anat/${ss}_rec-prescannorm_T2w.nii.gz
freesurfer_script=${CODE_DIR}/run/${ss}_freesurfer.sh
info_message_ln "Writing ${freesurfer_script}"

cat <<EOF > ${freesurfer_script}
#!/bin/bash

#$ -S /bin/bash

FREESURFER_HOME=/home/colmconn/Applications/freesurfer-7.0/
SUBJECTS_DIR=$DERIVATIVE_DATA/freesurfer-7.0/
[[ ! -d \${SUBJECTS_DIR} ]] && mkdir \${SUBJECTS_DIR}

export OMP_NUM_THREADS=${threads}
export FREESURFER_HOME SUBJECTS_DIR
source \$FREESURFER_HOME/SetUpFreeSurfer.sh

recon-all -subject ${ss} -i ${t1w} -T2 ${t2w} -T2pial -all

@SUMA_Make_Spec_FS \\
		   -NIFTI \\
		   -fspath \${SUBJECTS_DIR}/${ss} \\
		   -sid    ${ss}
 
##
## This script was generated on $( date ) by invoking the following command on $( uname -n ):
## ${FULL_COMMAND_LINE}
##	
echo "FINISHED:  ${FULL_COMMAND_LINE}"
EOF
chmod +x ${freesurfer_script}
if [[ $enqueue -eq 1 ]] ; then
    q=parallel.q
    pe=smp
    info_message_ln "Submitting job for execution to queuing system"
    LOG_FILE=${LOG_DIR}/${ss}_freesurfer.log
    info_message_ln "To see progress run: tail -f $LOG_FILE"
    rm -f ${LOG_FILE}

    qsub -N ${ss}_freesurfer -q ${q} -pe ${pe} ${threads} -j y -m n -V \
	 -wd $( pwd ) -o ${LOG_FILE} ${freesurfer_script}

    info_message_ln "Running qstat"
    qstat -q ${q}

else
    info_message_ln "Job *NOT* submitted for execution to queuing system."
    info_message_ln "Pass -q or --enqueue option to this script to do so."	
fi

