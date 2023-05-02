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
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-anat
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}
[[ ! -d ${PIPELINE_DIR} ]] && mkdir -p ${PIPELINE_DIR}

## enqueue the job for execution ; 1 means queue, zero means do not
## queue; on server a queing system, can queu system for jobs to run
## in parallel, if leave it set as zero it will not, just create
## script.  If set to 1, will submit script to queuing system so that
## it will be 1.  if running multiple subjects, can tell it to submit
## it and queue will run them in order.  1 means "make the scripts and
## run in parallel"
enqueue=0

GETOPT_OPTIONS=$( $GETOPT  -o "e:h:j:nqs:" --longoptions "enqueue,jobid:,noisy,session:,subject:,threads:" -n ${PROGRAM_NAME} -- "$@" )
exitStatus=$?
if [ $exitStatus != 0 ] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

## do not do @NoisySkullStrip by default
noisy=0

# Note the quotes around `$GETOPT_OPTIONS': they are essential!
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-e|--session)
	    session=$2; shift 2 ;;
	-h|--threads)
	    threads=$2; shift 2 ;;
	-j|--jobid)
	    jobid=$2; shift 2 ;;	
	-n|--noisy)
	    noisy=1; shift 1 ;;
	-q|--enqueue)
	    enqueue=1; shift 1 ;;	
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

if [[ "x${threads}" == "x" ]] ; then
     threads=1
     warn_message_ln "No value for the number of parallel threads to use was provided. Defaulting to $threads"
else
    info_message_ln "Using threads value of ${threads}"
fi

## Use @NoisySkullStrip
if [[ ${noisy} -eq 1 ]] ; then
    info_message_ln "Using @NoisySkullStrip"
fi

prefix=${subject}/${session}
ss=${subject}_${session}
queue=parallel.q
nslots=${threads}
pe="-pe smp ${nslots}"

info_message_ln "####################################################################################################"
info_message_ln "### Subject: $subject Session: ${Session}"

if [[ ! -z ${jobid} ]] ; then
    holdJid="-hold_jid $jobid"
fi

btemplate=MNI152_2009_template_SSW.nii.gz    
tpath=$( @FindAfniDsetPath ${btemplate} )

anat_script=${CODE_DIR}/run/${ss}_preprocess-anat-afni.sh
info_message_ln "Writing ${anat_script}"

if [[ ! -d ${PIPELINE_DIR}/ ]] ; then
    mkdir -p ${PIPELINE_DIR}/
fi

cat <<EOF > ${anat_script}
#!/bin/bash
# set -x
set -e

## only use a single thread since we're going to run so many subjects
## in parallel
export OMP_NUM_THREADS=${threads}
export AFNI_MESSAGE_COLORIZE=NO

if [[ ! -d ${PIPELINE_DIR}/${prefix}/ ]] ; then
    mkdir -p ${PIPELINE_DIR}/${prefix}/
fi

if [[ ${noisy} -eq 1 ]] ; then
cd ${PIPELINE_DIR}/${prefix}/
3dcopy ${SOURCE_DATA}/${prefix}/anat/${ss}_rec-prescannorm_T1w.nii.gz \\
       anat_orig+orig
@NoisySkullStrip -input anat_orig+orig

@SSwarper \\
	  -odir  ${PIPELINE_DIR}/${prefix}/ \\
	  -input ${SOURCE_DATA}/${prefix}/anat/${ss}_rec-prescannorm_T1w.nii.gz \\
	  -mask_ss anat_orig.ns+orig.HEAD \\
	  -base  ${tpath}/${btemplate} \\
	  -subid ${ss}

else 
@SSwarper \\
	  -odir  ${PIPELINE_DIR}/${prefix}/ \\
	  -input ${SOURCE_DATA}/${prefix}/anat/${ss}_rec-prescannorm_T1w.nii.gz \\
	  -base  ${tpath}/${btemplate} \\
	  -subid ${ss}
fi
##
## This script was generated on $( date ) by invoking the following command on $( uname -n ):
## ${FULL_COMMAND_LINE}
##	
echo "FINISHED:  ${FULL_COMMAND_LINE}"
EOF
chmod +x ${anat_script}
if [[ $enqueue -eq 1 ]] ; then
    info_message_ln "Submitting job for execution to queuing system"
    LOG_FILE=${LOG_DIR}/${ss}_preprocess-anat-afni.log
    info_message_ln "To see progress run: tail -f $LOG_FILE"
    rm -f ${LOG_FILE}
    qsub -N ${subject}_${session}_anat -q ${queue} ${pe} ${holdJid} \
	 -j y -m n -V \
	 -wd $( pwd ) \
	 -S /bin/bash -o ${LOG_FILE} ${anat_script}

    info_message_ln "Running qstat"
    qstat -q ${queue}

else
    info_message_ln "Job *NOT* submitted for execution to queuing system."
    info_message_ln "Pass -q or --enqueue option to this script to do so."	
fi

