#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task=mriqc

PROGRAM_NAME=`basename $0`

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=${ROOT}/data
RAW_DATA=${ROOT}/rawdata
SOURCE_DATA=${ROOT}/sourcedata
DERIVATIVE_DATA=${ROOT}/derivative
WORK_DIR=${ROOT}/work
LOG_DIR=${ROOT}/log
CODE_DIR=${ROOT}/code

if [[ ! -d ${WORK_DIR} ]] ; then
    mkdir -p ${WORK_DIR}
fi

GETOPT_OPTIONS=$( $GETOPT  -o "h:qs:" --longoptions "enqueue,threads:,subjects:" -n ${PROGRAM_NAME} -- "$@" )
exitStatus=$?
if [ $exitStatus != 0 ] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

# Note the quotes around `$GETOPT_OPTIONS': they are essential!
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-h|--threads)
	    threads=$2; shift 2 ;;
	-q|--enqueue)
	    enqueue=1; shift 1 ;;	
	-s|--subject)
	    subjects="$2"; shift 2 ;;
	--) 
	    shift ; break ;;

	*) 
	    error_message_ln "${PROGRAM_NAME}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

if [[ -z "${subjects}" ]] ; then 
    warn_message_ln "No subject ID(s) provided. Running MRIQC on all subjects"
fi

if [[ "x${threads}" == "x" ]] ; then
    threads=1
    warn_message_ln "No value for the number of parallel threads to use was provided. Defaulting to $threads"
else
    info_message_ln "Using threads value of ${threads}"
fi

newSubjects=""
for ss in ${subjects} ; do
    if [[ ${ss:0:4} != "sub-" ]] ; then
	ss="sub-${ss}"
    fi
    if [[ -z ${newSubjects} ]] ; then
	newSubjects="${ss}"
    else
	newSubjects="${newSubjects} ${ss}"
    fi
done
subjects="${newSubjects}"

if [[ ! -z ${subjects} ]] ; then 
    info_message_ln "Got the following subjects"
    (( sc=1 ))
    for ss in ${subjects} ; do
	msg=$( printf '[%02d]: %s\n' ${sc} "${ss}" )
	info_message_ln "${msg}"
	(( sc = sc + 1 ))
    done
    
    participantLabelArg="--participant-label ${subjects} "
fi


## info_message_ln "${dsets_me_run_arg}"
info_message_ln "Writing: ${outputScriptName}"

if [[ ! -d ${PIPELINE_DIR}/ ]] ; then
    mkdir -p ${PIPELINE_DIR}/
fi

outputScriptName=${CODE_DIR}/run/${task}.sh
info_message_ln "Writing script: $outputScriptName"

#the next is a here document, will take anything between them but not include them and in this case into the outputScriptName
    cat <<EOF > $outputScriptName
#!/bin/bash

set -x 

#$ -S /bin/bash

## activate the python mriqc virtual environment 
source ${HOME}/envs/mriqc/bin/activate

## mriqc --participant-label sub-105 -vvvvv --nprocs 4 --omp-nthreads 2  --no-datalad-get --no-sub  --notrack  ./sourcedata/ ./derivatives/mriqc participant
##ompthreads=$(( threads / 2 ))
ompthreads=1
mriqc ${participantLabelArg} -vvvvv --nprocs ${threads} \\
      --omp-nthreads  \${ompthreads} \\
      --mem 30 \\
      --no-datalad-get \\
      --no-sub \\
      --notrack \\
      --work-dir ${WORK_DIR} \\
      ${SOURCE_DATA} ${DERIVATIVE_DATA}/${task} participant
EOF

    chmod +x $outputScriptName
## note that the following is only relevant to the server and pass to the enqueue zero if on my mac
if [[ $enqueue -eq 1 ]] ; then
    q=parallel.q
    pe=smp
    info_message_ln "Submitting job for execution to queuing system"

    LOG_FILE=${LOG_DIR}/${task}.log
    info_message_ln "To see progress run: tail -f $LOG_FILE"

    rm -f ${LOG_FILE}
    qsub -N ${task} -q ${q} -pe ${pe} ${threads} -j y -m n -wd $( pwd )  -o ${LOG_FILE} $outputScriptName
    info_message_ln "Running qstat"
    qstat -q ${q}
else
    info_message_ln "Job *NOT* submitted for execution to queuing system."
    info_message_ln "Pass -q or --enqueue option to this script to do so."	
fi

