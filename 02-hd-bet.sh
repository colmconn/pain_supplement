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
DERIVATIVE_DATA=$ROOT/derivatives
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

GETOPT_OPTIONS=$( $GETOPT  -o "qs:" --longoptions "enqueue:,suffix::" -n ${PROGRAM_NAME} -- "$@" )
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
	-s|--suffix)
	    suffix=$2; shift 2 ;;	
	--) 
	    shift ; break ;;

	*) 
	    error_message_ln "${PROGRAM_NAME}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

threads=1

if [[ "x${suffix}" == "x" ]] ; then
     suffix="T1w"
     warn_message_ln "No value for the number of parallel suffix to use was provided. Defaulting to $suffix"
else
    info_message_ln "Using suffix value of ${suffix}"
fi

hd_bet_script=${CODE_DIR}/run/hd_bet.sh
info_message_ln "Writing ${hd_bet_script}"


cat <<EOF > ${hd_bet_script}
#!/bin/bash

#$ -S /bin/bash

source ${HOME}/envs/hd-bet/bin/activate

if ! command -v hd-bet &> /dev/null
then
    echo "The hd-bet command could not be found."
    echo "Install it with pip install hd-bet."    
    exit
fi

#export FSLDIR=${HOME}/Applications/fsl-6.0
#source \${FSLDIR}/etc/fslconf/fsl.sh

# if ! command -v fslreorient2std &> /dev/null
# then
#     echo "The fslreorient2std command could not be found."
#     exit
# fi

mkdir -p ${DERIVATIVE_DATA}/hd-bet/{input,output}
cd ${DERIVATIVE_DATA}/hd-bet/

cd input
find ${SOURCE_DATA} -name "*_rec-prescannorm_${suffix}.nii.gz" -print0 | xargs -0 -I '{}' ln -sf '{}' .
cd ../

hd-bet -i input -o output --save_bet_mask --verbose

##
## This script was generated on $( date ) by invoking the following command on $( uname -n ):
## ${FULL_COMMAND_LINE}
##	
echo "FINISHED:  ${FULL_COMMAND_LINE}"
EOF
chmod +x ${hd_bet_script}
if [[ $enqueue -eq 1 ]] ; then
    task=run_hd_bet_${ss}
    queue=localQ
    info_message_ln "Submitting job for execution to queuing system"
    LOG_FILE=${LOG_DIR}/hd_bet.log
    info_message_ln "To see progress run: tail -f $LOG_FILE"
    rm -f ${LOG_FILE}
    sbatch --job-name ${task} \
	   --partition ${queue} \
	   --export=ALL \
	   --mail-type=NONE \
	   --chdir $( pwd ) \
	   --output ${LOG_FILE} \
	   --ntasks=1 \
	   --cpus-per-task=${threads} \
	   ${hd_bet_script}

    info_message_ln "Running squeue"
    squeue -ar

else
    info_message_ln "Job *NOT* submitted for execution to queuing system."
    info_message_ln "Pass -q or --enqueue option to this script to do so."	
fi

