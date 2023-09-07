#!/bin/bash

# set -x

#exit immediately on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="gppi-seed-analysis"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

## the %P argument in printf prints the file's name with the name of
## the starting-point under which it was found removed.
## See https://man7.org/linux/man-pages/man1/find.1.html
subjects=$( cd ${SOURCE_DATA}; find ./ -regex '.*sub-[0-9][0-9][0-9]/ses-[a-z]*' -printf "%P\n" )

## echo ${subjects}

taskFile=$CODE_DIR/run/${task}.taskfile
info_message_ln "List of tasks to be executed is stored in $taskFile"

cat /dev/null > $taskFile

for ss in ${subjects} ; do
    subject=${ss%%/*}
    session=${ss##*/}
    info_message_ln "Subject: ${subject} Session: ${session}"
    
    echo "${CODE_DIR}/11-single-subject-gppi.sh --subject ${subject} --session ${session}" >> ${taskFile} 
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
sge_command="qsub -N ${task} -q ${queue} -j y -m n -V -wd $( pwd ) -o ${LOG_DIR} -t 1-${nTasks} -v OMP_NUM_THREADS=1" 
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
