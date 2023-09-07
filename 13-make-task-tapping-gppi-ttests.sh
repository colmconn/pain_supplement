#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"
ttests_task="${task}-gppi-ttests"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
TASK_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
TASK_TTEST_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}-ttests
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${ttests_task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}
[[ ! -d ${PIPELINE_DIR} ]] && mkdir -p ${PIPELINE_DIR}

btemplate=MNI152_2009_template_SSW.nii.gz    
tpath=$( @FindAfniDsetPath ${btemplate} )
if [[ ${tpath} == "" ]] ; then 
   error_message_ln "Couldn't find path to MNI152_2009_template_SSW.nii.gz template."
   error_message_ln "Exiting"
   exit 1
fi

subjects=( $( cd ${SOURCE_DATA} ; find  ./ -maxdepth 1 -name 'sub-[0-9][0-9][0-9]' | sed 's#\./##g' | sort ) )
if [[ -z ${subjects} ]] ; then
    error_message_ln "Found no subject directories in ${SOURCE_DATA}"
    error_message_ln "Quitting"
    exit
fi

outputDir=$( cd ${TASK_PIPELINE_DIR}/${subject[0]}; find ./ -type d -name ${task}-preprocessed-* | head -1 )
outputDir=${outputDir##*/}
## echo ${outputDir}

seeds=$( ls ${TASK_TTEST_PIPELINE_DIR}/roi*+tlrc.HEAD )
if [[ -z ${seeds} ]] ; then
    error_message_ln "Found no seeds in ${TTEST_PIPELINE_DIR}"
    exit
fi

## link in the MNI template for use as an underlay 
(  cd ${PIPELINE_DIR}; ln -sf $tpath/$btemplate . )

taskFile=$CODE_DIR/run/${ttests_task}.taskfile
info_message_ln "List of tasks to be executed is stored in $taskFile"
cat /dev/null > $taskFile

((seedCount=1 ))
for seed in ${seeds} ; do
    sc_pp=$( printf "%02d" ${seedCount} )
    seedName=$( cat ${TASK_TTEST_PIPELINE_DIR}/clusters.locations.baseline-followup.csv | \
		    awk -F"," -v rr=${seedCount} '{print $rr}' | \
		    tr -d '()' )
    
    msg=$( printf "Making t-tests for seed %s : %s" ${sc_pp} ${seedName} )
    info_message_ln "${msg}"

    ## sets is an associative array
    declare -A sets
    sets[baseline]=""
    sets[followup]=""
    for subject in ${subjects[*]} ; do
	if [[ ${subject} == "sub-105" ]] ; then
	    continue
	fi
	if [[ -f ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/stats.${subject}_ses-baseline_REML+tlrc.HEAD ]] && \
	       [[ -f ${TASK_PIPELINE_DIR}/${subject}/ses-followup/${outputDir}/stats.${subject}_ses-followup_REML+tlrc.HEAD ]] ; then
	    if [[ ! -f  ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/00_DO_NOT_ANALYSE_${subject}_ses-baseline_20percent.txt  ]] && 
		   [[ ! -f  ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/00_DO_NOT_ANALYSE_${subject}_ses-followup_20percent.txt  ]] ; then
		sets[baseline]="${sets[baseline]} ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/count-${sc_pp}_seed-${seedName}.PPI.full.stats.${subject}_ses-baseline_REML+tlrc.HEAD"
		sets[followup]="${sets[followup]} ${TASK_PIPELINE_DIR}/${subject}/ses-followup/${outputDir}/count-${sc_pp}_seed-${seedName}.PPI.full.stats.${subject}_ses-followup_REML+tlrc.HEAD"
	    fi
	fi
    done
    
    cd ${PIPELINE_DIR}
    datatableFile=datatable_count-${sc_pp}_seed-${seedName}.tsv
    echo "Subj session InputFile" > ${datatableFile}
    ## for session in "${!sets[@]}" ; do
    for session in baseline followup ; do
	for inputfile in ${sets[${session}]} ; do
	    subject=$( echo ${inputfile} | sed -e "s#.*\(sub-[0-9][0-9][0-9]\)_.*#\1#" )
	    echo "${subject} ${session} ${inputfile}" >> ${datatableFile}
	done
    done

    if [[ $verbose==1 ]] ;then
	(( ii=1 ))
	info_message_ln "[SS]: Baseline Subject File"
	for sfile in ${sets[baseline]} ; do
	    msg="$( printf '[%02d] %s' ${ii} ${sfile} )"
	    info_message_ln "${msg}"
	    (( ii=ii+1 ))
	done
	(( ii=1 ))
	info_message_ln "[SS]: Followup Subject File"
	for sfile in ${sets[followup]} ; do
	    msg="$( printf '[%02d] %s' ${ii} ${sfile} )"
	    info_message_ln "${msg}"
	    (( ii=ii+1 ))
	done
    fi

    if [[ ! -f Bmask_epi+tlrc.HEAD ]] ; then
	info_message_ln "Making brain mask dataset"
	first_stats_file=$( echo ${sets[baseline]} | awk '{print $1}' )
	3dbucket -prefix Bmask $tpath/$btemplate'[Bmask]' $tpath/$btemplate 
	3dresample -rmode NN -master ${first_stats_file} -input Bmask+tlrc -prefix Bmask_epi+tlrc
    fi
    cat  <<EOF >> ${taskFile}
    3dttest++ -setA ${sets[baseline]} \
	      -setB ${sets[followup]} \
	      -labelA baseline \
	      -labelB followup \
	      -paired \
	      -mask Bmask_epi+tlrc.HEAD \
	      -prefix count-${sc_pp}_seed-${seedName}_${ttests_task} \
	      -Clustsim
EOF
    
    (( seedCount=seedCount+1 ))
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
sge_command="qsub -N ${ttests_task} -q ${queue} -j y -m n -V -wd ${PIPELINE_DIR} -o ${LOG_DIR} -t 1-${nTasks} -v OMP_NUM_THREADS=2" 
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
