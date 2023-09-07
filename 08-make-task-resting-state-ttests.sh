#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="resting-state"
ttests_task="${task}-ttests"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${STUDY_ROOT:-/data/colmconn/$studyName}

DATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
TASK_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
CORR_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}-corr
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${ttests_task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

[[ ! -d ${LOG_DIR} ]] && mkdir -p ${LOG_DIR}
[[ ! -d ${PIPELINE_DIR} ]] && mkdir -p ${PIPELINE_DIR}

GETOPT_OPTIONS=$( $GETOPT  -o "h:l:" \
			   --longoptions "threads:,seedlist:" \
			   -n ${PROGRAM_NAME} -- "$@" )
exitStatus=$?
if [[ $exitStatus != 0 ]] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

# Note the quotes around `$GETOPT_OPTIONS': they are essential!
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-h|--threads)
	    threads=$2; shift 2 ;;
	-l|seedlist)
	    seedList="$2"; shift 2 ;;
	--) 
	    shift ; break ;;

	*) 
	    error_message_ln "${programName}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

if [[ "x${threads}" == "x" ]] ; then
     threads=1
     warn_message_ln "No value for the number of parallel threads to use was provided. Defaulting to $threads"
else
    info_message_ln "Using threads value of ${threads}"
fi

if [[ "x$seedList" == "x" ]] ; then
    error_message_ln "No value provided for seedlist. Exiting."
    exit
fi

sl=""
for ff in ${seedList} ; do
    if [[ -f $ff ]] ; then
	info_message_ln "File exists: ${ff}"
	sl="${sl} ${ff}"
    else
	warn_message_ln "No such file: ${ff}"
    fi
done
if [[ -z ${sl} ]] ; then
    error_message_ln "The seed list file(s) do not exit. Exiting."
    exit
fi
seedList=${sl}

seeds=$( eval echo $( cat ${seedList} ) )

info_message_ln "Data for the following seeds will be t-tested:"
nn=1
for seed in ${seeds}; do
    msg=$( printf "[%03d]: %s" ${nn} ${seed} )
    info_message_ln "${msg}"
    (( nn=nn+1 ))
done


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

## sets is an associative array
declare -A sets
sets[baseline]=""
sets[followup]=""
for subject in ${subjects[*]} ; do
    if [[ -f ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/errts.${subject}_ses-baseline_REML+tlrc.HEAD ]] && \
       [[ -f ${TASK_PIPELINE_DIR}/${subject}/ses-followup/${outputDir}/errts.${subject}_ses-followup_REML+tlrc.HEAD ]] ; then
	if [[ ! -f  ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/00_DO_NOT_ANALYSE_${subject}_ses-baseline_20percent.txt  ]] && 
	   [[ ! -f  ${TASK_PIPELINE_DIR}/${subject}/ses-baseline/${outputDir}/00_DO_NOT_ANALYSE_${subject}_ses-followup_20percent.txt  ]] ; then 
	    sets[baseline]="${sets[baseline]} ${subject}_ses-baseline"
	    sets[followup]="${sets[followup]} ${subject}_ses-followup"
	fi
    fi
done

(( ii=1 ))
info_message_ln "[SS]: Baseline Subjects"
for sfile in ${sets[baseline]} ; do
    msg="$( printf '[%02d] %s' ${ii} ${sfile} )"
    info_message_ln "${msg}"
    (( ii=ii+1 ))
done
(( ii=1 ))
info_message_ln "[SS]: Followup Subjects"
for sfile in ${sets[followup]} ; do
    msg="$( printf '[%02d] %s' ${ii} ${sfile} )"
    info_message_ln "${msg}"
    (( ii=ii+1 ))
done

taskFile=$CODE_DIR/run/${ttests_task}.taskfile
info_message_ln "List of tasks to be executed is stored in $taskFile"
cat /dev/null > $taskFile

cd ${PIPELINE_DIR}
ln -sf $tpath/$btemplate .
for seed in ${seeds} ; do
    seedName=${seed##*/}
    if echo ${seedName} | grep -q "nii" ; then 
        seedName=${seedName%%.nii*}
    else 
        seedName=${seedName%%+*}
    fi
    
    info_message_ln "Running t-tests for ${seedName}"
    echo "Subj session InputFile" > datatable.${seedName}.tsv
    
    # declare -A sets
    # files[baseline]=""
    # files[followup]=""
    for session in baseline followup ; do
	for ss in ${sets[${session}]} ; do
	    subject=$( echo ${ss} | sed -e "s#.*\(sub-[0-9][0-9][0-9]\)_.*#\1#" )
	    ## echo "$session: $ss : $subject"
	    
	    inputfile=${CORR_PIPELINE_DIR}/${subject}/ses-${session}/${seedName}/${seedName}.z-score+tlrc.HEAD
	    ## inputfile=${subject}/ses-${session}/${seedName}/${seedName}.z-score+tlrc.HEAD	    
	    echo "${subject} ${session} ${inputfile}" >> datatable.${seedName}.tsv
	    ## files["${session}"]+="${inputfile}"
	done
    done

    ## first_stats_file=$( echo ${files[baseline]} | awk '{print $1}' )
    first_stats_file=$( head -2 datatable.${seedName}.tsv | tail -1 | awk '{print $3}' )
    if [[ ! -f Bmask+tlrc.HEAD ]] ; then 
	3dbucket -prefix Bmask $tpath/$btemplate'[Bmask]' $tpath/$btemplate 
	3dresample -rmode NN -master ${first_stats_file} -input Bmask+tlrc -prefix Bmask_epi+tlrc 
    fi
#     cat <<EOF >> ${taskFile}
#     3dttest++ \
#     	  -setA $( grep baseline datatable.${seedName}.tsv | awk '{print $3}' | tr '\n' ' ' ) \
# 	   $( grep followup datatable.${seedName}.tsv | awk '{print $3}' | tr '\n' ' ' ) \
# 	  -labelA baseline \
# 	  -mask Bmask_epi+tlrc.HEAD \
# 	  -prefix ${ttests_task}.${seedName} \
# 	  -Clustsim ${threads}
# EOF

    cat <<EOF >> ${taskFile}
    3dttest++ \
    	  -setA $( grep baseline datatable.${seedName}.tsv | awk '{print $3}' | tr '\n' ' ' ) \
	  -setB $( grep followup datatable.${seedName}.tsv | awk '{print $3}' | tr '\n' ' ' ) \
	  -labelA baseline \
	  -labelB followup \
	  -paired \
	  -mask Bmask_epi+tlrc.HEAD \
	  -prefix ${ttests_task}.${seedName} \
	  -Clustsim ${threads}
EOF


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
sge_command="qsub -N ${ttests_task} -q ${queue} -j y -m n -V -wd ${PIPELINE_DIR} -o ${LOG_DIR} -t 1-${nTasks}" 
info_message_ln $sge_command
( exec $sge_command <<EOF
#!/bin/bash

#$ -S /bin/bash

command=\`sed -n -e "\${SGE_TASK_ID}p" $taskFile\`

exec /bin/sh -c "\$command"
EOF
)

info_message_ln "Running qstat"
qstat -q ${queue}


    # 3dttest++ -setA ${files[baseline]} \
    # 	  -setB ${files[followup]} \
    # 	  -labelA baseline \
    # 	  -labelB followup \
    # 	  -paired \
    # 	  -mask Bmask_epi+tlrc.HEAD \
    # 	  -prefix ${ttests_task}.${seedName} \
    # 	  -Clustsim ${threads}
