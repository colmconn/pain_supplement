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
ROOT=${MDD_ROOT:-/data/colmconn/${studyName}}

DDATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivative
TASK_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${ttests_task}
LOG_DIR=$ROOT/log
CODE_DIR=${ROOT}/code

##GROUP_DATA=$DATA/Group.data
##GROUP_RESULTS=$DATA/Group.results
MDD_STANDARD=$ROOT/standard
MDD_TISSUEPRIORS=$ROOT/tissuepriors
scriptsDir=${ROOT}/scripts

LOG_DIR=${DATA}/log
GETOPT_OPTIONS=$( $GETOPT  -o "a:l:n:op:s:" \
			   --longoptions "alpha:,seedlist:,nn:,overwrite,pvalue:,sided:" \
			   -n ${PROGRAM_NAME} -- "$@" )
exitStatus=$?
if [[ $exitStatus != 0 ]] ; then 
    error_message_ln "Error with getopt. Terminating..." >&2 
    exit $exitStatus
fi

overwrite=0
# Note the quotes around `$GETOPT_OPTIONS': they are essential!
eval set -- "$GETOPT_OPTIONS"
while true ; do 
    case "$1" in
	-a|--alpha)
	    alpha=$2;
	    shift 2 ;;
	-l|seedlist)
	    seedList="$2"; shift 2 ;;
	-n|--nn)
	    NN=$2; 
	    shift 2 ;;
	-o|--overwrite ) 
	    overwrite=1; 
	    shift ;;
	-p|--pvalue)
	    pValue=$2;
	    shift 2 ;;
	-s|--sided )
	    ss=$2
	    if [[ $ss == "1" ]] ; then 
		side="1"
	    elif [[ $ss == "2" ]] ; then 
		side="2"
	    elif [[ $ss == "bi" ]] ; then 
		side="bi"
	    else
		warn_message_ln "Unknown argument provided to -s or --sided. Valid values are 1, 2, bi. Defaulting to bisided"
		side="bisided"		
	    fi
	    shift 2 ;;	
	--) 
	    shift ; break ;;

	*) 
	    echo "${programName}: ${1}: invalid option" >&2
	    exit 2 ;;
    esac
done

function extractStatisticsParameters {
    local bucket="$1"
    local subbrikId="$2"

    a=$( 3dAttribute BRICK_STATSYM $bucket"[$subbrikId]" )
    b=${a##*(}
    c=${b%%)*}

    echo $( echo $c | tr "," " " )
}

if [ "x$NN" == "x" ] ; then 
    ## nearest neighbour 1=touching at faces, 2=faces and edges 3=faces,
    ## edges and corners, just like in the afni clusterize window
    NN=1
fi

if [[ "x$pValue" == "x" ]] ; then
    ## voxelwise pvalue
    pValue=0.05
    info_message_ln "Set voxelwise pvalue to $pValue (default)"
else
    info_message_ln "Set voxelwise pvalue to $pValue"
fi

if [[ "x$alpha" == "x" ]] ; then
    # clusterwise pvalue
    alpha=0.050
    info_message_ln "Set whole brain alpha to $alpha (default)"	    
else
    info_message_ln "Set whole brain alpha to $alpha"    
fi

if [[ "x$side" == "x" ]] ; then
    info_message_ln "No value provided for side. Defaulting to bisided"
    side="bi"
else
    info_message_ln "Running a ${side}-sided test"
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

info_message_ln "Data for the following seeds' t-tests will be clustered:"
nn=1
for seed in ${seeds}; do
    msg=$( printf "[%03d]: %s" ${nn} ${seed} )
    info_message_ln "${msg}"
    (( nn=nn+1 ))
done

info_message_ln "Will use group results files in ${PIPELINE_DIR}"

tLabelPrefix="baseline-followup"

cd ${PIPELINE_DIR}
csvFile=parameters.${tLabelPrefix}.csv
#if [[ $overwrite -eq 1 ]] || [[ ! -f $csvFile ]] ; then 
echo "seed,tLabelPrefix,contrastBrikId,statsBrikId,tThreshold,NN,nVoxels,pValue,alpha,nClusters,tTestFile" > $csvFile
#fi

for seed in ${seeds} ; do
    seedName=${seed##*/}
    if echo ${seedName} | grep -q "nii" ; then 
        seedName=${seedName%%.nii*}
    else 
        seedName=${seedName%%+*}
    fi
    
    info_message_ln "Clustering t-tests for the ${seedName} seed"

    dataTableFilename=${PIPELINE_DIR}/datatable.${seedName}.tsv 
    info_message_ln "Data table file is: $dataTableFilename"
    
    tTestFile=${ttests_task}.${seedName}+tlrc
    info_message_ln "T-test file is: ${tTestFile}"
    
    contrastBrikId=$( 3dinfo -label2index "${tLabelPrefix}_mean" $tTestFile 2> /dev/null )    
    statsBrikId=$( 3dinfo -label2index "${tLabelPrefix}_Zscr" $tTestFile 2> /dev/null )
    csimfile=${ttests_task}.${seedName}.CSimA.NN${NN}_${side}sided.1D
    nVoxels=$( ${CODE_DIR}/get.minimum.voxel.count.r --nn $NN --alpha=$alpha --pthr=$pValue --side=$side -c ${csimfile} )
    nVoxels="$( echo $nVoxels | tr -d '[:space:]')"
    if [[ "x$nVoxels" == "x" ]] || [[ "$nVoxels" == "NA" ]] ; then                                                                                                                                
	info_message_ln "Couldn't get the correct number of voxels to go with pvalue=${pValue} and corrected alpha=${alpha}"
	info_message_ln "You may need to pad these values with zeros to ensure you match the correct row and column in ${csimfile}"
	info_message_ln "This may also indicate that the 3dClustSim table does not contain the p values specified"
	exit
    fi
    
    ## tThreshold=$( cdf -p2t fitt $pValue $df | sed 's/t = //' )
    tThreshold=$( cdf -p2t fizt $pValue | sed 's/t = //' )
    ## info_message_ln "fwhm = ${usedFwhm}"
    info_message_ln "tLabelPrefix = $tLabelPrefix"
    info_message_ln "contrastBrikId = $contrastBrikId"
    info_message_ln "statsBrikId = $statsBrikId"
    info_message_ln "tThreshold = $tThreshold"
    info_message_ln "NN  = $NN"
    info_message_ln "nVoxels = $nVoxels"
    ## info_message_ln "df = $df"
    info_message_ln "voxelwise pValue = $pValue"
    info_message_ln "corrected pValue = $alpha"
    info_message_ln "Clustersim file = ${csimfile}"
    
    suffix=${tLabelPrefix}.${seedName}
    
    3dClusterize  \
	-inset ${tTestFile} \
	-ithr ${statsBrikId} \
	-idat ${contrastBrikId} \
	-mask Bmask_epi+tlrc.HEAD \
	-NN $NN \
	-bisided p=${pValue} \
	-clust_nvox ${nVoxels} \
	-pref_map clusters.order.${suffix} > clusters.table.${suffix}.txt
    
    if grep -q "NO CLUSTERS FOUND" clusters.table.${suffix}.txt > /dev/null 2>&1 ; then 
	warn_message_ln "clusters.table.${suffix}.txt contains no clusters. Deleting it."
	rm -f clusters.table.${suffix}.txt
    fi
    
    if [[ -f clusters.order.${suffix}+tlrc.HEAD ]] ; then 
	
	3dcalc -datum float -a clusters.order.${suffix}+tlrc.HEAD -b ${tTestFile}\[${statsBrikId}\] -expr "step(a)*b" -prefix clusters.stats.${suffix}
	
	nClusters=$( 3dBrickStat -max clusters.order.${suffix}+tlrc.HEAD 2> /dev/null | tr -d ' ' )
	
	columnNumber=$( head -1 ${dataTableFilename} | tr '[[:space:]]' '\n' | grep -n InputFile | cut -f1 -d':' )
	if [[ -z ${columnNumber} ]] ; then
            error_message_ln "Couldn't find a column named InputFile in $dataTableFilename"
            error_message_ln "Cannot continue"
            exit 1
	fi
	
	3dROIstats -mask clusters.order.${suffix}+tlrc.HEAD $( tail -n +2 ${dataTableFilename} |  awk -v cn=${columnNumber} '{ print $cn }' ) > roi.stats.${suffix}.txt
	
	3dROIstats -nobriklab -mask clusters.order.${suffix}+tlrc.HEAD ${tTestFile}\[${contrastBrikId}\] > roi.stats.${suffix}.averageContrastValue.txt
	3dROIstats -nobriklab -mask clusters.order.${suffix}+tlrc.HEAD ${tTestFile}\[${statsBrikId}\]    > roi.stats.${suffix}.averageZscore.txt
	
	3drefit -cmap INT_CMAP clusters.order.${suffix}+tlrc.HEAD
    else
	nClusters=0
	warn_message_ln "WARNING No clusters found!"
    fi
    echo "${seedName},${tLabelPrefix},${contrastBrikId},${statsBrikId},${tThreshold},${NN},${nVoxels},${pValue},${alpha},${nClusters},${tTestFile}" >> ${csvFile}
done

cd ${CODE_DIR}
##info_message_ln "Making cluster location tables using Maximum intensity"
##./cluster2Table.pl --space=mni --force -mi $GROUP_RESULTS

info_message_ln "Making cluster location tables using Center of Mass"
./cluster2Table.pl --space=mni --force ${PIPELINE_DIR}

