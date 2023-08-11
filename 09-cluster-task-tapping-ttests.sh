#!/bin/bash

set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"
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
GETOPT_OPTIONS=$( $GETOPT  -o "a:n:op:s:" \
			   --longoptions "alpha:,nn:,overwrite,pvalue:,sided:" \
			   -n ${PROGRAM_NAME} -- "$@" )
exitStatus=$?
if [[ $exitStatus != 0 ]] ; then 
    echo "Error with getopt. Terminating..." >&2 
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
		echo "Unknown argument provided to -s or --sided. Valid values are 1, 2, bi. Defaulting to bisided"
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

function extractTStatpars {
    local bucket="$1"
    local subbrikId="$2"

    a=$(3dAttribute BRICK_STATSYM $bucket"[$subbrikId]" )
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
    echo "*** Set voxelwise pvalue to $pValue (default)"
else
    echo "*** Set voxelwise pvalue to $pValue"
fi

if [[ "x$alpha" == "x" ]] ; then
    # clusterwise pvalue
    alpha=0.050
    echo "*** Set whole brain alpha to $alpha (default)"	    
else
    echo "*** Set whole brain alpha to $alpha"    
fi

if [[ "x$side" == "x" ]] ; then
    echo "*** No value provided for side. Defaulting to bisided"
    side="bi"
else
    echo "*** Running a $side test"
fi

echo "*** Will use group results files in ${PIPELINE_DIR}"

tLabelPrefix="baseline-followup"
csvFile=parameters.${tLabelPrefix}.csv

cd ${PIPELINE_DIR}

if [[ $overwrite -eq 1 ]] || [[ ! -f $csvFile ]] ; then 
    echo "tLabelPrefix,contrastBrikId,statsBrikId,tThreshold,NN,nVoxels,pValue,alpha,nClusters,tTestFile" > $csvFile
fi

dataTableFilename=${PIPELINE_DIR}/datatable.tsv 
echo "### Data table file is: $dataTableFilename"

tTestFile=${ttests_task}+tlrc
echo "### T-test file is: $tTestFile"

tContrastBrikId=$( 3dinfo -label2index "${tLabelPrefix}_mean" $tTestFile 2> /dev/null )    
statsBrikId=$( 3dinfo -label2index "${tLabelPrefix}_Zscr" $tTestFile 2> /dev/null )
csimfile=${ttests_task}.CSimA.NN${NN}_${side}sided.1D
nVoxels=$( ${CODE_DIR}/get.minimum.voxel.count.r --nn $NN --alpha=$alpha --pthr=$pValue --side=$side -c ${csimfile} )
nVoxels="$( echo $nVoxels | tr -d '[:space:]')"
if [[ "x$nVoxels" == "x" ]] || [[ "$nVoxels" == "NA" ]] ; then                                                                                                                                
    echo "*** Couldn't get the correct number of voxels to go with pvalue=${pValue} and corrected alpha=${alpha}"
    echo "*** You may need to pad these values with zeros to ensure you match the correct row and column in ${csimfile}"
    echo "*** This may also indicate that the 3dClustSim table does not contain the p values specified"
    exit
fi

## tThreshold=$( cdf -p2t fitt $pValue $df | sed 's/t = //' )
tThreshold=$( cdf -p2t fizt $pValue | sed 's/t = //' )
## echo "### fwhm = ${usedFwhm}"
echo "### tLabelPrefix = $tLabelPrefix"
echo "### tContrastBrikId = $tContrastBrikId"
echo "### statsBrikId = $statsBrikId"
echo "### tThreshold = $tThreshold"
## echo "### rmm = $rmm"
echo "### NN  = $NN"
echo "### nVoxels = $nVoxels"
## echo "### df = $df"
echo "### voxelwise pValue = $pValue"
echo "### corrected pValue = $alpha"
echo "### Clustersim file = ${csimfile}"

suffix=${tLabelPrefix}

3dClusterize  \
    -inset ${tTestFile} \
    -ithr ${statsBrikId} \
    -idat ${tContrastBrikId} \
    -mask Bmask_epi+tlrc.HEAD \
    -NN $NN \
    -bisided p=${pValue} \
    -clust_nvox ${nVoxels} \
    -pref_map clusters.order.${suffix} > clusters.table.${suffix}.txt
cp clusters.table.${suffix}.txt clusters.table.${suffix}.txt.bak

if grep -q "NO CLUSTERS FOUND" clusters.table.${suffix}.txt > /dev/null 2>&1 ; then 
    echo "*** clusters.table.${suffix}.txt contains no clusters. Deleting it."
    rm -f clusters.table.${suffix}.txt
fi

if [[ -f clusters.order.${suffix}+tlrc.HEAD ]] ; then 

    3dcalc -datum float -a clusters.order.${suffix}+tlrc.HEAD -b ${tTestFile}\[${statsBrikId}\] -expr "step(a)*b" -prefix clusters.stats.${suffix}
	
    nClusters=$( 3dBrickStat -max clusters.order.${suffix}+tlrc.HEAD 2> /dev/null | tr -d ' ' )
    
    columnNumber=$( head -1 ${dataTableFilename} | tr '[[:space:]]' '\n' | grep -n InputFile | cut -f1 -d':' )
    if [[ -z ${columnNumber} ]] ; then
        echo "Couldn't find a column named InputFile in $dataTableFilename"
        echo "*** Cannot continue"
        exit 1
    fi
    
    3dROIstats -mask clusters.order.${suffix}+tlrc.HEAD $( tail -n +2 ${dataTableFilename} |  awk -v cn=${columnNumber} '{ print $cn }' ) > roi.stats.${suffix}.txt

    3dROIstats -nobriklab -mask clusters.order.${suffix}+tlrc.HEAD ${tTestFile}\[${tContrastBrikId}\] > roi.stats.${suffix}.averageContrastValue.txt
    3dROIstats -nobriklab -mask clusters.order.${suffix}+tlrc.HEAD ${tTestFile}\[${statsBrikId}\]     > roi.stats.${suffix}.averageZscore.txt
    
    3drefit -cmap INT_CMAP clusters.order.${suffix}+tlrc.HEAD
else
    nClusters=0
    echo "*** WARNING No clusters found!"
fi
echo "${tLabelPrefix},${contrastBrikId},${statsBrikId},${tThreshold},${NN},${nVoxels},${pValue},${alpha},${nClusters},${tTestFile}" >> ${csvFile}

cd ${CODE_DIR}
##echo "*** Making cluster location tables using Maximum intensity"
##./cluster2Table.pl --space=mni --force -mi $GROUP_RESULTS

echo "*** Making cluster location tables using Center of Mass"
./cluster2Table.pl --space=mni --force ${PIPELINE_DIR}

