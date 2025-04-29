#!/bin/bash

# set -x

#exit immediatly on an error
set -e 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

studyName=pain_supplement
task="task-tapping"
lme_task="${task}-lmes-mt0.35-ex0.30"

PROGRAM_NAME=`basename $0`
FULL_COMMAND_LINE="$( readlink -f $( dirname $0 ) )/$PROGRAM_NAME ${*}"

GETOPT=$( which getopt )
ROOT=${MDD_ROOT:-/data/colmconn/${studyName}}

DDATA=$ROOT/data
SOURCE_DATA=$ROOT/sourcedata
DERIVATIVE_DATA=$ROOT/derivatives
TASK_PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${task}
PIPELINE_DIR=${DERIVATIVE_DATA}/afni-${lme_task}
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

function extractBrikLabels {
    local bucket="$1"

    a=$(3dAttribute BRICK_LABS $bucket )

    # echo "$( echo ${a} | sed 's/~$//' )" #| sed 's/ /_/g' )" # | sed 's/~/,/g' )"
    echo "${a}"
}

function extract_stat_pars {
    local bucket="$1"
    local subbrikId="$2"

    a=$(3dAttribute BRICK_STATSYM $bucket"[$subbrikId]" 2> /dev/null )
    b=${a##*(}
    c=${b%%)*}

    echo $( echo $c | tr "," " " )
}

function is_f_test {

    f_regex='F$'
    if [[ ${1} =~ ${f_regex} ]] ; then
	return 0
    else
	return 1
    fi
}

function is_chi_sq_test {

    f_regex='Chi-sq$'
    if [[ ${1} =~ ${f_regex} ]] ; then
	return 0
    else
	return 1
    fi
}

function is_intercept {

    intercept_regex='.*Intercept.*'
    if [[ ${1} =~ ${intercept_regex} ]] ; then
	return 0
    else
	return 1
    fi
}

if [ "x$NN" == "x" ] ; then 
    ## nearest neighbour 1=touching at faces, 2=faces and edges 3=faces,
    ## edges and corners, just like in the afni clusterize window
    NN=1
fi

if [[ "x$pValue" == "x" ]] ; then
    ## voxelwise pvalue
    pValue=0.05
    warn_message_ln "Set voxelwise pvalue to $pValue (default)"
else
    info_message_ln "Set voxelwise pvalue to $pValue"
fi

if [[ "x$alpha" == "x" ]] ; then
    # clusterwise pvalue
    alpha=0.050
    warn_message_ln "Set whole brain alpha to $alpha (default)"	    
else
    info_message_ln "Set whole brain alpha to $alpha"    
fi

if [[ "x$side" == "x" ]] ; then
    warn_message_ln "No value provided for side. Defaulting to bisided"
    side="bi"
else
    info_message_ln "Running a $side test"
fi

info_message_ln "Will use group results files in ${PIPELINE_DIR}"

cd ${PIPELINE_DIR}

dataTableFilename=${PIPELINE_DIR}/lme_data_table.tsv 
info_message_ln "Data table file is: ${dataTableFilename}"

acfDataTableFilename=${PIPELINE_DIR}/lme_acf_data_table.tsv
info_message_ln "ACF data table file is: ${acfDataTableFilename}"

statsFilename=session_by_intervention+tlrc.HEAD
info_message_ln "LME statistics file is: ${statsFilename}"

residualsFilename=session_by_intervention_residuals+tlrc.HEAD
info_message_ln "LME residuals file is: ${residualsFilename}"

csvFile=parameters.${statsFilename%%+*}.csv

if [[ $overwrite -eq 1 ]] || [[ ! -f $csvFile ]] ; then 
    echo "prefix,contrastBrikLabel,contrastBrikId,statsBrikLabel,statsBrikId,statsParameters,statThreshold,NN,nVoxels,pValue,alpha,nClusters,statsFilename" \
	  > $csvFile
fi

bls="$( extractBrikLabels ${statsFilename} )"
info_message_ln "Brik Labels: ${bls}"

declare -a brikLabels
old_ifs="$IFS"
IFS="~"
read -ra brikLabels <<< "${bls}" # Splits the string into an array
IFS="$old_ifs"
# brikLabels=( "intervention  F" )
nBrikLabels=${#brikLabels[@]}

count=0
while [ ${count} -lt ${nBrikLabels} ] ; do
    if is_f_test "${brikLabels[${count}]}" ; then
	info_message_ln "ME(F)      : $(( count + 1 )) of ${nBrikLabels}: ${brikLabels[${count}]}"
    elif is_chi_sq_test "${brikLabels[${count}]}" ; then
	info_message_ln "ME(Chi Sq) : $(( count + 1 )) of ${nBrikLabels}: ${brikLabels[${count}]}"	     
    else
	info_message_ln "GLT        : $(( count + 1 )) of ${nBrikLabels}: ${brikLabels[${count}]}"
    fi
    (( count = count + 1 ))
done

if [[ ! -f 3dClustSim.cmd ]] ; then
    if [[ -f ${residualsFilename} ]] ; then
	info_message_ln "Found a residuals data set. Estimating smoothing ACF from it"
	mkdir files_acf
	3dFWHMx \
	    -detrend \
	    -mask Bmask_epi+tlrc. \
	    -ACF files_acf/out.3dfwhmx.resid.1D \
	    ${residualsFilename} > blur.lme_residuals.1D
	acf="$( tail -1 blur.lme_residuals.1D | awk '{print $1,$2,$3}' )"
	info_message_ln "ACF parameters output by 3dFWHMx: ${acf}"
    else
	acf="$( cat lme_acf_data_table.tsv | awk '{print $1,$2,$3}' )"
	info_message_ln "ACF parameters from lme_acf_data_table.tsv: ${acf}"	
    fi

    info_message_ln "Running 3dClustSim to get probability table"
    3dClustSim -mask Bmask_epi+tlrc -acf ${acf} -niml -prefix CStemp
else
    info_message_ln "Found existing 3dClustSim.cmd. Skipping run of 3dClustSim"
fi

count=0
for (( count=0; count < ${nBrikLabels}; count++ )) ; do
    info_message_ln "################################################################################"
    info_message_ln  "Effect $(( count+1 )) of ${nBrikLabels}: ${brikLabels[${count}]}"
    if is_intercept ${brikLabels[${count}]} ; then
	info_message_ln "Skipping intercept effect"
	continue
    fi

    contrastBrikLabel="${brikLabels[${count}]}"
    contrastBrikId=$( 3dinfo -label2index "${brikLabels[${count}]}" ${statsFilename} 2> /dev/null )
    if is_f_test "${brikLabels[${count}]}" ; then
	info_message_ln "Got main effect F test"
	statsBrikLabel="${brikLabels[${count}]}"
	statsBrikId=$( 3dinfo -label2index "${brikLabels[${count}]}" ${statsFilename} 2> /dev/null )
	statsParameters=$( extract_stat_pars ${statsFilename} ${statsBrikId} )
	statThreshold=$( cdf -p2t fift $pValue ${statsParameters} | sed 's/t = //' )
    elif is_chi_sq_test "${brikLabels[${count}]}" ; then
	info_message_ln "Got main effect Chi Sq test"
	statsBrikLabel="${brikLabels[${count}]}"
	statsBrikId=$( 3dinfo -label2index "${brikLabels[${count}]}" ${statsFilename} 2> /dev/null )
	statsParameters=$( extract_stat_pars ${statsFilename} ${statsBrikId} )
	statThreshold=$( cdf -p2t fict $pValue ${statsParameters} | sed 's/t = //' )
    else
	info_message_ln "Got GLT test"
	statsBrikLabel="${brikLabels[$(( count + 1 ))]}"	
	statsBrikId=$( 3dinfo -label2index "${brikLabels[$(( count + 1 ))]}" ${statsFilename} 2> /dev/null )
	statsParameters="NA"
	statThreshold=$( cdf -p2t fizt $pValue | sed 's/t = //' )
	# skip the next subbrik label because it will be the z score
	# associated with the current GLT contrast subbrik
	(( count=count+1 ))
    fi

    prefix=$( echo ${contrastBrikLabel} | tr -s ' ' '_' )
    csimFile=CStemp.NN${NN}_${side}sided.niml
    nVoxels=$( ${CODE_DIR}/get.minimum.voxel.count.r --nn $NN --alpha=$alpha --pthr=$pValue --side=$side -c ${csimFile} )
    nVoxels="$( echo $nVoxels | tr -d '[:space:]')"
    if [[ "x$nVoxels" == "x" ]] || [[ "$nVoxels" == "NA" ]] ; then                                                                                                                                
	error_message_ln "Couldn't get the correct number of voxels to go with pvalue=${pValue} and corrected alpha=${alpha}"
	error_message_ln "You may need to pad these values with zeros to ensure you match the correct row and column in ${csimFile}"
	error_message_ln "This may also indicate that the 3dClustSim table does not contain the p values specified"
	exit
    fi   

    info_message_ln "prefix            = ${prefix}"
    info_message_ln "contrastBrikLabel = ${contrastBrikLabel}"
    info_message_ln "contrastBrikId    = ${contrastBrikId}"    
    info_message_ln "statsBrikLabel    = ${statsBrikLabel}"
    info_message_ln "statsBrikId       = ${statsBrikId}"
    info_message_ln "statsParameters   = ${statsParameters}"    
    info_message_ln "statThreshold     = ${statThreshold}"
    info_message_ln "NN                = ${NN}"
    info_message_ln "nVoxels           = ${nVoxels}"
    info_message_ln "voxelwise pValue  = ${pValue}"
    info_message_ln "corrected pValue  = ${alpha}"
    info_message_ln "Clustersim file   = ${csimFile}"

    infix=${prefix}
    info_message_ln "Running 3dClusterize"
    3dClusterize  \
	-inset ${statsFilename} \
	-ithr ${statsBrikId} \
	-idat ${contrastBrikId} \
	-mask Bmask_epi+tlrc.HEAD \
	-NN $NN \
	-${side}sided p=${pValue} \
	-clust_nvox ${nVoxels} \
	-pref_map clusters.order.${infix} > clusters.table.${infix}.txt
    cp clusters.table.${infix}.txt clusters.table.${infix}.txt.bak

    if grep -q "NO CLUSTERS FOUND" clusters.table.${infix}.txt > /dev/null 2>&1 ; then 
	warn_message_ln "clusters.table.${infix}.txt contains no clusters. Deleting it."
	rm -f clusters.table.${infix}.txt
    fi

    if [[ -f clusters.order.${infix}+tlrc.HEAD ]] ; then 

	3dcalc -datum float -a clusters.order.${infix}+tlrc.HEAD -b ${statsFilename}\[${statsBrikId}\] -expr "step(a)*b" -prefix clusters.stats.${infix}
	
	nClusters=$( 3dBrickStat -max clusters.order.${infix}+tlrc.HEAD 2> /dev/null | tr -d ' ' )
	
	columnNumber=$( head -1 ${dataTableFilename} | tr '[[:space:]]' '\n' | grep -n InputFile | cut -f1 -d':' )
	if [[ -z ${columnNumber} ]] ; then
            error_message_ln "Couldn't find a column named InputFile in ${dataTableFilename}"
            error_message_ln "Cannot continue"
            exit 1
	fi
	
	3dROIstats -mask clusters.order.${infix}+tlrc.HEAD \
		   $( tail -n +2 ${dataTableFilename} |  \
			  awk -v cn=${columnNumber} '{ print $cn }' )\
		   > roi.stats.${infix}.txt
	3dROIstats \
	    -nobriklab \
	    -mask clusters.order.${infix}+tlrc.HEAD \
	    ${statsFilename}\[${contrastBrikId}\] > roi.stats.${infix}.averageContrastValue.txt
	3dROIstats \
	    -nobriklab \
	    -mask clusters.order.${infix}+tlrc.HEAD \
	    ${statsFilename}\[${statsBrikId}\]    > roi.stats.${infix}.averageStatValue.txt
	
	3drefit -cmap INT_CMAP clusters.order.${infix}+tlrc.HEAD
    else
	nClusters=0
	warn_message_ln "WARNING No clusters found!"
    fi

    echo "${prefix},${contrastBrikLabel},${contrastBrikId},${statsBrikLabel},${statsBrikId},${statsParameters},${statThreshold},${NN},${nVoxels},${pValue},${alpha},${nClusters},${statsFilename}" \
	 >> ${csvFile}
done

info_message_ln "Making cluster location tables using Center of Mass"
${CODE_DIR}/cluster2Table.pl --space=mni --force ${PIPELINE_DIR}
