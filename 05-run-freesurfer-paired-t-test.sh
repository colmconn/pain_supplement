#!/bin/bash

if [[ ! -d ../derivative/freesurfer-7.0/paired.t.test ]] ; then
    mkdir ../derivative/freesurfer-7.0/paired.t.test
fi

Rscript ./make-freesurfer-longitudinal-datatable.r

# based on https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/GroupAnalysis adn
# https://surfer.nmr.mgh.harvard.edu/fswiki/PairedAnalysis
export SUBJECTS_DIR=$( readlink -f ../derivative/freesurfer-7.0 )
cd  ../derivative/freesurfer-7.0/paired.t.test
for hemi in lh rh ; do
    for measure in thickness ; do
	mris_preproc --target fsaverage \
		     --hemi ${hemi}    \
		     --meas ${measure} \
		     --out ${hemi}.paired-diff.${measure}.mgh    \
		     --fsgd paired.fsgd \
		     --paired-diff
	
	mri_surf2surf --s fsaverage \
		      --hemi ${hemi} \
		      --fwhm 5 \
		      --sval ${hemi}.paired-diff.${measure}.mgh  \
		      --tval ${hemi}.paired-diff.${measure}.sm05.mgh

	echo "1" > mean.mtx

	mri_glmfit \
	    --glmdir ${hemi}.paired-diff \
	    --surf fsaverage ${hemi} \
	    --y ${hemi}.paired-diff.${measure}.sm05.mgh \
	    --fsgd paired.diff.fsgd \
	    --C mean.mtx
	
	mri_glmfit-sim \
	    --glmdir ${hemi}.paired-diff \
	    --cache 4 neg \
	    --cwp  0.05\
	    --2spaces
    done
done

