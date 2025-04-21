#!/bin/bash

## set -x 

trap exit SIGHUP SIGINT SIGTERM

. logger_functions.sh

cat /dev/null > ../derivatives/r-pain-ratings/pain_ratings.csv
echo "subject,session,task,run,rating,file.size,file" > ../derivatives/r-pain-ratings/pain_ratings.csv
for ss in ../rawdata/sub-[0-9][0-9][0-9]/ses-{baseline,followup} ../rawdata/sub-[0-9][0-9][0-9][0-9][0-9]/ses-{baseline,followup} ; do
    subject=${ss##../rawdata/}
    subject=${subject%%/*}
    subject=${subject##sub-}
    ## echo ${subject}
    
    session=${ss##*/}
    session=${session##ses-}
    ## echo ${session}

    info_message_ln "Extracting ratings for sub-${subject}/ses-${session}"
    ./extract-pain-ratings.r -l "../rawdata/sub-${subject}/ses-${session}"'/beh/*/*.log' \
    			     -o ../derivatives/r-pain-ratings \
    			     -s ${subject} \
     			     -e ${session}
done
