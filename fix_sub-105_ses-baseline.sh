#!/bin/bash

cd /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/func

rm -f *.{nii.gz,json}

ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_rest_2.5_iso_cmmr_20221103100656_7_e1.json    sub-105_ses-baseline_task-tapping_run-1_echo-1_bold.json 
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_rest_2.5_iso_cmmr_20221103100656_7_e1.nii.gz  sub-105_ses-baseline_task-tapping_run-1_echo-1_bold.nii.gz 
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_rest_2.5_iso_cmmr_20221103100656_7_e2.json    sub-105_ses-baseline_task-tapping_run-1_echo-2_bold.json  
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_rest_2.5_iso_cmmr_20221103100656_7_e2.nii.gz  sub-105_ses-baseline_task-tapping_run-1_echo-2_bold.nii.gz 
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_rest_2.5_iso_cmmr_20221103100656_7_e3.json    sub-105_ses-baseline_task-tapping_run-1_echo-3_bold.json 
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_rest_2.5_iso_cmmr_20221103100656_7_e3.nii.gz  sub-105_ses-baseline_task-tapping_run-1_echo-3_bold.nii.gz
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_run1_2.5_iso_cmmr_20221103100656_8_e1.json    sub-105_ses-baseline_task-tapping_run-2_echo-1_bold.json  
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_run1_2.5_iso_cmmr_20221103100656_8_e1.nii.gz  sub-105_ses-baseline_task-tapping_run-2_echo-1_bold.nii.gz  
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_run1_2.5_iso_cmmr_20221103100656_8_e2.json    sub-105_ses-baseline_task-tapping_run-2_echo-2_bold.json  
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_run1_2.5_iso_cmmr_20221103100656_8_e2.nii.gz  sub-105_ses-baseline_task-tapping_run-2_echo-2_bold.nii.gz  
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_run1_2.5_iso_cmmr_20221103100656_8_e3.json    sub-105_ses-baseline_task-tapping_run-2_echo-3_bold.json  
ln -sf /data/colmconn/pain_supplement/sourcedata/sub-105/ses-baseline/dcm2nii/heads_ahn_lab_20221103_100656_390000_mb_3echo_run1_2.5_iso_cmmr_20221103100656_8_e3.nii.gz  sub-105_ses-baseline_task-tapping_run-2_echo-3_bold.nii.gz  
