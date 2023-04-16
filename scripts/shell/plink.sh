#!/bin/bash
#SBATCH --job-name=DKA_chr1_22
#SBATCH --partition=sunlab
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32

for ((i=1; i<=22; i++)); do

imputedir="UKB/Data_raw"
outdir="DKA_UKB/T2DKA_all"

input=$imputedir"/ukb_imp_chr"$i"_v3"

/projects/sunlab/bin/plink2 \
--memory 30000 \
--threads 32 \
--pfile $input \
--maf 0.01 \
--no-psam-pheno \
--pheno /projects/DKA_UKB/covar_pheno.txt \
--pheno-name T2DKA \
--1 \
--covar /projects/DKA_UKB/covar_pheno.txt \
--covar-name Sex Age_enrolled PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 \
--logistic cols=+a1freq,+machr2,+beta hide-covar \
--covar-variance-standardize \
--out $outdir"/T2DKA_C"$i""

done
