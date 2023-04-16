# DKA-GWAS
SQL, shell, and R scripts for GWAS of DKA in UKB


*Discovery cohort was UKB. Will be repeated in MVP.* 

## Overall pipeline steps:

1. Query for patients with DKA (type 1, type 2, unspecified)
2. Query for patients with diabetes
3. Pheno files (by ethnicity): Age, sex, DKA case (yes/no), diabetes case (yes/no), principal components 1-10 (by ethnicity where appropriate)
4. Summary stats for study cohort
5. Plink/ REGENIE GWAS
6. Combine chromosome data
7. Manhattan/ QQ plots
8. Identify variants of interest

**GWAS conducted to find variants associated with type 1, type 2, or any DKA in the general population, as well as in only diabetics. By ethnicity.**  
