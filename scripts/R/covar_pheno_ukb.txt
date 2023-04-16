#### Script to generate covariate/ pheno file using only white Europeans
#### Age, Sex, and PC1-10 are default covariates. Add more if needed in the appropriate section 
# Note that from UK Biobank, 1 = Male and 0 = Female
#### Phenotype = DKA/ diabetes

rm(list=ls())
suppressPackageStartupMessages(library(data.table,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(crayon,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(dplyr,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(cli,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(withr,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(labeling,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(farver,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(digest,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(ggplot2,lib.loc="/projects/sunlab/R.lib"))
suppressPackageStartupMessages(library(viridisLite,lib.loc="/projects/sunlab/R.lib"))



#### Labeling locations. Edit as needed

cov_loc = "UKB/Data_clean/ukb_core_data_669473.rda" # This file has Age, ethnicity, sex, and PC1-10 data. It will be loaded as 'bd'
pheno_loc = "DKA_UKB/DKA_diabetes.txt"  # Creation outlined in DKA_diabetes.R file

#### Setting covariates and phenotype
covars = c("IID","Age_enrolled","Gen_Ethnic","Ethnic","Sex","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
pheno_names = c("Diabetes", "DKA", "T1DKA", "T2DKA")
#field_ids = c()
#instances = c(0)
#arrays =  c(0)
#pheno_ids = paste(paste(field_ids, instances, sep = "-"), arrays, sep = ".")
#names(pheno_ids) = pheno_names
cat( "Covariates:\n", covars, "\n")
cat( "Phenotypes:\n", pheno_names, "\n")
#cat( "Phenotype Field IDs:\n", pheno_ids, "\n")


#### ********* Do you want the final file to only include complete cases? 
#############  Yes means all covariates and phenotypes must be present for all patients
complete_cases = "no"  


#### ********* Do you want to create a bar plot that shows phenotype by sex?
####             (note this has only been tested with continuous phenotypes) 
make_plot = "no"

# Loading covariate file

cat("Loading covariate file\n This takes a while\n")
load(file = cov_loc) # Takes forever to load
cat("File loaded\n")

### Renaming covariates. ADD TO THIS SECTION FOR MORE COVARIATES***************


names(bd)[grepl("eid", names(bd))] = "IID"
names(bd)[names(bd)=="f.50.0.0"]<-"Height"
names(bd)[names(bd)=="f.21002.0.0"]<-"Weight"
names(bd)[names(bd)=="f.21001.0.0"]<-"BMI"
names(bd)[names(bd)=="f.21022.0.0"]<-"Age_enrolled"
names(bd)[names(bd)=="f.22006.0.0"]<-"Gen_Ethnic"
names(bd)[names(bd)=="f.21000.0.0"]<-"Ethnic"
names(bd)[names(bd)=="f.31.0.0"]<-"Sex"
names(bd)[names(bd)=="f.22009.0.1"]<-"PC1"
names(bd)[names(bd)=="f.22009.0.2"]<-"PC2"
names(bd)[names(bd)=="f.22009.0.3"]<-"PC3"
names(bd)[names(bd)=="f.22009.0.4"]<-"PC4"
names(bd)[names(bd)=="f.22009.0.5"]<-"PC5"
names(bd)[names(bd)=="f.22009.0.6"]<-"PC6"
names(bd)[names(bd)=="f.22009.0.7"]<-"PC7"
names(bd)[names(bd)=="f.22009.0.8"]<-"PC8"
names(bd)[names(bd)=="f.22009.0.9"]<-"PC9"
names(bd)[names(bd)=="f.22009.0.10"]<-"PC10"


# make df with only covariate columns

covars = c("IID","Height","Weight","BMI","Age_enrolled","Gen_Ethnic","Ethnic","Sex","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
cat( "Covariates:\n", covars, "\n")
covar_df = bd[,covars]
rm(bd)



### Summary of covariates

cat("\nSummary Statistics for all patients \n")

# Function to calculate sum stats
calc <- function(a){
  m <- mean(na.omit(a))
  sd <- sd(na.omit(a))
  len <- length(na.omit(a))
  return(c(m,sd,len))
}

#Height
Height = calc(covar_df$Height)
Height =  paste0(round(Height[1],2),"(",round(Height[2],2),")")

#Weight
Weight = calc(covar_df$Weight)
Weight =  paste0(round(Weight[1],2),"(",round(Weight[2],2),")")

#BMI
BMI = calc(covar_df$BMI)
BMI =  paste0(round(BMI[1],2),"(",round(BMI[2],2),")")

#Age
Age = calc(covar_df$Age_enrolled)
Age =  paste0(round(Age[1],2),"(",round(Age[2],2),")")

#Sex 1=Male, 0=Female
Sex_count = table(covar_df$Sex)
female_prop = prop.table(Sex_count)
Sex = paste0(Sex_count[1],"(",round(female_prop[1],2),")")

Summary_all = as.data.frame(t(data.frame(Sex,Height,Weight,BMI,Age)))
rownames(Summary_all) = c("Female;%","Height","Weight","BMI","Age")
colnames(Summary_all) = c("Count(%) / Mean(SD)")

print(Summary_all)
cat("\n\n")

###### ******Loading phenotype file

cat("\nLoading phenotype file\n")
pheno_file = fread(pheno_loc, stringsAsFactors=F)
cat("\nPhenotype file loaded\n Dimensions\n", dim(pheno_file), "\n\n")

## Renaming columns 
#names(pheno_file)[grepl("eid", names(pheno_file))] = "IID"  

#for(i in 1:length(field_ids)){
#  names(pheno_file)[names(pheno_file)==pheno_ids[i]] = names(pheno_ids)[i]
#}

# Making a df with phenotype info
#pheno_df = pheno_file[,c("IID", ..pheno_names)]

### Viewing summary stats

cat("\n Summary Stats for DKA in Diabetes patients:\n")
 
calc2 = function(x){
 count = table(x)
 prop = prop.table(count)
 final = paste0(count[2],"(",round(prop[2],4),")")
}

dka = calc2(pheno_file$DKA)
t1dka = calc2(pheno_file$T1DKA)
t2dka = calc2(pheno_file$T2DKA)

Summary_all = as.data.frame(t(data.frame(dka, t1dka, t2dka)))
rownames(Summary_all) = c("DKA","T1DKA","T2DKA")
colnames(Summary_all) = c("Count(%)")

print(Summary_all)
cat("\n\n")


# merging pheno and covars dfs by "IID". IDs that are not in both will be dropped

cat("Merging covariate and phenotype dataframes by IID \n Starting dimensions of covariate file: \n",
	dim(covar_df), "\nStarting dimensions of phenotype file: \n", dim(pheno_file), "\n")

covar_pheno = merge(covar_df, pheno_file, by = "IID", all.x = TRUE)

# Replacing NA phenotype entries with 0
covar_pheno$Diabetes = ifelse(is.na(covar_pheno$Diabetes),0,1)
covar_pheno$DKA = ifelse(is.na(covar_pheno$DKA),0,covar_pheno$DKA)
covar_pheno$T1DKA = ifelse(is.na(covar_pheno$T1DKA),0,covar_pheno$T1DKA)
covar_pheno$T2DKA = ifelse(is.na(covar_pheno$T2DKA),0,covar_pheno$T2DKA)

cat("Dimensions of combined dataframe: \n", dim(covar_pheno), "\n")


cat("\n Summary Stats for phenotypes in all patients:\n")

diabetes = calc2(covar_pheno$Diabetes)
dka = calc2(covar_pheno$DKA)
t1dka = calc2(covar_pheno$T1DKA)
t2dka = calc2(covar_pheno$T2DKA)

Summary_all = as.data.frame(t(data.frame(diabetes, dka, t1dka, t2dka)))
rownames(Summary_all) = c("Diabetes","DKA","T1DKA","T2DKA")
colnames(Summary_all) = c("Count(%)")

print(Summary_all)
cat("\n\n")

#Only interested in white British patients (ethnicity 1)

cat("Removing patients not of white British ancestry \n")
before = nrow(covar_pheno)

covar_pheno = subset(covar_pheno, Ethnic %in% c("1","1001","1002","1003"))
after = nrow(covar_pheno)

cat("Removed", before-after, "patients\n")


# Making sure all covariates and phenotypes are present

if(tolower(complete_cases) == "yes"){
	cat("Complete cases set to 'yes' \nRemoving rows with missing values")
	before = nrow(covar_pheno)
	covar_pheno = covar_pheno[complete.cases(covar_pheno),]
	after = nrow(covar_pheno)
	cat("\nRemoved", before-after, "patients\n")}


## Remove Relatedness (entire section from Ellen)

cat("Removing patients with Kinship >=0.0884\n")

kin<-read.table("/projects/sunlab/UKB/Data_raw/ukb34031_rel_s488363.dat",header=T)
# dim(kin) #107156      5
kin<-kin[kin$Kinship>=0.0884,] #2nd degree
# dim(kin) #40230     5
# length(unique(kin$ID1)) #36179
# length(unique(kin$ID2)) #36159
ID<-c(kin$ID1,kin$ID2) #80460
dup<-names(table(ID)[table(ID)>1]) #9776
kin<-kin[(!(kin$ID1%in%dup))&(!(kin$ID2%in%dup)),] #27653

## Coding for diabetes. 1 = only ID1 has diabetes, 2 = only ID2 has diabetes, 3 = both have diabetes, 0 = neither has diabetes

kin$diabetes = ifelse(kin$ID1 %in% pheno_file$IID & !(kin$ID2 %in% pheno_file$IID), 1,
			ifelse(kin$ID2 %in% pheno_file$IID & !(kin$ID1 %in% pheno_file$IID), 2,
			ifelse(kin$ID1 %in% pheno_file$IID & kin$ID2 %in% pheno_file$IID, 3, 0)))

## Coding for DKA. 1 = only ID1 has DKA, 2 = only ID2 has DKA, 3 = both have DKA, 0 = neither has DKA

DKA = pheno_file[pheno_file$DKA == 1, 1] # IID's of patients with DKA
kin$dka = ifelse(kin$diabetes == 3 & kin$ID1 %in% DKA$IID & !(kin$ID2 %in% DKA$IID), 1,
			ifelse(kin$diabetes == 3 & kin$ID2 %in% DKA$IID & !(kin$ID1 %in% DKA$IID), 2,
			ifelse(kin$diabetes == 3 & kin$ID1 %in% DKA$IID & kin$ID2 %in% DKA$IID, 3, 0)))

## removing one related patient. Prioritizing patients with diabetes and DKA

set.seed(1235)
kin$temp<-sample(1:2,dim(kin)[1],replace=T)

kin$exclude = with(kin, ifelse(diabetes == 1, ID2,
		ifelse(diabetes == 2, ID1, 
		ifelse(diabetes ==3 & dka == 1, ID2,
		ifelse(diabetes == 3 & dka == 2, ID1,
		ifelse(temp ==1, ID1, ID2))))))

# length(unique(kin$exclude)) #27653
exclude<-c(kin$exclude,dup)
# length(exclude) # 37429 = 25653+9776
# length(unique(exclude)) # 37429

before = nrow(covar_pheno)
covar_pheno = subset(covar_pheno, !covar_pheno$IID %in% exclude)
after = nrow(covar_pheno)
cat("\nRemoved", before-after, "patients\n\n")

## Final summary statistics

cat("\n Summary Stats for unrelated white Europeans:\n")

#Height
Height = calc(covar_pheno$Height)
Height =  paste0(round(Height[1],2),"(",round(Height[2],2),")")

#Weight
Weight = calc(covar_pheno$Weight)
Weight =  paste0(round(Weight[1],2),"(",round(Weight[2],2),")")

#BMI
BMI = calc(covar_pheno$BMI)
BMI =  paste0(round(BMI[1],2),"(",round(BMI[2],2),")")

#Age
Age = calc(covar_pheno$Age_enrolled)
Age =  paste0(round(Age[1],2),"(",round(Age[2],2),")")

#Sex 1=Male, 0=Female
Sex_count = table(covar_pheno$Sex)
female_prop = prop.table(Sex_count)
Sex = paste0(Sex_count[1],"(",round(female_prop[1],2),")")

diabetes = calc2(covar_pheno$Diabetes)
dka = calc2(covar_pheno$DKA)
t1dka = calc2(covar_pheno$T1DKA)
t2dka = calc2(covar_pheno$T2DKA)


Summary_all = as.data.frame(t(data.frame(Sex,Height,Weight,BMI,Age, diabetes, dka, t1dka, t2dka)))
rownames(Summary_all) = c("Female;%","Height","Weight","BMI","Age", "Diabetes","DKA","T1DKA","T2DKA")
colnames(Summary_all) = c("Count(%) / Mean(SD)")


print(Summary_all)
cat("\n\n")


cat("\n Summary Stats for unrelated white Europeans with Diabetes:\n")

diabetes_subset = subset(covar_pheno, Diabetes ==1)
#Height
Height = calc(diabetes_subset$Height)
Height =  paste0(round(Height[1],2),"(",round(Height[2],2),")")

#Weight
Weight = calc(diabetes_subset$Weight)
Weight =  paste0(round(Weight[1],2),"(",round(Weight[2],2),")")

#BMI
BMI = calc(diabetes_subset$BMI)
BMI =  paste0(round(BMI[1],2),"(",round(BMI[2],2),")")

#Age
Age = calc(diabetes_subset$Age_enrolled)
Age =  paste0(round(Age[1],2),"(",round(Age[2],2),")")

#Sex 1=Male, 0=Female
Sex_count = table(diabetes_subset$Sex)
female_prop = prop.table(Sex_count)
Sex = paste0(Sex_count[1],"(",round(female_prop[1],2),")")

diabetes = calc2(diabetes_subset$Diabetes)
dka = calc2(diabetes_subset$DKA)
t1dka = calc2(diabetes_subset$T1DKA)
t2dka = calc2(diabetes_subset$T2DKA)


Summary_all = as.data.frame(t(data.frame(Sex,Height,Weight,BMI,Age, diabetes, dka, t1dka, t2dka)))
rownames(Summary_all) = c("Female;%","Height","Weight","BMI","Age", "Diabetes","DKA","T1DKA","T2DKA")
colnames(Summary_all) = c("Count(%) / Mean(SD)")


print(Summary_all)
cat("\n\n")




cat("Removing Height, Weight, BMI, and Ethnicity columns\n")
covar_pheno = covar_pheno[,-c(2:4,6,7)] # getting rid of ethnicity columns
cat("Final dataset dimensions:\n", dim(covar_pheno), "\nColumn names:\n", 
	paste(names(covar_pheno), "\n"), "Saving file to", getwd(), "\n\n")

write.table(covar_pheno, file = "covar_pheno.txt",row.names=F,quote=F)

cat("DONE!\n\n Saving file with only diabetes patients\n")
diabetes_subset = subset(covar_pheno, Diabetes ==1)
cat("\nFile Dimensions:\n", dim(diabetes_subset), "\nSaving to", getwd(), "as diabetes_DKA.pheno\n\n")

write.table(diabetes_subset, file = "diabetes_DKA.pheno",row.names=F,quote=F)

print("DONE!")

if(tolower(make_plot) == "yes"){
	covar_pheno = data.frame(covar_pheno)
	print("Creating phenotype x Sex plots")
	for( name in pheno_names){
		x = covar_pheno[,name]
		ggplot(covar_pheno, aes(as.factor(x))) + 
		geom_bar(aes(fill = as.factor(Sex)), position = "dodge") + 
		xlab(name) +
		scale_fill_discrete(name = "Sex", labels = c("Female", "Male"))
		ggsave(paste0(name,"xSex.png"))
		cat("\n", name, "plot saved")}}

