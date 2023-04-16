## Script to organize UKB DKA and diabetes data

###### remove everything in environment
rm(list=ls())


r.libs = "/projects/sunlab/R.lib"


###### Load libraries

suppressPackageStartupMessages(library(data.table,lib.loc= r.libs))
suppressPackageStartupMessages(library(crayon,lib.loc= r.libs))
suppressPackageStartupMessages(library(dplyr,lib.loc= r.libs))
suppressPackageStartupMessages(library(cli,lib.loc= r.libs))


###### Loading ukb files 

dka_icd10 = fread("DKA_UKB/ukb_data/ukb_dka_icd10.txt")
dka_icd9 = fread("DKA_UKB/ukb_data/ukb_dka_icd9.txt")

diabetes_icd10 = fread("DKA_UKB/ukb_data/ukb_diabetes_icd10.txt")
diabetes_icd9 = fread("DKA_UKB/ukb_data/ukb_diabetes_icd9.txt")


###### Viewing data

#  DKA ICD 10
table(dka_icd10$diag_icd10)
# E101 E111
# 1498 1051


# DKA ICD 9
table(dka_icd9$diag_icd9)
#  2501 25010 25011 25019
#    8     2     3    14


#  Diabetes ICD 10
table(diabetes_icd10$diag_icd10)
#  E100   E101   E102   E103   E104   E105   E106   E107   E108   E109   E110
#    85   1498   1069   3728   1545    758    230     32     95  18824    252

#  E111   E112   E113   E114   E115   E116   E117   E118   E119   E121   E123
#  1051   4573  12441   5341   4067    809     18    166 264479      2      1

#  E125   E128   E129   E130   E131   E132   E133   E134   E135   E136   E137
#     1      1      4      3     27     12     51      3      5     23      1

#  E138   E139   E140   E141   E142   E143   E144   E145   E146   E147   E148
#     8   1579     25     79     68   1122    116     53     71      3     50

#  E149
# 10177


# Diabetes ICD 9
table(diabetes_icd9$diag_icd9)

# 2500  2501  2503  2504  2505  2509 25000 25001 25009 25010 25011 25019 25029
#   36     8    26    57    12     1    70    33   245     2     3    14     1

# 25099
#    8


# Subsetting dfs 

## T1DKA 
names(dka_icd10)[5] =  "diag"
names(dka_icd9)[5] =  "diag"
dka_icd9$diag = as.character(dka_icd9$diag)
T1DKA = dka_icd10[dka_icd10$diag == "E101", c("eid", "diag")] # 1498 eids  494 unique
T1DKA = rbind(T1DKA, dka_icd9[dka_icd9$diag == "25011", c("eid", "diag")]) # 3 eids 3 unique from icd9
T1DKA = T1DKA[!duplicated(T1DKA$eid),] # 495 eids. Same as Ellen

## T2DKA
T2DKA = dka_icd10[dka_icd10$diag == "E111", c("eid", "diag")] # 1051 eids  444 unique
T2DKA = rbind(T2DKA, dka_icd9[dka_icd9$diag == "25010", c("eid", "diag")]) # 2 eids 2 unique
T2DKA = T2DKA[!duplicated(T2DKA$eid),] # 445 eids. Ellen reported 447

## Unspecified
UnspDKA = dka_icd9[dka_icd9$diag == "2501" | dka_icd9$diag == "25019", c("eid", "diag")] # 22 eids, 12 unique
UnspDKA = UnspDKA[!duplicated(UnspDKA$eid),]

## Any DKA
DKA_any = rbind(T1DKA, T2DKA, UnspDKA)
DKA_any = DKA_any[!duplicated(DKA_any$eid),] # 891 unique eids with DKA of any type. Same as Ellen


## Diabetes (any kind) from UKB ICD 
# 334546 icd 10 eids, 43190 unique
# 516 icd 9 eids, 143 unique
names(diabetes_icd10)[5] =  "diag"
names(diabetes_icd9)[5] =  "diag"
Diabetes = rbind(diabetes_icd10[,c("eid", "diag")],diabetes_icd9[,c("eid", "diag")])
Diabetes = Diabetes[!duplicated(Diabetes$eid),] # 43220 unique eids


### Self Reported Diabetes 
# Code 20002 = self reported
# From 20002 :  1220 = diabetes, 1222 = T1D, 1223 = T2D

data1 = fread("UKB/Data_clean/ukb27656.csv",stringsAsFactors=F)
self_reported = as.data.frame(select(data1, eid, contains('20002'))) # selecting self-reported data dimensions: 502536 103

# Selecting only rows that have values 1220, 1222, or 1223 in any column
Diabetes_sr = self_reported %>% filter_all(any_vars(. %in% c('1220', '1222', '1223'))) # 26629 eids all unique. Same as Ellen


## Combining diabetes eids
Diabetes_eid = unique(c(Diabetes$eid, Diabetes_sr$eid)) # 47248 unique eids

## Adding DKA data to diabetes data 
DKA_diabetes = data.frame(Diabetes_eid)
names(DKA_diabetes)[1] = "IID"
DKA_diabetes$Diabetes = 1
DKA_diabetes$DKA = ifelse(DKA_diabetes$IID %in% DKA_any$eid,1,0)
DKA_diabetes$T1DKA = ifelse(DKA_diabetes$IID %in% T1DKA$eid,1,0)
DKA_diabetes$T2DKA = ifelse(DKA_diabetes$IID %in% T2DKA$eid,1,0)

write.table(DKA_diabetes, file = "DKA_diabetes.txt", row.names = F, quote = F)


