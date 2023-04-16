##### This prepares plink results for FUMA analysis. It combines the chromosomes output, eliminates entries with MAF<=0.01 

# remove everything in environment
rm(list=ls())

cat("\n \n \n", getwd(), "\n")

# Setting Paths

r.libs = "/projects/sunlab/R.lib"

# Load libraries

suppressPackageStartupMessages(library(data.table,lib.loc= r.libs))
suppressPackageStartupMessages(library(crayon,lib.loc= r.libs))
suppressPackageStartupMessages(library(dplyr,lib.loc= r.libs))
suppressPackageStartupMessages(library(cli,lib.loc= r.libs))

# Function to extract data

multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, fread))
}

## Applying function to extract data

cat("Combining chromosome data \n\n")
path = "out"
data = multmerge(path)

cat("Done! \nDimensions: \n", dim(data), "\n\nRemoving SNPS with MAF <= 0.01 \n")
before = nrow(data)

data = data[data$A1_FREQ>0.01] # Filtering by MAF >0.01

after = nrow(data)
cat("Done! \n\n", before-after, "SNPS removed \n\n")

### Viewing data

n_samples = unlist(data[2,11])
cat("There are", n_samples, "samples. YOU WILL NEED TO NOTE THIS FOR FUMA \n\n")

cat("There are", nrow(data[data$P<5e-08]), 
	"SNPS with P values <5e-08 \n\n")

### Removing columns for FUMA analysis

data = data.frame(data[, c(1:5, 12, 13, 15)])
colnames(data) = c("CHR", "POS", "SNPID", "A0", "A1", "BETA", "SE", "P")

#### Saving data

cat("Saving data to", getwd(), "as MAF01.txt \n 
	Only the following columns are included:\n")

colnames(data)
cat("\n\n")
head(data)
cat("\n\n")

write.table(data, file = "MAF01.txt", row.names = F, quote = F)
cat("\n\n DONE!\n ===========================================================================================================\n\n")

q()
