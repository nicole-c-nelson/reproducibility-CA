library(tidyverse)
library(fs)
library(readxl)
library(FactoMineR)


#Read IRR and node summary files
IRR_files <- dir_ls("Data/IRR files") #create list of all files in the IRR data folder
summary_files <- dir_ls("Data/Node summary files") #create list of all files in node summary data folder

#Create IRR data frame
df_IRR <- IRR_files %>%
  map_dfr(read_csv, .id = "rater") %>% #read in every file; add "rater" variable based on file name
  select(Name, rater, Kappa) %>% #select three relevant variables
  mutate(rater = str_sub(rater, start = 16, end = -15)) %>% #fix name of "rater"
  filter(!grepl(":", Name)) %>% #remove IRR scores for individual articles, leaving only node summary scores
  pivot_wider(names_from = "rater", values_from = "Kappa") %>% #switch to wide data format
  mutate(Name = str_extract(Name, "[^\\\\.]+$")) %>% #fix name of nodes
  na.omit(.) %>% #get rid of rows with NA values
  mutate(Ave_Kappa = rowMeans(.[,-1])) %>% #calculate average Kappa, excluding Name column
  mutate_if(is.numeric, round, 2) %>% #round to two decimal places
  arrange(Ave_Kappa) #sort by average Kappa

#Create metadata data frame
df_metadata <- read.csv("Data/Metadata 2020-01-30.csv") %>%
  rename(Name = X)

###Data prep for metadata
## fix column names for the publication year columns
# get vector of current column names
n <- names(df_metadata)

# detect elements that have four digits in a row, then replace with those four digits
n2 <- ifelse(str_detect(n, "[[:digit:]]{4}") == TRUE, #test condition
             str_extract(n, "[[:digit:]]{4}"), #yes: extract those 4 digits
             n) #no: don't change the element

#assign new column names back to df_metadata
colnames(df_metadata) <- n2

#fix article name column
df_metadata <- df_metadata %>% 
  mutate(Name = str_replace(Name, "[[:digit:]]+[[:blank:]]:[[:blank:]]", #replace stuff up front...
                         "")) #with an empty string

#create column for publication year, complicatedly
df2 <- df_metadata %>%
  pivot_longer(cols = matches("[[:digit:]]{4}")) %>% #pivot all column with 4 digits in them
  mutate(value = replace(name, value == 0, NA)) %>% #replace cells that have 0 with NA
  drop_na(value) %>% #drop NAs in value
  pivot_wider() %>% #pivot to wide 
  pivot_longer(cols = matches("[[:digit:]]{4}"), #pivot columns with year
               names_to = "year") %>% #into a new "year" column
  drop_na(value) %>% #drop rows that have a value of NA
  select(-value) #drop value column


df_metadata %>%
  mutate(Audience = df_metadata$T...Reference.aaAudience...Scientific.audience, case_when(> 0 ~ "scientific audience")

df_metadata <- df_metadata %>%
  mutate_at(vars(matches("T...Reference")), 
            list(~case_when(. > 0 ~ "scientific audience"))) %>%
  mutate_at(vars(matches("U...Reference")),
            list(~case_when(. > 0 ~ "popular audience")))

df_metadata_2 <- gather(df_metadata, 'T...Reference.aaAudience...Scientific.audience', 
                        'U...Reference.aaAudience...Popular.audience', 
                        key = "audience", value = "audience.score")
  
mutate_at(vars(matches("Scientific.audience")), 
            list(~case_when(. > 0 ~ "scientific audience")))
  
  

#Create coverage data frame
df_coverage <- summary_files %>%
  map_dfr(read_xlsx, .id = "node") %>% #read in every file; add "node" variable based on file name
  select(node, Name, Coverage) %>% #select 3 relevant variables
  mutate(node = str_sub(node, start = 25, end = -6)) %>% #fix name of nodes
  pivot_wider(names_from = "node", values_from = "Coverage", values_fill = list(Coverage = 0)) #switch to wide data format; fill empty cells with 0

#Create count data frame
df_count <- summary_files %>%
  map_dfr(read_xlsx, .id = "node") %>% #read in every file; add "node" variable based on file name
  select(node, Name, References) %>% #select 3 relevant variables
  mutate(node = str_sub(node, start = 25, end = -6)) %>% #fix name of nodes
  pivot_wider(names_from = "node", values_from = "References", values_fill = list(References = 0)) #switch to wide data format; fill empty cells with 0

#Use IRR scores to select nodes from the coverage and count data frames
df_IRR_2 <- df_IRR %>%
  select(Name, Ave_Kappa) %>% #select node name and average kappa score
  filter(Ave_Kappa >= 0.60) %>% #select all nodes reaching the IRR threshold
  pivot_wider(names_from = Name, values_from = Ave_Kappa) %>% #pivot so that node names become the variables rather than the rows
  select(-starts_with("Overall")) #get rid of overall average Kappa score as a variable

df_IRR_2$Name <- NA #add a blank column to this data frame to match up with the "Name" variable in the count and coverage data frames

df_count_2 <- df_count %>% 
  select(colnames(df_IRR_2)) %>% #select nodes matching those in the filtered IRR table
  select(Name, everything()) %>% #put name column first
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column

write_csv(df_count_2, "Count_matrix_2020-01-30.csv")

df_coverage_2 <- df_coverage %>%
  select(colnames(df_IRR_2)) %>% #select nodes matching those in the filtered IRR table
  select(Name, everything()) %>% #put name column first
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column

write_csv(df_coverage_2, "Coverage_matrix_2020-01-30.csv")

#Correspondence analysis
CA_count <- CA(df_count_2, graph = F, ncp = ncol(df_count_2))
CA_coverage <- CA(df_coverage_2, graph = F, ncp = ncol(df_coverage_2))

#Plot codes latent dimensions 1 and 2, xlab and ylab %s manual for now
setwd("/Users/ncnelson/Box Sync/Reproducibility Project/NVivo article analysis/Correspondence analysis")

pdf("codes_latent_dimensions_1_2 (count) 2020-01-30.pdf", width = 10, height = 10)
plot(-CA_count$col$coord[,1], CA_count$col$coord[,2], asp = 1, pch = 3, 
     xlab = "Dim 1",
     ylab = "Dim 2")
text(-CA_count$col$coord[,1] + .01, CA_count$col$coord[,2] + .02, 
     labels = rownames(CA_count$col$coord), 
     adj = 0, cex = .4)
dev.off()

pdf("codes_latent_dimensions_1_2 (coverage) 2020-01-30.pdf", width = 10, height = 10)
plot(-CA_coverage$col$coord[,1], CA_coverage$col$coord[,2], asp = 1, pch = 3, 
     xlab = "Dim 1",
     ylab = "Dim 2")
text(-CA_coverage$col$coord[,1] + .01, CA_coverage$col$coord[,2] + .02, 
     labels = rownames(CA_coverage$col$coord), 
     adj = 0, cex = .4)
dev.off()








#Code graveyard below! Stuff that I'm not using for now

#Calculating IRR
#Reading IRR files from NVivo
setwd("/Users/ncnelson/Box Sync/Reproducibility Project/NVivo article analysis/IRR")
KI_JC <- read.csv("KI_JC 2019-11-04.csv")
KI_NCN <- read.csv("KI_NCN 2019-11-04.csv")
JC_NCN <- read_csv("JC_NCN 2019-11-04.csv")

#Remove extra columns, by-article IRR scores, and rename columns
KI_JC_Clean <- KI_JC %>%
  select(c("Name", "Kappa", "Agreement")) %>%
  filter(!grepl(":", Name)) %>%
  rename(KI_JC_Kappa = Kappa, KI_JC_Agreement = Agreement) 

KI_NCN_Clean <- KI_NCN %>%
  select(c("Name", "Kappa", "Agreement")) %>%
  filter(!grepl(":", Name)) %>%
  rename(KI_NCN_Kappa = Kappa, KI_NCN_Agreement = Agreement) 

JC_NCN_Clean <- JC_NCN %>%
  select(c("Name", "Kappa", "Agreement")) %>%
  filter(!grepl(":", Name)) %>%
  rename(JC_NCN_Kappa = Kappa, JC_NCN_Agreement = Agreement) 

#Join IRR files
IRR<- inner_join(KI_JC_Clean, KI_NCN_Clean, by = "Name") %>%
  inner_join(., JC_NCN_Clean, by = "Name")

#Create average Kappa and Agreement scores, reorder columns, sort by Kappa
IRR_2 <-IRR %>%
  mutate(Ave_Kappa = rowMeans(select(., KI_JC_Kappa, KI_NCN_Kappa, JC_NCN_Kappa))) %>%
  mutate(Ave_Agreement = rowMeans(select(., KI_JC_Agreement, KI_NCN_Agreement, JC_NCN_Agreement))) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Name, Ave_Kappa, Ave_Agreement, KI_JC_Kappa, KI_NCN_Kappa, JC_NCN_Kappa, everything()) %>%
  arrange(Ave_Kappa)

#Save as .csv
write_csv(IRR_2, "IRR 2019-11-04.csv")


#Performing correspondence analysis
#Reading NVivo output into R
setwd("/Users/ncnelson/Box Sync/Reproducibility Project/NVivo article analysis/Correspondence analysis")
coding_matrix <- read.csv("Coding matrix (count with auto) 2019-11-06.csv")

#Regular expressions for getting rid of the Excel-to-R artifacts
names(coding_matrix) <- coding_matrix %>% 
  names %>%
  sub("[[:alpha:]]*\\.\\.\\.", "", .) %>% 
  gsub("\\.", " ", .)
coding_matrix$X <- coding_matrix$X %>% 
  as.character %>%
  sub("[[:digit:]]* : ", "", .)

#Don't want a column of names, but set to rownames
coding_matrix_2 <- coding_matrix[,2:ncol(coding_matrix)]
rownames(coding_matrix_2) <- coding_matrix$X

coding_matrix_2 <- coding_matrix[,2:ncol(coding_matrix)]
rownames(coding_matrix_2) <- coding_matrix$"Name"

#For correspondence analysis, we can't have rows or columns of all 0
sum(rowSums(coding_matrix_2) == 0)
sum(colSums(coding_matrix_2) == 0)
coding_matrix_3 <- coding_matrix_2[which(rowSums(coding_matrix_2) > 0), which(colSums(coding_matrix_2) > 0)]
sum(rowSums(coding_matrix_3) == 0)
sum(colSums(coding_matrix_3) == 0)

#Removing codes with poor IRR, manual for now
coding_matrix_4 <- select(coding_matrix_3, -starts_with("Intrinsic"),
                          -starts_with("Other"),
                          -starts_with("Scientists..expectations"),
                          -starts_with("Regulation"),
                          -starts_with("Progress.of.science"),
                          -starts_with("Evidence.synthesis"),
                          -starts_with("Attention.in.scientific"),
                          -starts_with("Good"),
                          -starts_with("Other statistical"),
                          -starts_with("Bias"),
                          -starts_with("Sloppy.research"),
                          -starts_with("Fraud.is.not"),
                          -starts_with("Data.collection"),
                          -starts_with("Personal.ancedotes"),
                          -starts_with("Experimental.design"),
                          -starts_with("Fraud.is.a"),
                          -starts_with("Implausible.findings"),
                          -starts_with("Effect.size"),
                          -starts_with("Impact.on.medicine"),
                          -starts_with("Communication"),
                          -starts_with("Replication"),
                          -starts_with("Heterogeneity.of"),
                          -starts_with("Quantifying"),
                          -starts_with("Epistemology"),
                          -starts_with("Stakes.of.the")
)

#Install package for doing correspondence analysis
install.packages("FactoMineR")
library(FactoMineR)
corresp_analysis <- CA(coding_matrix_3, graph = F, ncp = ncol(coding_matrix_3))

#Scree plot. Second column is percentage variance
pdf("scree_plot (count with auto) 2019-11-06.pdf", width = 10, height = 7.5)
plot(corresp_analysis$eig[,2], type = "l", 
     ylab = "Percentage variance explained", 
     xlab = "Eigenvalue")
abline(v=4, col = 2, lty = 3)
dev.off()

#Plot codes latent dimensions 1 and 2, xlab and ylab %s manual for now
pdf("codes_latent_dimensions_1_2 (count with auto) 2019-11-06.pdf", width = 10, height = 10)
plot(-corresp_analysis$col$coord[,1], corresp_analysis$col$coord[,2], asp = 1, pch = 3, 
     xlab = "Dim 1",
     ylab = "Dim 2")
text(-corresp_analysis$col$coord[,1] + .01, corresp_analysis$col$coord[,2] + .02, 
     labels = rownames(corresp_analysis$col$coord), 
     adj = 0, cex = .4)
dev.off()

#Plot codes latent dimensions 1 and 3
pdf("codes_latent_dimensions_1_3 (no auto) 2019-10-29.pdf", width = 10, height = 10)
plot(-corresp_analysis$col$coord[,1], corresp_analysis$col$coord[,3], asp = 1, pch = 3, 
     xlab = "Dim 1",
     ylab = "Dim 3")
text(-corresp_analysis$col$coord[,1] + .01, corresp_analysis$col$coord[,3] + .02, 
     labels = rownames(corresp_analysis$col$coord), 
     adj = 0, cex = .4)
dev.off()

#Plot codes latent dimensions 1 and 4
pdf("codes_latent_dimensions_1_4 (no auto) 2019-10-29.pdf", width = 10, height = 10)
plot(-corresp_analysis$col$coord[,1], corresp_analysis$col$coord[,4], asp = 1, pch = 3, 
     xlab = "Dim 1",
     ylab = "Dim 4")
text(-corresp_analysis$col$coord[,1] + .01, corresp_analysis$col$coord[,4] + .02, 
     labels = rownames(corresp_analysis$col$coord), 
     adj = 0, cex = .4)
dev.off()

#Plot codes latent dimensions 3 and 4
pdf("codes_latent_dimensions_3_4 (count with auto) 2019-11-06.pdf", width = 10, height = 10)
plot(-corresp_analysis$col$coord[,3], corresp_analysis$col$coord[,4], asp = 1, pch = 3, 
     xlab = "Dim 3",
     ylab = "Dim 4")
text(-corresp_analysis$col$coord[,3] + .01, corresp_analysis$col$coord[,4] + .02, 
     labels = rownames(corresp_analysis$col$coord), 
     adj = 0, cex = .4)
dev.off()

#Plot article latent dimensions 1 and 3
pdf("article_latent_dimensions_1_3 2019-08-28.pdf", width = 10, height = 10)
plot(-corresp_analysis$row$coord[,1], corresp_analysis$row$coord[,3], asp = 1, pch = 3, 
     xlab = "Dim 1",
     ylab = "Dim 3")
text(-corresp_analysis$row$coord[,1] + .01, corresp_analysis$row$coord[,3] + .02, 
     labels = rownames(corresp_analysis$row$coord), 
     adj = 0, cex = .25)
dev.off()

#Plot article latent dimensions 3 and 4
pdf("article_latent_dimensions_3_4 2019-08-28.pdf", width = 10, height = 10)
plot(-corresp_analysis$row$coord[,3], corresp_analysis$row$coord[,4], asp = 1, pch = 3, 
     xlab = "Dim 3",
     ylab = "Dim 4")
text(-corresp_analysis$row$coord[,3] + .01, corresp_analysis$row$coord[,4] + .02, 
     labels = rownames(corresp_analysis$row$coord), 
     adj = 0, cex = .25)
dev.off()


