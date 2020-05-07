library(tidyverse)
library(fs)
library(readxl)
library(FactoMineR)
library(Factoshiny)
library(ggplot2)
library(viridis)
library(ggridges)


##Read IRR and node summary files
IRR_files <- dir_ls("Data/IRR files") #create list of all files in the IRR data folder
summary_files <- dir_ls("Data/Node summary files") #create list of all files in node summary data folder


##Create IRR data frame
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


##Create metadata data frame
#Create initial metadata data frame and clean up Name column
df_metadata <- read.csv("Data/Metadata 2020-03-19.csv") %>%
  rename(Name = X)

df_metadata <- df_metadata %>% 
  mutate(Name = str_replace(df_metadata$Name, 
                            "[0-9]*[:blank:]\\:[:blank:]", 
                            ""))

# fix column names 
# get vector of current column names
n <- names(df_metadata)

# fix year columns
# detect elements that have four digits in a row, then replace with those four digits
n2 <- ifelse(str_detect(n, "[[:digit:]]{4}") == TRUE, #test condition
             str_extract(n, "[[:digit:]]{4}"), #yes: extract those 4 digits
             n) #no: don't change the element

#remove the junk in front of the other column names
n3 <- ifelse(str_detect(n2, "\\.{3}.+$") == TRUE, #test condition: has ...
       str_extract(n2, "(?<=aa(Terms|Audience|Journalist)\\.{3}).*"), #yes: extract those 4 digits
       n2) #no: don't change the element

#assign new column names back to df_metadata
colnames(df_metadata) <- n3

#create column for publication year, complicatedly
df_metadata_2 <- df_metadata %>%
  pivot_longer(cols = matches("[[:digit:]]{4}")) %>% #pivot all column with 4 digits in them
  mutate(value = replace(name, value == 0, NA)) %>% #replace cells that have 0 with NA
  drop_na(value) %>% #drop NAs in value
  pivot_wider() %>% #pivot to wide 
  pivot_longer(cols = matches("[[:digit:]]{4}"), #pivot columns with year
               names_to = "year") %>% #into a new "year" column
  drop_na(value) %>% #drop rows that have a value of NA
  select(-value) #drop value column

#create variable for audience
df_metadata_3 <-  df_metadata_2 %>% 
  pivot_longer(2:3) %>% #pivot the two audience variables
  mutate(value = replace(name, value == 0, NA)) %>% #replace cells that have 0 with NA
  drop_na(value) %>%
  select(-value) %>% 
  rename(audience = name)

#create variable for journalist/not journalist
df_metadata_4 <- df_metadata_3 %>% 
  pivot_longer(2:3) %>% 
  mutate(value = replace(name, value == 0, NA)) %>% #replace cells that have 0 with NA
  drop_na(value) %>%
  select(-value) %>% 
  rename(journalist = name)

#create variable for topic
df_metadata_5 <- df_metadata_4 %>% 
  pivot_longer(2:46) %>% 
  mutate(value = replace(name, value == 0, NA)) %>% #replace cells that have 0 with NA
  drop_na(value) %>%
  select(-value) %>% 
  rename(topic = name)

#check for articles that have missing metadata
df_metadata_2 %>% 
  anti_join(df_metadata_5, by = "Name") %>% 
  select(Name)

# alternative way to consolidate the factor levels of the topic column
# df_metadata_5 %>% 
#   mutate(topic = fct_collapse(topic, 
#                "reproducibility" = c("Reproducibility", 
#                                      "Reproducibility.crisis", 
#                                      "Irreproducibility", 
#                                      "Data.reproducibility.crisis"),
#                "replication" = c("Replication.replicability",
#                                  "Replication.crisis",
#                                  "Replicability.Crisis"),
#                other_level = "other"))
# 

#create variable for reproducibility/replication
df_metadata_6 <- df_metadata_5 %>%
  mutate(
    repro_repli = case_when(
      topic == "Reproducibility" ~ "reproducibility",
      topic == "Reproducibility.crisis" ~ "reproducibility",
      topic == "Irreproducibility" ~ "reproducibility",
      topic == "Data.reproducibility.crisis" ~ "reproducibility",
      topic == "Replication.replicability" ~ "replication",
      topic == "Replication.crisis" ~ "replication",
      topic == "Replicability.Crisis" ~ "replication",
      TRUE ~ "other"))


##Create coverage data frame
df_coverage <- summary_files %>%
  map_dfr(read_xlsx, .id = "node") %>% #read in every file; add "node" variable based on file name
  select(node, Name, Coverage) %>% #select 3 relevant variables
  mutate(node = str_sub(node, start = 25, end = -6)) %>% #fix name of nodes
  pivot_wider(names_from = "node", values_from = "Coverage", values_fill = list(Coverage = 0)) #switch to wide data format; fill empty cells with 0

#Use IRR scores to select nodes from the coverage frame
df_IRR_2 <- df_IRR %>%
  select(Name, Ave_Kappa) %>% #select node name and average kappa score
  filter(Ave_Kappa >= 0.60) %>% #select all nodes reaching the IRR threshold
  pivot_wider(names_from = Name, values_from = Ave_Kappa) %>% #pivot so that node names become the variables rather than the rows
  select(-starts_with("Overall")) %>% #get rid of overall average Kappa score as a variable
  mutate(Name = NA)  #add a blank column to this data frame to match up with the "Name" variable in the count and coverage data frames

df_coverage_2 <- df_coverage %>%
  select(colnames(df_IRR_2)) %>% #select nodes matching those in the filtered IRR table
  select(Name, everything()) 

#join metadata to coverage dataframe
df_coverage_3 <- inner_join(df_coverage_2, df_metadata_6, by = "Name")

df_coverage_3 %>%
  filter(year <2016, `2016 Nature survey` > 0)

#set article names to row names for FactomineR analysis
df_coverage_4 <- df_coverage_3 %>%
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column


##Analysis with FactomineR
Factoshiny(df_coverage_4) #open FactoShiny interface

#Correspondence analysis
coverage_CA_result <- CA(df_coverage_4, #perform CA
                         quali.sup = c(30,31,32,33,34), #define qualitative variables as supplementary
                         ncp=18, #retain first 18 dimensions (for later clustering)
                         graph = FALSE) 
show(coverage_CA_result$eig)

#Investigate(coverage_CA_result) #generate automatic CA report; doesn't work
#summary_coverage_CA <- summary.CA(coverage_CA_result, nbelements = Inf, ncp = 5) #output CA summary result, makes a messy object that I'm not sure how to deal with

dimdesc_1_4 <- dimdesc(coverage_CA_result, axes = 1:4, proba = 0.01) #create dimension descriptions

show(dimdesc_1_4$"Dim 2"$quali) #output dimension descriptions
show(dimdesc_1_4$"Dim 2"$category)

#Clustering
coverage_HCPC_result <- HCPC(coverage_CA_result, nb.clust=4, consol=TRUE, graph=FALSE) #perform clustering


##Plot for first factor plane uwsing FactoMineR functions
plot.CA(coverage_CA_result, axes = c(1, 2), #plot first two dimensions
        selectCol='contrib 8', #label only the 8 most contributing nodes
        label = "col", #label only the nodes
        invisible = c("quali.sup"), #make the supplementary variables invisible
        habillage = "repro_repli", #color the articles according to the repro_repli variable
        col.col = "chartreuse4", 
        palette=palette(c("black","red","blue")), #
        graph.type = c("ggplot"),
        title = "Coverage Dim 1/2; eight most contributing nodes"
        )

##Extract data from the CA and clustering objects to use for ggplot
article_coord_1 <- coverage_CA_result$row$coord[,1]
article_coord_2 <- coverage_CA_result$row$coord[,2]
article_coord_3 <- coverage_CA_result$row$coord[,3]
article_coord_4 <- coverage_CA_result$row$coord[,4]
article_labels <- rownames(coverage_CA_result$row$coord)

node_coord_1 <- coverage_CA_result$col$coord[,1]
node_coord_2 <- coverage_CA_result$col$coord[,2]
node_coord_3 <- coverage_CA_result$col$coord[,3]
node_coord_4 <- coverage_CA_result$col$coord[,4]
node_contrib_1 <- coverage_CA_result$col$contrib[,1]
node_contrib_2 <- coverage_CA_result$col$contrib[,2]
node_contrib_3 <- coverage_CA_result$col$contrib[,3]
node_contrib_4 <- coverage_CA_result$col$contrib[,4]
node_labels <- rownames(coverage_CA_result$col$coord)

article_coord <- data.frame("Dim_1" = article_coord_1, 
                            "Dim_2" = article_coord_2,
                            "Dim_3" = article_coord_3,
                            "Dim_4" = article_coord_4) %>%
  `rownames<-`(article_labels) %>%
  rownames_to_column(var = "Name")

node_coord <- data.frame("Dim_1" = node_coord_1, 
                         "Dim_2" = node_coord_2,
                         "Dim_3" = node_coord_3,
                         "Dim_4" = node_coord_4,
                         "Contrib_1" = node_contrib_1, 
                         "Contrib_2" = node_contrib_2,
                         "Contrib_3" = node_contrib_3,
                         "Contrib_4" = node_contrib_4) %>%
  `rownames<-`(node_labels) %>%
  rownames_to_column(var = "Name") %>%
  mutate("Contrib_1_2" = Contrib_1 + Contrib_2) %>%
  mutate("Contrib_3_4" = Contrib_3 + Contrib_4)

HCPC_labels <- rownames(coverage_HCPC_result$data.clust)
coverage_HCPC_clusters <- as_tibble(coverage_HCPC_result$data.clust) %>%
  `rownames<-`(HCPC_labels) %>%
  rownames_to_column(var = "Name") %>%
  select(Name, clust) 

article_coord_metadata <- inner_join(article_coord, df_metadata_6, by = "Name")
article_coord_cluster <- inner_join(article_coord, coverage_HCPC_clusters, by = "Name")
article_coord_metadata_cluster <- inner_join(article_coord_metadata, coverage_HCPC_clusters, by = "Name")


##Plot first factor plane using ggplot
ggplot(article_coord_metadata, aes(Dim_1,Dim_2)) +
  geom_point(aes(color = repro_repli)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  geom_point(data = node_coord, aes(Dim_1, Dim_2), shape = 3) +
  geom_label(data = subset(node_coord, Contrib_1_2 > 4), 
             aes(label = Name),
             nudge_y = 0.1,
             label.size = 0.2)

ggplot(article_coord_metadata, aes(Dim_1,Dim_2)) +
  geom_point(aes(color = repro_repli)) +
  scale_color_manual(values = c("darkgrey","red", "blue")) +
  geom_point(data = node_coord, aes(Dim_1, Dim_2), shape = 3) +
  geom_label(data = subset(node_coord, Contrib_1_2 > 4), 
             aes(label = Name),
             nudge_y = 0.1,
             label.size = 0.2)


##Create subsets of data
#Create subsets for journalist/scientist
df_coverage_journ <- df_coverage_3 %>%
  filter(journalist == "Journalist") %>% #select articles written by journalists
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column

df_coverage_notjourn <- df_coverage_3 %>%
  filter(journalist == "Not.a.journalist") %>% # select articles written by scientists
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column

Factoshiny(df_coverage_journ)
Factoshiny(df_coverage_notjourn)

#Create subsets for popular/scientific
df_coverage_sci <- df_coverage_3 %>%
  filter(audience == "Scientific.audience") %>% #select articles aimed at a scientific audience
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column

df_coverage_pop <- df_coverage_3 %>%
  filter(audience == "Popular.audience") %>% # select articles aimed at a popular audience
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column

Factoshiny(df_coverage_sci)
Factoshiny(df_coverage_pop)


##Create data frames for MFA
df_coverage_sorted_by_journ <- df_coverage_3 %>%
  arrange(journalist) %>%
  select(-c(31:35)) %>%
  column_to_rownames(var = "Name")

df_coverage_sorted_by_journ_2 <- data.frame(t(df_coverage_sorted_by_journ))

Factoshiny(df_coverage_sorted_by_journ_2)

df_coverage_sorted_by_aud <- df_coverage_3 %>%
  arrange(audience) %>%
  select(-c(31:35)) %>%
  column_to_rownames(var = "Name")

df_coverage_sorted_by_aud_2 <- data.frame(t(df_coverage_sorted_by_aud))

Factoshiny(df_coverage_sorted_by_aud_2)

df_coverage_sorted_by_year <- df_coverage_3 %>%
  arrange(year) %>%
  select(-c(31:35)) %>%
  column_to_rownames(var = "Name")

df_coverage_sorted_by_year_2 <- data.frame(t(df_coverage_sorted_by_year))

group_by(df_coverage_3, year) %>%
  tally()

##Mean article profile by year
#Create dataframe for mean article profiles
df_mean_article_year <-df_coverage_3 %>%
  mutate(total_coded = rowSums(.[,2:30])) %>%
  filter(year > 2010) %>%
  filter(year < 2019) %>%
  mutate_at(c(2:30), funs((./total_coded)*100)) %>%
  group_by(year) %>%
  summarise_if(is.numeric, mean) %>%
  select(-c(31)) %>%
  #select("year","Career costs to scientists", "Economic cost", "Impact on policy or habits", 
  #       "Legitimacy of science", "Loss of funding") %>%
  #select("year", "2016 Nature survey", "Amgen or Bayer studies", "Andrew Gelman", 
  #       "Brian Nosek and COS", "Failure to replicate important findings", "John Ioannidis", 
  #      "Popular press coverage", "Retractions") %>%
  #select ("year", "Fraud", "Heterogeneity complexity", "Incentives", 
  #        "Journals and publishing culture", "P-values", "Peer review", 
  #       "Reagents", "Sample size and power") %>%
  #select("year", "Bayesian stats", "Governmental or NGO actions", "Incentives", 
  #      "Journals and publishing culture", "Meta-science", "P-values", "Peer review",
  #       "Pre-registration", "Sample size and power","Training in research methods",
  #       "Transparency of data or methodology") %>%
  select("year", everything()) %>%
  gather(-c(1), key = "node", value = "coverage")

#Plot individual density plots
ggplot(df_mean_article_year,
       aes(x=year, y=coverage, group=node))+
  geom_area()+
  facet_wrap(~node)

#Plot heatmap
ggplot(df_mean_article_year,
       aes(x=year, y=node, fill=coverage))+
  geom_tile()+
  scale_fill_viridis(discrete = FALSE)
    
#Plot something kind of like sparklines (not happy with this yet)
ggplot(df_mean_article_year,
       aes(x=year, y=coverage, group=node))+
  geom_line()+
  facet_grid(rows = vars(node))

#Plot density ridges
ggplot(df_mean_article_year,
       aes(x=year, y=node, height=coverage, group=node))+ 
  geom_density_ridges(stat = "identity", scale = 1.3)


##Conduct MFA with nodes as individuals and years as groups
Factoshiny(df_coverage_sorted_by_year_2)

coverage_MFA_year_result <- MFA(df_coverage_sorted_by_year_2,
             group=c(28,29,23,23,43,62,58,90),
             type=c('f','f','f','f','f','f','f','f'),
             name.group=c('Up to 2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'),
             num.group.sup=c(),graph=FALSE)

#Extract separate analysis by year data for ggplot
Dim_1_2012 <- coverage_MFA_year_result$separate.analyses$`2012`$ind$coord[,1]
Dim_1_2013 <- coverage_MFA_year_result$separate.analyses$`2013`$ind$coord[,1]
Dim_1_2014 <- coverage_MFA_year_result$separate.analyses$`2014`$ind$coord[,1]
Dim_1_2015 <- coverage_MFA_year_result$separate.analyses$`2015`$ind$coord[,1]
Dim_1_2016 <- coverage_MFA_year_result$separate.analyses$`2016`$ind$coord[,1]
Dim_1_2017 <- coverage_MFA_year_result$separate.analyses$`2017`$ind$coord[,1]
Dim_1_2018 <- coverage_MFA_year_result$separate.analyses$`2018`$ind$coord[,1]

Dim_2_2012 <- coverage_MFA_year_result$separate.analyses$`2012`$ind$coord[,2]
Dim_2_2013 <- coverage_MFA_year_result$separate.analyses$`2013`$ind$coord[,2]
Dim_2_2014 <- coverage_MFA_year_result$separate.analyses$`2014`$ind$coord[,2]
Dim_2_2015 <- coverage_MFA_year_result$separate.analyses$`2015`$ind$coord[,2]
Dim_2_2016 <- coverage_MFA_year_result$separate.analyses$`2016`$ind$coord[,2]
Dim_2_2017 <- coverage_MFA_year_result$separate.analyses$`2017`$ind$coord[,2]
Dim_2_2018 <- coverage_MFA_year_result$separate.analyses$`2018`$ind$coord[,2]

Contrib_1_2012 <- coverage_MFA_year_result$separate.analyses$`2012`$ind$contrib[,1]
Contrib_1_2013 <- coverage_MFA_year_result$separate.analyses$`2013`$ind$contrib[,1]
Contrib_1_2014 <- coverage_MFA_year_result$separate.analyses$`2014`$ind$contrib[,1]
Contrib_1_2015 <- coverage_MFA_year_result$separate.analyses$`2015`$ind$contrib[,1]
Contrib_1_2016 <- coverage_MFA_year_result$separate.analyses$`2016`$ind$contrib[,1]
Contrib_1_2017 <- coverage_MFA_year_result$separate.analyses$`2017`$ind$contrib[,1]
Contrib_1_2018 <- coverage_MFA_year_result$separate.analyses$`2018`$ind$contrib[,1]

Contrib_2_2012 <- coverage_MFA_year_result$separate.analyses$`2012`$ind$contrib[,2]
Contrib_2_2013 <- coverage_MFA_year_result$separate.analyses$`2013`$ind$contrib[,2]
Contrib_2_2014 <- coverage_MFA_year_result$separate.analyses$`2014`$ind$contrib[,2]
Contrib_2_2015 <- coverage_MFA_year_result$separate.analyses$`2015`$ind$contrib[,2]
Contrib_2_2016 <- coverage_MFA_year_result$separate.analyses$`2016`$ind$contrib[,2]
Contrib_2_2017 <- coverage_MFA_year_result$separate.analyses$`2017`$ind$contrib[,2]
Contrib_2_2018 <- coverage_MFA_year_result$separate.analyses$`2018`$ind$contrib[,2]

MFA_node_labels <- rownames(coverage_MFA_year_result$separate.analyses$`2012`$ind$coord)

MFA_1_2 <- data.frame("Dim1_2012" = Dim_1_2012, 
           "Dim1_2013" = Dim_1_2013,
           "Dim1_2014" = Dim_1_2014,
           "Dim1_2015" = Dim_1_2015,
           "Dim1_2016" = Dim_1_2016, 
           "Dim1_2017" = Dim_1_2017,
           "Dim1_2018" = Dim_1_2018,
           "Dim2_2012" = Dim_2_2012,
           "Dim2_2013" = Dim_2_2013,
           "Dim2_2014" = Dim_2_2014,
           "Dim2_2015" = Dim_2_2015,
           "Dim2_2016" = Dim_2_2016, 
           "Dim2_2017" = Dim_2_2017,
           "Dim2_2018" = Dim_2_2018,
           "Contrib_1_2012" = Contrib_1_2012,
           "Contrib_1_2013" = Contrib_1_2013,
           "Contrib_1_2014" = Contrib_1_2014,
           "Contrib_1_2015" = Contrib_1_2015,
           "Contrib_1_2016" = Contrib_1_2016,
           "Contrib_1_2017" = Contrib_1_2017,
           "Contrib_1_2018" = Contrib_1_2018,
           "Contrib_2_2012" = Contrib_2_2012,
           "Contrib_2_2013" = Contrib_2_2013,
           "Contrib_2_2014" = Contrib_2_2014,
           "Contrib_2_2015" = Contrib_2_2015,
           "Contrib_2_2016" = Contrib_2_2016,
           "Contrib_2_2017" = Contrib_2_2017,
           "Contrib_2_2018" = Contrib_2_2018) %>%
             `rownames<-`(MFA_node_labels) %>%
             rownames_to_column(var = "Name") %>%
             mutate("TotalContrib_2012" = Contrib_1_2012 + Contrib_2_2012) %>%
             mutate("TotalContrib_2013" = Contrib_1_2013 + Contrib_2_2013) %>%
             mutate("TotalContrib_2014" = Contrib_1_2014 + Contrib_2_2014) %>%
             mutate("TotalContrib_2015" = Contrib_1_2015 + Contrib_2_2015) %>%
             mutate("TotalContrib_2016" = Contrib_1_2016 + Contrib_2_2016) %>%
             mutate("TotalContrib_2017" = Contrib_1_2017 + Contrib_2_2017) %>%
             mutate("TotalContrib_2018" = Contrib_1_2018 + Contrib_2_2018) 
  
MFA_1_2_long <- MFA_1_2 %>%
  select(-c(16:29)) %>%
  pivot_longer(-Name,
               names_to = c(".value", "Year"), 
               names_sep = "_") %>%
  mutate_if(is.numeric, round, 2)
 
#extract subset analysis by year for ggplot            
MFA_Dim1 <- coverage_MFA_year_result$ind$coord.partiel[,1]
MFA_Dim2 <- coverage_MFA_year_result$ind$coord.partiel[,2]
partial_point_labels <- rownames(coverage_MFA_year_result$ind$coord.partiel)

MFA_partial_points <- data_frame("Dim1" = MFA_Dim1,
                                 "Dim2" = MFA_Dim2) %>%
  `rownames<-`(partial_point_labels) %>%
  rownames_to_column(var = "Name") %>%
  mutate(Year = Name) %>%
  mutate(Name = str_remove(Name, "\\..*")) %>%
  mutate(Year = str_remove(Year, "^.*\\."))

#ggplot for separate analyses by year
ggplot(MFA_1_2_long,
       aes(x=Dim1, y=Dim2, group=Year))+
  geom_point()+
  facet_wrap(~Year)+
  geom_text(data = subset(MFA_1_2_long, TotalContrib > 4), 
             aes(label = Name),
            size = 3, vjust = 0, nudge_y = 0.2, check_overlap = TRUE)

#ggplot for subset analyses by year
ggplot(MFA_partial_points,
       aes(x=Dim1, y=Dim2, group=Year))+
  geom_point()+
  facet_wrap(~Year)+
  geom_text(aes(label=Name),
            size = 2, vjust = 0, nudge_y = 0.2, check_overlap = TRUE)





###Code graveyard below! Stuff that I'm not using for now

#Create count data frame
df_count <- summary_files %>%
  map_dfr(read_xlsx, .id = "node") %>% #read in every file; add "node" variable based on file name
  select(node, Name, References) %>% #select 3 relevant variables
  mutate(node = str_sub(node, start = 25, end = -6)) %>% #fix name of nodes
  pivot_wider(names_from = "node", values_from = "References", values_fill = list(References = 0)) #switch to wide data format; fill empty cells with 0

df_count_2 <- df_count %>% 
  select(colnames(df_IRR_2)) %>% #select nodes matching those in the filtered IRR table
  select(Name, everything()) #put name column first

#join metadata to coverage dataframe
df_count_3 <- inner_join(df_count_2, df_metadata_6, by = "Name")

#set article names to row names for FactomineR analysis
df_count_4 <- df_count_3 %>%
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column

result_CA_count <- Factoshiny(df_count_4)

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

#Correspondence analysis
CA_count <- CA(df_count_2, graph = F, ncp = ncol(df_count_2))
CA_coverage <- CA(df_coverage_2, graph = F, ncp = ncol(df_coverage_2))

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


