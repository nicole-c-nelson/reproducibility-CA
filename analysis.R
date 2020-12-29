library(tidyverse)
library(fs)
library(readxl)
library(FactoMineR)
library(Factoshiny)
library(ggplot2)
library(viridis)
library(ggridges)
library(ggrepel)
library(factoextra)
library(psychometric)
library(moderndive)



##Read IRR and node summary files
IRR_files <- dir_ls("Data/IRR files") #create list of all files in the IRR data folder
summary_files <- dir_ls("Data/Node summary files 2020-05-21") #create list of all files in node summary data folder


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
df_metadata <- read.csv("Data/Metadata 2020-05-26.csv") %>%
  rename(Name = X)

df_metadata <- df_metadata %>% 
  mutate(Name = str_replace(df_metadata$Name, 
                            "[0-9]*[:blank:]\\:[:blank:]", ""))

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

#create variable for author
df_metadata_4 <- df_metadata_3 %>% 
  pivot_longer(2:4) %>% 
  mutate(value = replace(name, value == 0, NA)) %>% #replace cells that have 0 with NA
  drop_na(value) %>%
  select(-value) %>% 
  rename(author = name)

#create variable for term
df_metadata_5 <- df_metadata_4 %>% 
  pivot_longer(2:46) %>% 
  mutate(value = replace(name, value == 0, NA)) %>% #replace cells that have 0 with NA
  drop_na(value) %>%
  select(-value) %>% 
  rename(term = name)

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
      term == "Reproducibility" ~ "reproducibility",
      term == "Reproducibility.crisis" ~ "reproducibility",
      term == "Irreproducibility" ~ "reproducibility",
      term == "Data.reproducibility.crisis" ~ "reproducibility",
      term == "Replication.replicability" ~ "replication",
      term == "Replication.crisis" ~ "replication",
      term == "Replicability.Crisis" ~ "replication",
      TRUE ~ "other"))


##Use IRR scores to select nodes from the coverage data frame
#Create coverage data frame
df_coverage <- summary_files %>%
  map_dfr(read_xlsx, .id = "node") %>% #read in every file; add "node" variable based on file name
  select(node, Name, Coverage) %>% #select 3 relevant variables
  mutate(node = str_sub(node, start = 36, end = -6)) %>% #fix name of nodes
  pivot_wider(names_from = "node", values_from = "Coverage", values_fill = list(Coverage = 0)) #switch to wide data format; fill empty cells with 0

#Create a data frame of nodes reaching the IRR threshold
df_IRR_2 <- df_IRR %>%
  select(Name, Ave_Kappa) %>% #select node name and average kappa score
  filter(Ave_Kappa >= 0.60) %>% #select all nodes with an average Kappa of greater than 0.60
  pivot_wider(names_from = Name, values_from = Ave_Kappa) %>% #pivot so that node names become the variables rather than the rows
  select(-starts_with("Overall")) %>% #get rid of overall average Kappa score as a variable
  mutate(Name = NA)  #add a blank column to this data frame to match up with the "Name" variable in the count and coverage data frames

#Use this IRR threshold data frame to filter the coverage data frame
df_coverage_2 <- df_coverage %>%
  select(colnames(df_IRR_2)) %>% #select nodes matching those in the IRR threshold data frame
  select(Name, everything()) 

#join metadata to coverage dataframe
df_coverage_3 <- inner_join(df_coverage_2, df_metadata_6, by = "Name")

#join auto-coded nodes to coverage dataframe
df_auto_code <-select(df_coverage, Name, contains("(auto)"))
df_coverage_4 <-inner_join(df_coverage_3, df_auto_code, by = "Name")

#set article names to row names for FactomineR analysis
df_coverage_5 <- df_coverage_4 %>%
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column


##Analysis with FactomineR
Factoshiny(df_coverage_5) #open FactoShiny interface

#Correspondence analysis
coverage_CA_result <- CA(df_coverage_5, #perform CA
                         quali.sup = c(30,31,32,33,34), #define qualitative variables as supplementary
                         quanti.sup = c(35,36,37,38,39,40),
                         ncp=18, #retain first 18 dimensions (for later clustering)
                         graph = FALSE) 
view(coverage_CA_result$eig)
view(coverage_CA_result$col$contrib)

Factoshiny(df_coverage_5)

#Investigate(coverage_CA_result) #generate automatic CA report; doesn't work
summary_coverage_CA <- summary.CA(coverage_CA_result, nbelements = Inf, ncp = 5) #output CA summary result, makes a messy object that I'm not sure how to deal with

dimdesc_1_3 <- dimdesc(coverage_CA_result, axes = 1:3, proba = 0.01) #create dimension descriptions

view(dimdesc_1_3$"Dim 2"$quali) #output dimension descriptions
view(dimdesc_1_3$"Dim 1"$category)
view(dimdesc_1_3$`Dim 1`$quanti)

#Calculate confidence intervals for correlations
#NIH
CIr(0.30, 353, level = 0.95)
#Psychology
CIr(-0.29, 353, level = 0.95)
#Term
CI.Rsq(0.14, 353, 3, level = 0.95)
CIr(0.38, 353)
#Audience
CI.Rsq(0.12, 353, 2, level = 0.95)
CIr(0.34, 353)


#Clustering
coverage_HCPC_result <- HCPC(coverage_CA_result, nb.clust=4, consol=TRUE, graph=FALSE) #perform clustering


##Plots using FactoMineR functions
#First factor plane
plot.CA(coverage_CA_result, axes = c(1, 2), #plot first two dimensions
        selectCol='contrib 26', #label only the 8 most contributing nodes
        label = "col", #label only the nodes
        invisible = c("quali.sup"), #make the supplementary variables invisible
        habillage = "repro_repli", #color the articles according to the repro_repli variable
        col.col = "chartreuse4", 
        palette=palette(c("black","red","blue")), #
        graph.type = c("ggplot"),
        title = "Coverage Dim 1/2; eight most contributing nodes")

##Plots using factoextra functions
#Scree plot
fviz_screeplot(coverage_CA_result)+
  geom_hline(yintercept=(1/(29-1)*100),linetype=2, color="red")

fviz_eig(coverage_CA_result, choice="eigenvalue")

#First factor plane
fviz_ca_biplot(coverage_CA_result, repel = TRUE,
               label = "col")
  
fviz_ca_col(coverage_CA_result, repel = TRUE)

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
node_cos2_1 <- coverage_CA_result$col$cos2[,1]
node_cos2_2 <- coverage_CA_result$col$cos2[,2]
node_cos2_3 <- coverage_CA_result$col$cos2[,3]
node_labels <- rownames(coverage_CA_result$col$coord)

sup_coord_1 <- coverage_CA_result$quanti.sup$coord[,1]
sup_coord_2 <- coverage_CA_result$quanti.sup$coord[,2]
sup_labels <- rownames(coverage_CA_result$quanti.sup$coord)

quali_sup_coord_1 <-coverage_CA_result$quali.sup$coord[,1]
quali_sup_coord_2 <-coverage_CA_result$quali.sup$coord[,2]
quali_sup_labels <- rownames(coverage_CA_result$quali.sup$coord)

article_coord <- data.frame("Dim_1" = article_coord_1, 
                            "Dim_2" = article_coord_2,
                            "Dim_3" = article_coord_3,
                            "Dim_4" = article_coord_4) %>%
  mutate(Name = article_labels)

node_coord <- data.frame("Dim_1" = node_coord_1, 
                         "Dim_2" = node_coord_2,
                         "Dim_3" = node_coord_3,
                         "Dim_4" = node_coord_4,
                         "Contrib_1" = node_contrib_1, 
                         "Contrib_2" = node_contrib_2,
                         "Contrib_3" = node_contrib_3,
                         "Contrib_4" = node_contrib_4,
                         "Cos2_1" = node_cos2_1,
                         "Cos2_2" = node_cos2_2,
                         "Cos2_3" = node_cos2_3) %>%
  mutate(Name = node_labels) %>% 
  mutate("Contrib_1_2" = Contrib_1 + Contrib_2) %>%
  mutate("Contrib_1_3" = Contrib_1 + Contrib_3) %>%
  mutate("Contrib_1_2_3" = Contrib_1 + Contrib_2 + Contrib_3) %>%
  mutate("Cos2_1_2" = Cos2_1 + Cos2_2) %>%
  mutate("Cos2_1_3" = Cos2_1 + Cos2_3) %>%
  mutate("Cos2_1_2_3" = Cos2_1 + Cos2_2 + Cos2_3)
    
    

coverage_HCPC_clusters <- as_tibble(coverage_HCPC_result$data.clust) %>%
  mutate(Name = rownames(coverage_HCPC_result$data.clust)) %>% 
  select(Name, clust)

quant_sup_coord <- data_frame("Dim_1" = sup_coord_1,
                         "Dim_2" = sup_coord_2) %>%
  mutate(Name = sup_labels) %>% 
  filter(Name == "Psychology (auto)" | Name == "NIH (auto)") %>%
  mutate(Name = str_remove(Name, "\\(auto\\)"))

quali_sup_coord <- data_frame("Dim_1" = quali_sup_coord_1,
                              "Dim_2" = quali_sup_coord_2) %>%
  mutate(Name = quali_sup_labels) %>% 
  filter(grepl("repro_repli.rep|audience", Name)) %>%
  mutate(Name = str_remove(Name, "repro_repli.")) %>%
  mutate(Name = str_remove(Name, "audience."))

sup_coord <- bind_rows(quali_sup_coord, quant_sup_coord)
  

article_coord_metadata <- inner_join(article_coord, df_metadata_6, by = "Name")
article_coord_cluster <- inner_join(article_coord, coverage_HCPC_clusters, by = "Name")
article_coord_metadata_cluster <- inner_join(article_coord_metadata, coverage_HCPC_clusters, by = "Name")
article_coord_metadata_auto <- inner_join(article_coord_metadata, df_auto_code, by = "Name")

article_coord_metadata_auto_subset <- article_coord_metadata_auto %>%
  filter(Dim_1<1, Dim_1>-1, Dim_2>-1, Dim_2<1)
node_coord_subset <- node_coord %>%
  filter(Dim_1<1, Dim_1>-1, Dim_2>-1, Dim_2<1)

##Plot first factor plane using ggplot
#First factor plane with repro/repli and most contributing nodes
ggplot(article_coord_metadata_auto, aes(Dim_1,Dim_2)) +
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  geom_point(aes(color = repro_repli)) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1)+
  geom_point(data = node_coord, aes(Dim_1, Dim_2, size=Contrib_1_2), shape = 1)+
  geom_text_repel(data = subset(node_coord, Contrib_1_2 > 4), 
             aes(label = Name), point.padding = 0.25, box.padding = 0.75)+
  geom_point(data=sup_coord, shape=3, color="red",
             aes(x=Dim_1, y=Dim_2))+
  geom_text_repel(data = sup_coord, color="red",
                  aes(label = Name), point.padding = 0.25, box.padding = 0.5)+
  labs(size="Contribution",color="Terms",
       x="Dim 1: 'Discipline' (8.50%)", y="Dim 2: 'Audience' (7.73%)")+
  theme(legend.position = "bottom")

#First factor plane detail with contributions and quality of representation
ggplot(node_coord_subset,
       aes(Dim_1,Dim_2, label=Name))+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  geom_point(aes(size=Contrib_1_2, color=Cos2_1_2))+
  scale_color_viridis(discrete = FALSE, option = "D")+
  geom_text_repel(data = subset(node_coord_subset, Cos2_1_2 > 0.05),
                  point.padding = 0.25, box.padding = 0.5)+
  labs(size="Contribution", color="cos2",
       x="Dimension 1 (8.50%)", y="Dimension 2 (7.73%)")+
  theme(legend.position = "bottom")

#Dims 1 and 3 with clusters
ggplot(article_coord_cluster, aes(Dim_1,Dim_3))+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  geom_point(aes(color = clust))+
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1)+
  geom_point(data = node_coord, aes(Dim_1, Dim_3, size=Contrib_1_3), shape = 1)+
  geom_text_repel(data = subset(node_coord, Contrib_1_3 > 1.7), 
                  aes(label = Name), point.padding = 0.25, box.padding = 0.5)+
  labs(size="Contribution", color="Cluster",
       x="Dim 1: 'Discipine' (8.50%)", y="Dim 3: 'Pereceptions of variation' (6.95%)")+
  theme(legend.position = "bottom")

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

##Descriptive stats about the data set
group_by(df_coverage_3, audience) %>% #articles broken out by audience
  tally()

group_by(df_coverage_3, author) %>% #articles broken out by author type
  tally()

group_by(df_coverage_3, year) %>% #articles broken out by year
  tally()

descriptive_stats <- group_by(df_coverage_3, audience, author, year) %>%
  tally()

write.csv(descriptive_stats, "Outputs/Descriptive_stats_2020-05-26.csv")

##Create data frames for MFA
df_coverage_sorted_by_auth <- df_coverage_3 %>%
  arrange(author) %>%
  select(-c(31:35)) %>%
  column_to_rownames(var = "Name")

df_coverage_sorted_by_auth_2 <- data.frame(t(df_coverage_sorted_by_auth))

df_coverage_sorted_by_aud <- df_coverage_3 %>%
  arrange(audience) %>%
  select(-c(31:35)) %>%
  column_to_rownames(var = "Name")

df_coverage_sorted_by_aud_2 <- data.frame(t(df_coverage_sorted_by_aud))

df_coverage_sorted_by_year <- df_coverage_3 %>%
  arrange(year) %>%
  select(-c(31:35)) %>%
  column_to_rownames(var = "Name")

df_coverage_sorted_by_year_2 <- data.frame(t(df_coverage_sorted_by_year))

#MFA on data sorted by audience
coverage_MFA_aud_result <-MFA(df_coverage_sorted_by_aud_2,
                              group=c(136,217),type=c('f','f'),
                              name.group=c('Popular', 'Scientific'),
                              num.group.sup=c(),graph=FALSE)

plot.MFA(coverage_MFA_aud_result, 
         choix="ind",partial='all',
         lab.par=FALSE,select='contrib  10',
         habillage='group',
         title="MFA by audience, 10 most contributing nodes",
         cex=0.55,cex.main=0.55,cex.axis=0.55)

show(which(coverage_MFA_aud_result$freq$coord[,2]>1.5)) #finding articles that are high on Dim2 for this MFA
show(which(coverage_MFA_aud_result$freq$coord[,2]<(-0.6)))
view(coverage_MFA_aud_result$freq$coord)
view(coverage_MFA_aud_result$ind$contrib)
view(coverage_MFA_aud_result$inertia.ratio)

aud_within_inertia_1 <- coverage_MFA_aud_result$ind$within.inertia[,1]
aud_within_inertia_2 <- coverage_MFA_aud_result$ind$within.inertia[,2]
aud_within_inertia_labels <- rownames(coverage_MFA_aud_result$ind$within.inertia)

aud_within_inertia <- data_frame("Dim_1" = aud_within_inertia_1,
                                 "Dim_2" = aud_within_inertia_2) %>%
  `rownames<-`(aud_within_inertia_labels) %>%
  rownames_to_column(var = "Node") %>%
  mutate(Dim_1_2 = Dim_1 + Dim_2)

#MFA on data sorted by author
Factoshiny(df_coverage_sorted_by_auth_2)

coverage_MFA_auth_result <-MFA(df_coverage_sorted_by_auth_2,
                              group=c(120,25,208),type=c('f','f','f'),
                              name.group=c('Journalist', 'Other', 'Scientist'),
                              num.group.sup=c(2),graph=FALSE)

plot.MFA(coverage_MFA_auth_result, 
         choix="ind",partial='all',
         lab.par=FALSE,select='contrib  10',
         habillage='group',
         title="MFA by author, 10 most contributing nodes",
         cex=0.55,cex.main=0.55,cex.axis=0.55)

view(coverage_MFA_auth_result$ind$within.inertia)
view(coverage_MFA_auth_result$inertia.ratio)

auth_within_inertia_1 <- coverage_MFA_auth_result$ind$within.inertia[,1]
auth_within_inertia_2 <- coverage_MFA_auth_result$ind$within.inertia[,2]
auth_within_inertia_labels <-rownames(coverage_MFA_auth_result$ind$within.inertia)

auth_within_inertia <- data_frame("Dim_1" = auth_within_inertia_1,
                                  "Dim_2" = auth_within_inertia_2) %>%
  `rownames<-`(auth_within_inertia_labels) %>%
  rownames_to_column(var = "Node") %>%
  mutate(Dim_1_2 = Dim_1 + Dim_2)

##Mean article profile by year
#Create data frame for mean article profiles
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
  scale_fill_viridis(discrete = FALSE)+
  labs(fill="% coverage", x="", y="")
    
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


#messing around with ellipses

noise <- function(x, na.rm = FALSE) x + abs(rnorm(1, mean = 0, sd = .001))
multiply1000 <- function(x, na.rm = FALSE) x*1000
multiply100 <- function(x, na.rm = FALSE) x*100
multiply50 <- function(x, na.rm = FALSE) x*50

df_coverage_6 <- df_coverage_4 %>%
  select(c(1:30))%>%
  mutate_at(c(2:30), multiply50) %>%
  column_to_rownames(var = "Name")

CA_result <- CA(df_coverage_6, #perform CA
                graph = FALSE)

ellipseCA(CA_result, ellipse = c("col"), axes = c(1,2), col.row="white")

fviz(CA_result, element = "col", axes = c(1,3), invisible = c("quali"), addEllipses = TRUE)


fviz_cluster(HCPC_result, axes = c(1, 3), geom(c("point")), ellipse = TRUE,
             ellipse.type = "convex",
             ellipse.level = 0.95,)

data("housetasks")
view(housetasks)
res.ca <- CA(housetasks, graph = FALSE)
ellipseCA(res.ca, ellipse=c("col"))
fviz(res.ca, element = "row", addEllipses = TRUE)


data(decathlon)
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup = 13,graph=FALSE)
aa <- cbind.data.frame(decathlon[,13],res.pca$ind$coord)
bb <- coord.ellipse(aa,bary=TRUE)
plot(res.pca,habillage=13,ellipse=bb)

aa <- df_CA_results_articles_2 %>%
  column_to_rownames(var = "Name") %>%
  select(clust, Dim_1, Dim_3)
  cc <- coord.ellipse(aa, bary = TRUE)
plot(CA_result, axes = c(1,3), ellipse=cc)

ellipseCA(CA_result, ellipse = c("col"), method = "multinomial", axes = c(1, 2))

###Code graveyard below! Stuff that I'm not using for now

#Create count data frame
df_count <- summary_files %>%
  map_dfr(read_xlsx, .id = "node") %>% #read in every file; add "node" variable based on file name
  select(node, Name, References) %>% #select 3 relevant variables
  mutate(node = str_sub(node, start = 36, end = -6)) %>% #fix name of nodes
  pivot_wider(names_from = "node", values_from = "References", values_fill = list(References = 0)) %>% #switch to wide data format; fill empty cells with 0
  mutate(Name = str_replace_all(Name, "[^a-zA-Z0-9]", "")) %>% #remove special characters, which cause errors in some people's systems  
  rename(`Bayesian statistics` = `Bayesian stats`, #fix node names
         `Heterogeneity` = `Heterogeneity complexity`,
         `Field differences` = `Stakes differ by fields`,
         `Methods training` = `Training in research methods`,
         `Transparency` = `Transparency of data or methodology`,
         `P values` = `P-values`,
         `Government/NGO actions` = `Governmental or NGO actions`,
         `Brian Nosek/Center for Open Science` = `Brian Nosek and COS`,
         `Publishing culture` = `Journals and publishing culture`) 

df_count_2 <- df_count %>% 
  select(colnames(df_IRR_2)) %>% #select nodes matching those in the filtered IRR table 
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column


#join metadata to coverage dataframe
df_count_3 <- inner_join(df_count_2, df_metadata_5, by = "Name")

#set article names to row names for FactomineR analysis
df_count_4 <- df_count_3 %>%
  column_to_rownames(var = "Name") #set article names to row names, rather than a separate column

result_CA_count <- CA(df_count_2)
ellipseCA(result_CA_count, ellipse = c("col"), axes = c(1,2), col.row="white")


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


