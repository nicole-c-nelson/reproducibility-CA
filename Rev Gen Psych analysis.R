###Analysis for Review of General Psychology article

##Load libraries for analysis
library(tidyverse)
library(fs)
library(readxl)
library(FactoMineR)
library(ggplot2)
library(viridis)
library(ggrepel)


##Create a data frame of IRR scores
IRR_files <- dir_ls("Data/IRR files") #create list of all files in the IRR data folder

df_IRR <- IRR_files %>%
  map_dfr(read_csv, .id = "rater") %>% #read in every file; add "rater" variable based on file name
  select(Name, rater, Kappa) %>% #select three relevant variables
  mutate(rater = str_sub(rater, start = 16, end = -15)) %>% #fix name of "rater"
  filter(!grepl(":", Name)) %>% #remove IRR scores for individual articles, leaving only node summary scores
  pivot_wider(names_from = "rater", values_from = "Kappa") %>% #switch to wide data format
  mutate(Name = str_extract(Name, "[^\\\\.]+$")) %>% #fix name of nodes
  mutate(Name = case_when(Name == "Bayesian stats" ~ "Bayesian statistics", #clean up node names
                          Name == "Heterogeneity complexity" ~ "Heterogeneity",
                          Name == "Stakes differ by fields" ~ "Field differences",
                          Name == "Training in research methods" ~ "Methods training",
                          Name == "Transparency of data or methodology" ~ "Transparency",
                          Name == "P-values" ~ "P values",#Nature style
                          Name == "Governmental or NGO actions" ~ "Government/NGO actions",
                          Name == "Brian Nosek and COS" ~ "Brian Nosek/Center for Open Science",
                          Name == "Journals and publishing culture" ~ "Publishing culture",
                          TRUE ~ Name)) %>% 
  na.omit(.) %>% #get rid of rows with NA values
  mutate(Ave_Kappa = rowMeans(.[,-1])) %>% #calculate average Kappa, excluding Name column
  arrange(Ave_Kappa) #sort by average Kappa


##Create a data frame of node/code coverage
summary_files <- dir_ls("Data/Node summary files 2020-05-21") #create list of all files in node summary data folder

df_coverage <- summary_files %>%
  map_dfr(read_xlsx, .id = "node") %>% #read in every file; add "node" variable based on file name
  select(node, Name, Coverage) %>% #select 3 relevant variables
  mutate(node = str_sub(node, start = 36, end = -6)) %>% #fix name of nodes
  pivot_wider(names_from = "node", values_from = "Coverage", values_fill = list(Coverage = 0)) %>% #switch to wide data format; fill empty cells with 0
  rename(`Bayesian statistics` = `Bayesian stats`, #fix node names
         `Heterogeneity` = `Heterogeneity complexity`,
         `Field differences` = `Stakes differ by fields`,
         `Methods training` = `Training in research methods`,
         `Transparency` = `Transparency of data or methodology`,
         `P values` = `P-values`,
         `Government/NGO actions` = `Governmental or NGO actions`,
         `Brian Nosek/Center for Open Science` = `Brian Nosek and COS`,
         `Publishing culture` = `Journals and publishing culture`) 


##Create a coverage data frame that includes only the nodes reaching the IRR threshold
#Filter the IRR data frame
df_IRR_2 <- df_IRR %>%
  select(Name, Ave_Kappa) %>% #select node name and average kappa score
  filter(Ave_Kappa >= 0.60) %>% #select all nodes with an average Kappa of greater than 0.60
  pivot_wider(names_from = Name, values_from = Ave_Kappa) %>% #pivot so that node names become the variables rather than the rows
  select(-starts_with("Overall")) %>% #get rid of overall average Kappa score as a variable
  mutate(Name = NA)  #add a blank column to this data frame to match up with the "Name" variable in the coverage data frame

#Use this filtered IRR data frame to filter the coverage data frame
df_coverage_2 <- df_coverage %>%
  mutate(Name = str_replace_all(Name, "[^a-zA-Z0-9]", "")) %>% #remove special characters, which cause errors in some people's systems
  select(colnames(df_IRR_2)) %>% #select nodes matching those in the filtered IRR data frame
  select(Name, everything()) #put the Name column first


##Create a data frame of the publication year metadata
df_metadata <- read.csv("Data/Metadata 2020-05-26.csv") %>% #create the initial metadata data frame
  rename(Name = X)

df_metadata_2 <- df_metadata %>% 
  mutate(Name = str_replace(df_metadata$Name, 
                            "[0-9]*[:blank:]\\:[:blank:]", "")) %>% #clean up the Name column
  mutate(Name = str_replace_all(Name, "[^a-zA-Z0-9]", "")) %>% #remove special characters
  select(1:19) %>% #select columns with publication year data
  pivot_longer(cols = matches("[[:digit:]]{4}")) %>% #pivot all column with 4 digits in them
  mutate(value = replace(name, value == 0, NA)) %>% #replace cells that have 0 with NA
  drop_na(value) %>% #drop NAs in value
  pivot_wider() %>% #pivot to wide 
  pivot_longer(cols = matches("[[:digit:]]{4}"), #pivot columns with year
               names_to = "Year") %>% #into a new "year" column
  drop_na(value) %>% #drop rows that have a value of NA
  select(-value) %>% #drop value column
  mutate(Year = str_extract(Year, "[[:digit:]]{4}")) #clean up year entries

##Create a data frame of auto-coded nodes
df_auto_code <-select(df_coverage, Name, contains("(auto)")) %>%
  mutate_at(vars(-Name), funs(.*100)) %>% #multiply by 100
  mutate(Name = str_replace_all(Name, "[^a-zA-Z0-9]", "")) #remove special characters

##Join coverage and metadata into a single data frame
df_coverage_3 <- inner_join(df_coverage_2, df_metadata_2, by = "Name") %>% #combine coverage and publication year
  mutate(Year = fct_collapse(Year, #create groups by publication year
                             "2011/12" = c("2011", "2012"),
                             "2013/14" = c("2013", "2014"),
                             "2015/16" = c("2015", "2016"),
                             "2017/18" = c("2017", "2018"),
                             other_level = "Before 2011")) %>%
  arrange(Year) #sort by year

year_group_count <- df_coverage_3 %>% #tally of number of articles in each year 
  group_by(Year) %>%
  tally() %>%
  pull(n) #extract the tally as a vector for setting up FactoMineR analysis

df_coverage_4 <-select(df_coverage_3, -Year) %>% #remove the year column
  column_to_rownames(var = "Name") #set article names to row names for FactoMineR analysis

df_coverage_5 <- data.frame(t(df_coverage_4)) #make the nodes the rows instead of the columns

#Create data frame for mean article profiles by year grouping
df_mean_article_year <-df_coverage_3 %>%
  filter(Year != "Before 2011") %>% #remove articles published before 2011
  select(Name, Year, everything()) %>% #move the year column to the front
  mutate(total_coded = rowSums(.[3:31])) %>% # calculate the total % of text coded in each article
  mutate_at(c(3:31), funs((./total_coded)*100)) %>% #for each node, calculate the % of text coded as a % of total text coded
  group_by(Year) %>% #grouping by the year column doesn't do anything visible, but it allows the next calcuation to be performed
  summarise_if(is.numeric, mean) %>% #calculate the mean % of text coded for each node in year year grouping
  select(-c(31)) %>% #remove the % of text coded column
  select(Year, everything()) %>% #move the year column to the front
  gather(-c(1), key = "Node", value = "mean_%_coverage") #reformat the data from wide to long


##Perform subset correspondence analysis
MFA_result <- MFA(df_coverage_5,
                  group = year_group_count, #group by publication year
                  type = c('f','f','f','f','f'), #specify type of data (frequency)
                  name.group = c('2011/12','2013/14','2015/16','2017/18','Before 2011'), #name the groups
                  num.group.sup=c(5), #define the before 2011 group as supplementary
                  graph=FALSE)


##Extract values from the FactoMineR object to use in ggplot
#Extract the partial coordinates for the nodes
MFA_dim1_nodes <- MFA_result$ind$coord.partiel[,1] 
MFA_dim2_nodes <- MFA_result$ind$coord.partiel[,2]
MFA_node_labels <- rownames(MFA_result$ind$coord.partiel)

#Extract the global coordinates for the nodes
MFA_dim1_nodes_overall <- MFA_result$ind$coord[,1]
MFA_dim2_nodes_overall <- MFA_result$ind$coord[,2]
MFA_node_labels_overall <- rownames(MFA_result$ind$coord)

#Combine node coordinates into data frames
df_MFA_nodes <- data_frame("Dim1" = MFA_dim1_nodes,
                                 "Dim2" = MFA_dim2_nodes) %>%
  `rownames<-`(MFA_node_labels) %>%
  rownames_to_column(var = "Node") %>% #make the row names into their own column
  mutate(Year = Node) %>% #copy the node data into a new column named Year
  mutate(Node = str_remove(Node, "\\..*")) %>% #remove the year info from the Node column
  mutate(Year = str_remove(Year, "^.*\\.")) %>% #remove the node info from the Year column
  mutate(dist_org = sqrt(Dim1^2 + Dim2^2)) #create a new variable for distance from the origin


df_MFA_nodes_overall <- data_frame("Dim1_overall" = MFA_dim1_nodes_overall,
                                   "Dim2_overall" = MFA_dim2_nodes_overall) %>%
  `rownames<-`(MFA_node_labels_overall) %>%
  rownames_to_column(var = "Node") %>%
  mutate(dist_org_overall = sqrt(Dim1_overall^2 + Dim2_overall^2)) #create a new variable for distance from the origin

#Extract the contribution and cos2 information for the nodes
MFA_contrib1_nodes <- MFA_result$ind$contrib[,1]
MFA_contrib2_nodes <- MFA_result$ind$contrib[,2]
MFA_cos2_1_nodes <- MFA_result$ind$cos2[,1]
MFA_cos2_2_nodes <- MFA_result$ind$cos2[,2]
MFA_contrib_labels <- rownames(MFA_result$ind$contrib)

#Combine contribution and cos2 information into a data frame
df_MFA_node_contrib_cos <- data_frame("Contrib1" = MFA_contrib1_nodes,
                               "Contrib2" = MFA_contrib2_nodes,
                               "Cos2_1" = MFA_cos2_1_nodes,
                               "Cos2_2" = MFA_cos2_2_nodes) %>%
  `rownames<-`(MFA_contrib_labels) %>%
  rownames_to_column(var = "Node") %>% #make the row names into their own column
  mutate(Contrib1_2 = Contrib1 + Contrib2) %>% #create a new variable for contribution on the first factor plane
  mutate(Cos2_1_2 = Cos2_1 + Cos2_2) #create a new variable for cos2 on the first factor plane

#Combine node coordinates and contribution/cos2 information
df_MFA_nodes_2 <-inner_join(df_MFA_nodes_overall, df_MFA_node_contrib_cos, by = "Node")
df_MFA_nodes_3 <- inner_join(df_MFA_nodes_2, df_MFA_nodes, by = "Node")
df_MFA_nodes_4 <- inner_join(df_MFA_nodes_3, df_mean_article_year, by = c("Year" = "Year", "Node" = "Node")) %>%
  mutate_if(is.numeric, round, 2) %>% #round to two decimal places
  select(Node, Year, Dim1, Dim2, dist_org, `mean_%_coverage`, everything())

#Extract coordinates, contribution, and cos2 for articles
MFA_dim1_articles <- MFA_result$freq$coord[,1]
MFA_dim2_articles <- MFA_result$freq$coord[,2]
MFA_contrib1_articles <- MFA_result$freq$contrib[,1]
MFA_contrib2_articles <- MFA_result$freq$contrib[,2]
MFA_cos2_1_articles <- MFA_result$freq$cos2[,1]
MFA_cos2_2_articles <- MFA_result$freq$cos2[,2]
MFA_article_labels <- rownames(MFA_result$freq$coord)

#Combine article information into a data frame
df_MFA_articles <- data_frame("Dim1" = MFA_dim1_articles,
                           "Dim2" = MFA_dim2_articles,
                           "Contrib1" = MFA_contrib1_articles,
                           "Contrib2" = MFA_contrib2_articles,
                           "Cos2_1" = MFA_cos2_1_articles,
                           "Cos2_2" = MFA_cos2_2_articles) %>%
  `rownames<-`(MFA_article_labels) %>%
  rownames_to_column(var = "Name") %>% #make the row names into their own column
  mutate(Name = gsub("^(X)([0-9])", "\\2", Name)) #remove instances where FactoMineR has weirdly added an X in front of the year

df_MFA_articles_2 <- inner_join(df_MFA_articles, df_metadata_2, by="Name") %>%
  mutate(Year = fct_collapse(Year, #collapse the years into four groups matching the original MFA groups
                             "2011/12" = c("2011","2012"),
                             "2013/14" = c("2013","2014"),
                             "2015/16" = c("2015","2016"),
                             "2017/18" = c("2017","2018"))) %>%
  mutate(Contrib1_2 = Contrib1 + Contrib2) %>% #create a new variable for contribution on the first factor plane
  mutate(Cos2_1_2 = Cos2_1 + Cos2_2) #create a new variable for cos2 on the first factor plane
 
df_MFA_articles_3 <- inner_join(df_MFA_articles_2, df_auto_code, by = "Name") %>% #add the auto-code data to the articles data frame
  mutate(Psychology_log_trans = log1p(`Psychology (auto)`))


##Create figures using ggplot
#plot global analysis
ggplot(df_MFA_nodes_2, label=Node,
       aes(x=Dim1_overall, y=Dim2_overall))+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  theme_bw()+
  geom_point(data= df_MFA_articles_3, size=1.2,
             aes(x=Dim1, y=Dim2, color=Psychology_log_trans))+
  scale_color_viridis(direction = -1, option = "D")+
  geom_point(shape=1, aes(size=Contrib1_2))+
  geom_text_repel(data = filter(df_MFA_nodes_2, Contrib1_2 > 3.8),
                  aes(label=Node), 
                  point.padding = 0.25, box.padding = 0.5)+
  labs(size="Contribution", color="Mentions of psychology",
       x="Dim 1: 'Discipline' (8.78%)", y="Dim 2: 'Audience' (7.78%)")+
  theme(legend.position = "bottom")

#plot subsets by year grouping
ggplot(df_MFA_articles_3,
       aes(x=Dim1, y=Dim2, group=Year))+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  theme_bw()+
  geom_point(size=1.2, aes(color=Psychology_log_trans))+
  scale_color_viridis(direction = -1)+
  geom_point(data = subset(df_MFA_nodes_3, Contrib1_2 > 4), shape=1,
             aes(x=Dim1, y=Dim2, size=Contrib1_2, group=Year))+
  geom_point(data = subset(df_MFA_nodes_3, Contrib1_2 < 4), shape=1, alpha=0.4,
             aes(x=Dim1, y=Dim2, size=Contrib1_2, group=Year))+
  geom_text_repel(data= subset(df_MFA_nodes_3, Contrib1_2 > 4), 
                  aes(label=Node),
                  point.padding = 0.25, box.padding = 0.5)+
  facet_wrap(~Year)+
  labs(size="Contribution", color="Mentions of psychology",
       x="Dim 1: 'Discipline' (8.78%)", y="Dim 2: 'Audience' (7.78%)")+
  theme(legend.position = "bottom")




