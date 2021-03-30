library(tidyverse)
library(fs)
library(readxl)
library(FactoMineR)
library(ggplot2)
library(viridis)
library(ggrepel)
library(factoextra)
library(knitr)
library(waffle)
library(moderndive)
library(vegan)

###Read IRR and node summary files
IRR_files <- dir_ls("Data/IRR files") #create list of all files in the IRR data folder
summary_files <- dir_ls("Data/Node summary files 2020-05-21") #create list of all files in node summary data folder


# Create IRR data frame ---------------------------------------------------
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


# Create metadata data frame ----------------------------------------------
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

#create column for publication year
df_metadata_2 <- df_metadata %>%
  mutate(Name = str_replace_all(Name, "[^a-zA-Z0-9]", "")) %>% #remove special characters, which cause problems in some people's systems
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

#create variable for reproducibility/replication
df_metadata_6 <- df_metadata_5 %>% 
  mutate(term = fct_collapse(term,
                              "reproducibility" = c("Reproducibility", 
                                                    "Reproducibility.crisis", 
                                                    "Irreproducibility", 
                                                    "Data.reproducibility.crisis"),
                              "replication" = c("Replication.replicability",
                                                "Replication.crisis",
                                                "Replicability.Crisis"), 
                              other_level = "other"))

#check for articles that have missing metadata
df_metadata_2 %>% 
  anti_join(df_metadata_6, by = "Name") %>% 
  select(Name)



# Create coverage data frame ----------------------------------------------
df_coverage <- summary_files %>%
  map_dfr(read_xlsx, .id = "node") %>% #read in every file; add "node" variable based on file name
  select(node, Name, Coverage) %>% #select 3 relevant variables
  mutate(node = str_sub(node, start = 36, end = -6)) %>% #fix name of nodes
  mutate(Name = str_replace_all(Name, "[^a-zA-Z0-9]", "")) %>% #remove special characters, which cause errors in some people's systems
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


# Create data frames for MFA ----------------------------------------------
df_coverage_sorted_by_auth <- df_coverage_3 %>%
  arrange(author) %>%
  select(-c(31:34)) %>%
  column_to_rownames(var = "Name")

df_coverage_sorted_by_auth_2 <- data.frame(t(df_coverage_sorted_by_auth))

df_coverage_sorted_by_aud <- df_coverage_3 %>%
  arrange(audience) %>%
  select(-c(31:34)) %>%
  column_to_rownames(var = "Name")

df_coverage_sorted_by_aud_2 <- data.frame(t(df_coverage_sorted_by_aud))



# Analysis with FactoMineR ------------------------------------------------
#Correspondence analysis 
CA_result <- CA(df_coverage_5, #perform CA
                         quali.sup = c(30,31,32,33), #define qualitative variables as supplementary
                         quanti.sup = c(34,35,36,37,38,39),
                         ncp=18, #retain first 18 dimensions (for later clustering)
                         graph = FALSE) 

#Create a description of the first three dimensions of the CA
dimdesc_1_3 <- dimdesc(CA_result, axes = 1:3, proba = 0.05)

#View tables of correlations of supplementary variables with Dim 1 and Dim 2
view(dimdesc_1_3$`Dim 1`$quanti) 
view(dimdesc_1_3$`Dim 1`$quali)
view(dimdesc_1_3$`Dim 2`$quali)

#Clustering 
HCPC_result <- HCPC(CA_result, nb.clust=4, consol=TRUE, graph=FALSE) #perform clustering

#MFA on data sorted by audience
MFA_aud_result <-MFA(df_coverage_sorted_by_aud_2,
                              group=c(136,217),type=c('f','f'),
                              name.group=c('Popular', 'Scientific'),
                              num.group.sup=c(),graph=FALSE)

MFA_auth_result <-MFA(df_coverage_sorted_by_auth_2,
                      group=c(120,25,208),type=c('f','f','f'),
                      name.group=c('Journalist', 'Other', 'Scientist'),
                      num.group.sup=c(2),graph=FALSE)


# Bootstrap analysis-------------------------------------------------------

#I think this first way is not going to work because it just gives the coordinates for the supplementary individuals
#It doesn't seem to produce coordinates for where the codes would be based on those supplementary individuals

#df_bootstrap_sample <- df_coverage_2 %>%
  #rep_sample_n(size=353, replace=TRUE, reps = 100)

#df_coverage_bootstrap <- df_bootstrap_sample %>%
  #filter(replicate==1) %>%
  #ungroup() %>%
  #select(-replicate) %>%
  #mutate(Name = paste0(runif(353), Name)) %>% 
  #bind_rows(., df_coverage_2, id=NULL) %>%
  #column_to_rownames(var = "Name")

#CA_bootstrap_result <- CA(df_coverage_bootstrap,
                          #row.sup = c(1:353),
                          #graph = FALSE)

# function to do a single bootstrap replicate of the MFA analysis
# and return a dataframe with the nodes and the coordinates of the
# two dimensions
# takes the dataframe to be analyzed as its only input
bootstrap_MFA <- function(df) {

df_bootstrap_sample <- df %>%
  rep_sample_n(size=353, replace=TRUE, reps = 1)

df_coverage_bootstrap <- df_bootstrap_sample %>%
  ungroup() %>%
  select(-replicate) %>%
  bind_rows(., df, id=NULL) %>%
  bind_rows(., df, id=NULL) %>%
  mutate(Name = paste0(Name, runif(1059))) %>% #random number added because otherwise Names aren't unique and can't be set to row names
  column_to_rownames(var = "Name") #you have to do this for FactoMineR, but I always do this as the last step because tidyverse functions often erase the row names

df_coverage_bootstrap_2 <- data.frame(t(df_coverage_bootstrap))

MFA_bootstrap_result <- MFA(df_coverage_bootstrap_2,
                         group=rep(nrow(df), 3),
                         type=rep('f', 3),
                         #num.group.sup=c(1),
                         graph=FALSE)

return(as.data.frame(MFA_bootstrap_result$separate.analyses$Gr1$ind$coord[,1:2]) %>% 
  rownames_to_column(var = "node"))
}

bootstrap_MFA(df_coverage_2)

#run bootstrap MFA n times and append results
repeat_bootstrap_MFA  <- function(df, n) {
  #set up empty results tibble
  MFA_return <- data.frame(node = character(),
                           Dim.1 = numeric(),
                           Dim.2 = numeric(),
                           replicate = numeric())
  # run n loops of bootstrap MFA function and append results
  for(i in 1:n){
    MFA_result <- bootstrap_MFA(df)
    MFA_result <- MFA_result %>% 
      mutate(replicate = i) #add id column for the replicate
    MFA_return <- bind_rows(MFA_return, MFA_result)
  }
  return(MFA_return)
}

#Another way would be to run MFAs with larger numbers of bootstrap samples in them

nrep <-  1000
df_bootstrap_sample <- df_coverage_2 %>%
  rep_sample_n(size=353, replace=TRUE, reps = nrep)

df_coverage_bootstrap <- df_bootstrap_sample %>%
  ungroup() %>%
  select(-replicate) %>%
  bind_rows(., df_coverage_2, id=NULL) %>%
  mutate(Name = paste0(runif((nrep+1)*353), Name)) %>% #random number added because otherwise Names aren't unique and can't be set to row names
  column_to_rownames(var = "Name") 

df_coverage_bootstrap_2 <- data.frame(t(df_coverage_bootstrap))

MFA_bootstrap_result <- MFA(df_coverage_bootstrap_2,
                            group=rep(353,(nrep+1)),
                            type=rep('f',(nrep+1)),
                            graph=FALSE)

#extract node coordinates from MFA object
bootstrap_nodes_rownames <- rownames(MFA_bootstrap_result$ind$coord)

df_bootstrap_nodes <- as_tibble(MFA_bootstrap_result$ind$coord) %>%
  `rownames<-`(bootstrap_nodes_rownames) %>%
  rownames_to_column(var = "Name") %>%
  mutate(Group = as.numeric(str_replace_all(Name, "[^0-9]", ""))) %>%
  mutate(Node = str_remove(Name, "\\..*")) %>%
  select(-Name) 
# saveRDS(df_bootstrap_nodes, file = "df_bootstrap_nodes_1000.RDS")

#extract partial points from MFA object
bootstrap_rownames <- rownames(MFA_bootstrap_result$ind$coord.partiel)

df_bootstrap_partial_points <- as_tibble(MFA_bootstrap_result$ind$coord.partiel) %>%
  `rownames<-`(bootstrap_rownames) %>%
  rownames_to_column(var = "Name") %>%
  mutate(Group = as.numeric(str_replace_all(Name, "[^0-9]", ""))) %>%
  mutate(Node = str_remove(Name, "\\..*")) %>%
  select(-Name) 
# saveRDS(df_bootstrap_partial_points, file = "df_bootstrap_partial_points_1000.RDS")

#read the RDS files
df_bootstrap_partial_points <- readRDS("df_bootstrap_partial_points_1000.RDS")
df_bootstrap_nodes <- readRDS("df_bootstrap_nodes_1000.RDS")

# function to peel convex hull of the point cloud for each node
hull_peel <- function(df, threshold = 0.95) {
  nboot <- nrow(df) #calculate number of replicates
  #loop to peel hull points until threshold is reached
  repeat {
  hpts <- chull(df$Dim.1, df$Dim.2) #get indices of hull points
  npts <- nrow(df[-hpts,]) #calculate number of points remaining after removing the hull points
  if(npts/nboot < threshold) break #check whether proportion of remaining points to original number of replicates is below threshold
  df <- df[-hpts,] #remove hull points
  }
  return(df[hpts,]) #return hull points
}

# apply hull_peel function for each node
df_bootstrap_hull <- df_bootstrap_partial_points %>% 
  group_by(Node) %>% 
  group_modify(~ hull_peel(.x))

#plot bootstrap samples and hulls individually
ggplot(df_bootstrap_partial_points, aes(x=Dim.1, y=Dim.2, fill=Node))+
  geom_point(aes(color=Node), size = 0.7)+
  geom_point(data=filter(df_bootstrap_partial_points, Group == (1001)), color="black", shape=17, size=3)+
  geom_text_repel(data=filter(df_bootstrap_partial_points, Group == 1001), 
                  aes(label=Node), point.padding = 0.25, box.padding = 0.5)+
  geom_polygon(data = df_bootstrap_hull, alpha = 0.25)+
  facet_wrap(~Node)


#plot a subset of the nodes

node_filter <- function(df) {
  df %>%
  filter(Node %in% c("Bayesian statistics",
                     "Brian Nosek/Center for Open Science",
                     "Fraud",
                     "Heterogeneity",
                     "Impact on policy or habits",
                     "Incentives",
                     "Legitimacy of science",
                     "P values",
                     "Pre-registration",
                     "Publishing culture",
                     "Reagents",
                     "Retractions",
                     "Sample size and power",
                     "Transparency"))
}

df_bootstrap_partial_points_2 <- df_bootstrap_partial_points %>%
  node_filter()
df_bootstrap_hull_2 <- df_bootstrap_hull %>%
  node_filter()
df_bootstrap_nodes_2 <- df_bootstrap_nodes %>%
  node_filter()

#create data frame for original and bootstrap theme points
df_original_bootstrap <- df_bootstrap_partial_points_2 %>% 
  filter(Group == 1001) %>% 
  mutate(sample = "Original sample") %>% 
  rbind(df_bootstrap_nodes_2 %>% mutate(sample = "Bootstrap sample"))

##Figure 5 bootstrap---------------
ggplot(df_bootstrap_partial_points_2, aes(x=Dim.1, y=Dim.2, fill=Node))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  #geom_point(aes(color=Node), size=0.5, show.legend = F)+
  geom_polygon(data = df_bootstrap_hull_2, alpha = 0.4, show.legend = F)+
  geom_point(data = df_original_bootstrap, aes(shape = sample), size = 3)+
  guides(fill = F) +
  scale_shape_manual(name = element_blank(), values = c(15,22)) +
  geom_line(data = rbind(df_bootstrap_partial_points_2 %>% filter(Group == 1001), df_bootstrap_nodes_2),  aes(group=Node), linetype=2, show.legend = F)+
  geom_text_repel(data=filter(df_bootstrap_partial_points_2, Group == 1001), aes(label=Node), show.legend = F)+
  labs(x= "Dimension 1: ‘Discipline’ (8.38%)", y = "Dimension 2: ‘Audience’ (7.65%)")+
  theme(legend.position="bottom")
## exported as SVG with 650x650
## manual edits in Inkscape
## reduce alpha for bootstrap sample points to 70
## remove connecting lines between bootstrap/sample points when they're touching
## move labels and add label lines
## change line type for connecting lines to finer dash

##Momin's code
df <- as.data.frame(df_coverage_2)
rownames(df) <- df$Name
df <- df[,-1]

nboot <- 1000
boot <- array(data = NA,
              dim = c(nboot, 2, ncol(df)),
              dimnames = list(bootiter = 1:nboot,
                              coords = c("Dim 1", "Dim 2"),
                              col = names(df)))
errors <- rep(NA, nboot)
for (i in 1:nboot) {
  CA_boot <- CA(df[sample(x = 1:nrow(df),
                          replace = T),],
                ncp = 2,
                graph = FALSE)
  rotated <- vegan::procrustes(CA_result$col$coord[,1:2],
                               CA_boot$col$coord[,1:2])
  errors[i] <- rotated$ss
  boot[i,,] <- t(fitted(rotated))
  if (i%%100==0) {print(i)}
}

hist(errors, breaks = 500)
hist(log(errors), breaks = 100)

plot(CA_result$col$coord[,1:2], pch = 19, cex = .2,
     ylim = c(-2, 3),
     xlim = c(-3, 3))
points(apply(boot, c(3,2), median), pch = 19, cex = .2, col = "lightgray") #calculates median coords for each node
for (i in 1:ncol(df)) {
  points <- boot[,,i] #extract first 1000 coords for node i
  repeat { # http://carme-n.org/?sec=code2
    hpts <- chull(points)
    npts <- nrow(points[-hpts,]) #next number of points in peeled set
    if(npts/nboot < 0.5) break #keep repeating until half of points have been removed
    points <- points[-hpts,] #remove the hulled points
  }
  hpts <- c(hpts,hpts[1])
  lines(points[hpts,], lty = 3)
}


# Figure 1 ----------------------------------------------------------------
#Create data frames of article attributes
df_article_attrib <- df_coverage_3 %>%
  mutate(Audience = str_remove(audience, "\\..*")) %>%
  group_by(Audience, author) %>%
  tally() %>%
  pivot_wider(names_from = Audience, values_from =n) %>%
  mutate(Popular = Popular/sum(Popular)*100) %>%
  mutate(Scientific = Scientific/sum(Scientific)*100) %>%
  pivot_longer(-author, names_to = "Audience", values_to = "Percentage")

df_article_attrib_2 <- df_coverage_3 %>%
  mutate(Audience = str_remove(audience, "\\..*")) %>%
  group_by(Audience, author, year) %>%
  tally() 

#Plot Fig 1a using ggplot


cols <- viridis(3)


df_article_attrib_2 %>% 
  mutate(author = as.factor(author)) %>% 
  mutate(author = fct_relevel(author, "Journalist", "Scientist")) %>% 
  arrange(author) %>% 
  ggplot(aes(fill=author, values = n))+
  geom_waffle(n_rows = 10, flip = T, color = "white")+
  facet_wrap(~ Audience,
             labeller = labeller(Audience = c(Popular = "Popular (n = 136)",
                                              Scientific = "Scientific (n = 217)"))) +
  coord_equal() +
  scale_fill_manual(values = c(cols[1], cols[3], cols[2]),
                    breaks = c("Journalist", "Scientist", "Other")) +
  theme_void()+
  theme_enhance_waffle() +
  theme_void()+
  theme(legend.position = "bottom") +
  labs(fill = "Author")

## saved to svg with 550x550 px size
## edited in Inkscape to add "a" label


#Plot Fig 1b using ggplot
df_article_attrib_2 %>% 
  mutate(year = case_when(year < 2009 ~ "1996-2008",
                          T ~ year)) %>% 
  ggplot(aes(fill=Audience, x=year, y=n))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  theme(legend.position = "bottom") +
  labs(x = element_blank(), y = "Number of articles") +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust=1.2,hjust=1),
        panel.grid.major.x = element_blank())


# Figure 2 ----------------------------------------------------------------
#Extract data from the CA and clustering objects to use for ggplot
article_coord_1 <- CA_result$row$coord[,1]
article_coord_2 <- CA_result$row$coord[,2]
article_coord_3 <- CA_result$row$coord[,3]
article_labels <- rownames(CA_result$row$coord)

node_coord_1 <- CA_result$col$coord[,1]
node_coord_2 <- CA_result$col$coord[,2]
node_coord_3 <- CA_result$col$coord[,3]
node_contrib_1 <- CA_result$col$contrib[,1]
node_contrib_2 <- CA_result$col$contrib[,2]
node_contrib_3 <- CA_result$col$contrib[,3]
node_cos2_1 <- CA_result$col$cos2[,1]
node_cos2_2 <- CA_result$col$cos2[,2]
node_cos2_3 <- CA_result$col$cos2[,3]
node_labels <- rownames(CA_result$col$coord)

sup_coord_1 <- CA_result$quanti.sup$coord[,1]
sup_coord_2 <- CA_result$quanti.sup$coord[,2]
sup_labels <- rownames(CA_result$quanti.sup$coord)

quali_sup_coord_1 <-CA_result$quali.sup$coord[,1]
quali_sup_coord_2 <-CA_result$quali.sup$coord[,2]
quali_sup_labels <- rownames(CA_result$quali.sup$coord)

df_CA_article_coord <- data.frame("Dim_1" = article_coord_1, 
                            "Dim_2" = article_coord_2,
                            "Dim_3" = article_coord_3) %>%
  `rownames<-`(article_labels) %>%
  rownames_to_column(var = "Name")

df_CA_results_nodes <- data.frame("Dim_1" = node_coord_1, 
                         "Dim_2" = node_coord_2,
                         "Dim_3" = node_coord_3,
                         "Contrib_1" = node_contrib_1, 
                         "Contrib_2" = node_contrib_2,
                         "Contrib_3" = node_contrib_3,
                         "Cos2_1" = node_cos2_1,
                         "Cos2_2" = node_cos2_2,
                         "Cos2_3" = node_cos2_3) %>%
  `rownames<-`(node_labels) %>%
  rownames_to_column(var = "Node") %>%
  mutate("Contrib_1_2" = Contrib_1 + Contrib_2) %>%
  mutate("Contrib_1_3" = Contrib_1 + Contrib_3) %>%
  mutate("Cos2_1_2" = Cos2_1 + Cos2_2) %>%
  mutate("Cos2_1_3" = Cos2_1 + Cos2_3)

HCPC_labels <- rownames(HCPC_result$data.clust)
df_HCPC_clusters <- as_tibble(HCPC_result$data.clust) %>%
  `rownames<-`(HCPC_labels) %>%
  rownames_to_column(var = "Name") %>%
  select(Name, clust) 

df_CA_results_articles <- inner_join(df_CA_article_coord, df_metadata_6, by = "Name")
df_CA_results_articles_2 <- inner_join(df_CA_results_articles, df_HCPC_clusters, by = "Name")

df_CA_quant_sup_var <- data_frame("Dim_1" = sup_coord_1,
                              "Dim_2" = sup_coord_2) %>%
  `rownames<-`(sup_labels) %>%
  rownames_to_column(var = "Name") %>%
  filter(Name == "Psychology (auto)" | Name == "NIH (auto)") %>%
  mutate(Name = str_remove(Name, "\\(auto\\)"))

df_CA_quali_sup_var <- data_frame("Dim_1" = quali_sup_coord_1,
                              "Dim_2" = quali_sup_coord_2) %>%
  `rownames<-`(quali_sup_labels) %>%
  rownames_to_column(var = "Name") %>%
  filter(grepl("repro_repli.rep|audience", Name)) %>%
  mutate(Name = str_remove(Name, "repro_repli.")) %>%
  mutate(Name = str_remove(Name, "audience."))

df_CA_results_sup_var <- bind_rows(df_CA_quali_sup_var, df_CA_quant_sup_var)


#Plot Fig 2a using ggplot
ggplot(df_CA_results_articles_2, aes(Dim_1,Dim_2)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  geom_point(aes(color = term)) +
  geom_point(data=df_CA_results_sup_var, 
             shape = 25, 
             color="white", 
             fill = "red",
             size = 3,
             aes(x=Dim_1, y=Dim_2))+
  geom_point(data = df_CA_results_nodes, 
             aes(Dim_1, Dim_2, size=Contrib_1_2), 
             shape = 22, 
             fill = "lightgrey") +
  geom_text_repel(data=df_CA_results_sup_var, color="red",
                  aes(label = Name), point.padding = 0.25, box.padding = 0.5)+
  geom_text_repel(data = subset(df_CA_results_nodes, Contrib_1_2 > 4), 
                  aes(label = Node), point.padding = 0.25, box.padding = 0.75)+
  labs(size="Contribution",color="Terms",
       x="Dimension 1: ‘Discipline’ (8.50%)", y="Dimension 2: ‘Audience’ (7.73%)")+
  theme(legend.position = "bottom")
# file saved as SVG with 650x650 pixels
# manual edits in Inkscape:
# - moved labels to avoid overlap (keeping labels in the same quadrant as the corresponding data point)
# - removed period in "Scientific.audience" and "Popular.Audience" labels

#Plot Fig 2b using ggplot
ggplot(df_CA_results_articles_2, aes(Dim_1,Dim_3))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  geom_point(aes(color = clust))+
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1)+
  geom_point(data = df_CA_results_nodes, aes(Dim_1, Dim_3, size=Contrib_1_3), 
             shape = 22, 
             fill = "lightgrey",
             alpha = 0.8)+
  geom_text_repel(data = subset(df_CA_results_nodes, Contrib_1_3 > 1.9), 
                  aes(label = Node), point.padding = 0.25, box.padding = 0.5)+
  labs(size="Contribution", color="Cluster",
       x="Dimension 1: ‘Discipline’ (8.50%)", y="Dimension 3: ‘Perceptions of variation’ (6.95%)")+
  theme(legend.position = "bottom")
# file saved as SVG with 650x650 pixels
# manual edits in Inkscape:
# - moved labels to avoid overlap (keeping labels in the same quadrant as the corresponding data point)

# Figure 3 ----------------------------------------------------------------
#Create data frames for MFA by author results
df_MFA_auth_articles <- as.data.frame(MFA_auth_result$freq$coord) %>%
  rownames_to_column(var = "Name")

df_MFA_auth_within_inertia <- as.data.frame(MFA_auth_result$ind$within.inertia)%>%
  rownames_to_column(var = "Node") %>%
  mutate(Within_inert_1_2 = Dim.1 + Dim.2) %>%
  select(Node, Within_inert_1_2)

df_MFA_auth_nodes <- as.data.frame(MFA_auth_result$ind$coord)%>%
  rownames_to_column(var = "Node")

df_MFA_auth_part_points <- as.data.frame(MFA_auth_result$ind$coord.partiel)%>%
  rownames_to_column(var = "Node") %>%
  mutate(Author = Node) %>%
  mutate(Node = str_remove(Node, "\\..*")) %>% 
  mutate(Author = str_remove(Author, "^.*\\.")) %>%
  select(Node, Author, Dim.1, Dim.2) %>%
  pivot_wider(names_from = Author, values_from = c(Dim.1, Dim.2))

df_MFA_auth_nodes_2 <- inner_join(df_MFA_auth_within_inertia, df_MFA_auth_nodes, by = "Node")

df_MFA_auth_nodes_3 <- inner_join(df_MFA_auth_nodes_2, df_MFA_auth_part_points, by = "Node") %>%
  rename(Dim.1_Mean = Dim.1) %>%
  rename(Dim.2_Mean = Dim.2) %>%
  select(-c(Dim.3, Dim.4, Dim.5)) %>%
  pivot_longer(-c(Node, Within_inert_1_2), 
               names_to = c("Dim", "Point_type"),
               names_pattern = "(.*)_(.*)",
               values_to = "Value") %>%
  pivot_wider(names_from = Dim, values_from = Value)

#Create data frames for MFA by audience results
df_MFA_aud_articles <- as.data.frame(MFA_aud_result$freq$coord) %>%
  rownames_to_column(var = "Name")

df_MFA_aud_within_inertia <- as.data.frame(MFA_aud_result$ind$within.inertia)%>%
  rownames_to_column(var = "Node") %>%
  mutate(Within_inert_1_2 = Dim.1 + Dim.2) %>%
  select(Node, Within_inert_1_2)

df_MFA_aud_nodes <- as.data.frame(MFA_aud_result$ind$coord)%>%
  rownames_to_column(var = "Node")

df_MFA_aud_part_points <- as.data.frame(MFA_aud_result$ind$coord.partiel)%>%
  rownames_to_column(var = "Node") %>%
  mutate(Author = Node) %>%
  mutate(Node = str_remove(Node, "\\..*")) %>% 
  mutate(Author = str_remove(Author, "^.*\\.")) %>%
  select(Node, Author, Dim.1, Dim.2) %>%
  pivot_wider(names_from = Author, values_from = c(Dim.1, Dim.2))

df_MFA_aud_nodes_2 <- inner_join(df_MFA_aud_within_inertia, df_MFA_aud_nodes, by = "Node")

df_MFA_aud_nodes_3 <- inner_join(df_MFA_aud_nodes_2, df_MFA_aud_part_points, by = "Node") %>%
  rename(Dim.1_Mean = Dim.1) %>%
  rename(Dim.2_Mean = Dim.2) %>%
  select(-c(Dim.3, Dim.4, Dim.5)) %>%
  pivot_longer(-c(Node, Within_inert_1_2), 
               names_to = c("Dim", "Point_type"),
               names_pattern = "(.*)_(.*)",
               values_to = "Value") %>%
  pivot_wider(names_from = Dim, values_from = Value)


#Plot Fig 3a using ggplot
ggplot(df_MFA_auth_nodes_3, aes(Dim.1, Dim.2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  geom_point(data=df_MFA_auth_articles, aes(Dim.1, Dim.2), alpha=0.20)+
  geom_point(data=subset(df_MFA_auth_nodes_3, Point_type=="Mean"),
    aes(size=Within_inert_1_2), 
    shape = 22, 
    fill = "lightgrey",
    alpha = 0.8)+
  geom_point(data=subset(df_MFA_auth_nodes_3, Within_inert_1_2 > 4 & Point_type=="Scientist"
                         | Within_inert_1_2 > 4 & Point_type=="Journalist"),
             aes(color=Point_type), size=3, shape=15)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  geom_line(data = subset(df_MFA_auth_nodes_3, Within_inert_1_2 > 4),
            aes(group=Node), linetype=2)+
  geom_text_repel(data = subset(df_MFA_auth_nodes_3, Point_type=="Mean" & Within_inert_1_2 > 4),
                  aes(label=Node), point.padding = 0.25, box.padding = 0.5)+
  labs(size="Within-theme inertia", color="Group",
       x="Dimension 1 (8.72%)", y="Dimension 2 (7.79%)")+
  theme(legend.position = "bottom")
  
#Plot Fig 3b using ggplot
ggplot(df_MFA_aud_nodes_3, aes(Dim.1, Dim.2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_hline(yintercept = 0, linetype=2, color="darkgrey")+
  geom_vline(xintercept = 0, linetype=2, color="darkgrey")+
  geom_point(data=df_MFA_aud_articles, aes(Dim.1, Dim.2), alpha=0.20)+
  geom_point(data=subset(df_MFA_aud_nodes_3, Point_type=="Mean"),
             aes(size=Within_inert_1_2),
             shape = 22, 
             fill = "lightgrey",
             alpha = 0.8)+
  geom_point(data=subset(df_MFA_aud_nodes_3, Within_inert_1_2 > 9 & Point_type=="Scientific"
                         | Within_inert_1_2 > 9 & Point_type=="Popular"),
             aes(color=Point_type), size=3, shape=15)+
  scale_color_viridis(discrete = TRUE, option = "D")+
  geom_line(data = subset(df_MFA_aud_nodes_3, Within_inert_1_2 > 9),
            aes(group=Node), linetype=2)+
  geom_text_repel(data = subset(df_MFA_aud_nodes_3, Point_type=="Mean" & Within_inert_1_2 > 9),
                  aes(label=Node), point.padding = 0.25, box.padding = 0.5)+
  labs(size="Within-theme inertia", color="Group",
       x="Dimension 1 (8.11%)", y="Dimension 2 (6.53%)")+
  theme(legend.position = "bottom")


# Supplementary figures and tables ----------------------------------------
#Supplementary table 1
df_coverage_2 %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(where(is.numeric)))) %>% #new variable for the total amount of text coded in each article
  ungroup() %>% 
  mutate(across(where(is.numeric), ~./total)) %>% #calculate amount of text coded for each theme as a percentage of total text coded
  pivot_longer(-Name, names_to = "code", values_to = "coverage") %>% 
  group_by(code) %>% 
  summarize(Percent_mean_article = mean(coverage)*100) %>% #compute mean for each theme
  inner_join(df_CA_results_nodes, by = c("code" = "Node")) %>%
  select(code, Percent_mean_article, Dim_1, Dim_2, Contrib_1_2, Cos2_1_2) %>% #Join to coordinates, contribution, and Cos2 info from the CA
  arrange(desc(Percent_mean_article)) %>% 
  kable(format = "latex", 
        col.names = c("Theme", "% mean article", "Dim. 1", "Dim. 2", "Contribution", "cos^2"),
        booktabs = T, 
        longtable = T,
        digits = 2) #output as LaTeX table code

#Supplementary table 2
df_IRR %>%
  filter(!grepl("Overall", Name)) %>% 
  arrange(desc(Ave_Kappa)) %>% 
  kable(format = "latex", 
        col.names = c("Theme", "JC--NCN", "KI--JC", "KI--NCN", "Average Kappa"),
        booktabs = T, 
        longtable = T,
        digits = 2)
  
#Bonus visualization: plot the histogram of this table so you can see the distribution of average Kappa scores  
#not included in paper
df_IRR %>%
  filter(!grepl("Overall", Name)) %>%
  ggplot(aes(x=Ave_Kappa))+
    geom_histogram(binwidth = 0.05)





#Bonus figure 
#not included in paper
fviz_screeplot(CA_result)+
  geom_hline(yintercept=(1/(29-1)*100),linetype=2, color="red") +
  theme_minimal()


#Bonus figure 
#not included in paper
plot(HCPC_result, choice="bar")

  
