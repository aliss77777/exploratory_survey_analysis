#Anisha test code
# 0. Import files  -------------------------------------------------
setwd("/Users/alexander.liss/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/exploratory_survey_analysis/")

library(tidyverse)
library(readxl)
# library(scales)

mydata <- read.csv("raw data.csv")


# 1. Data Cleaning --------------------------------------------------------

# create a subset just containing questions 5 and 7, with open-ends (text responses) removed
data_for_clustering <- mydata %>% select(contains("uuid"), 
                                              starts_with("Q5"), -contains("Q5r5"), -contains("Q5r6"), 
                                              starts_with("Q7"), -contains("Q7r8"))


# need to remove all 99's b/c that's NA; as well as filter out missing data
data_for_clustering_slim <- data_for_clustering
data_for_clustering_slim[data_for_clustering_slim == "99"] <- NA

# this removes rows with blank data for Q5 and Q7; as a result not all rows in the original data file will be
# included in the clustering algorithm
data_for_clustering_slim <- data_for_clustering_slim[complete.cases(data_for_clustering_slim),]

#need to remove the UUIDS for the factor analysis and kmeans algorithm
uuid_lookup <- data.frame(data_for_clustering_slim[,1]) # we'll use this later
data_for_clustering_slim <- data_for_clustering_slim[,-1]

set.seed(42) # creates same random values for reproducibility


# 2. Factor Analysis ------------------------------------------------------

# diagnostic plot to determine # of factors
library(nFactors) #this masks select from dplyr so only attach and detach when necessary
ev <- eigen(cor(data_for_clustering_slim)) # get eigenvalues
ap <- parallel(subject=nrow(data_for_clustering_slim),var=ncol(data_for_clustering_slim),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

library(psych)
fit <- principal(data_for_clustering_slim, nfactors=4, rotate="varimax") #choose # of factors based on results of diagnostic plot
fit # print results

#visualize the factor & features
library(semPlot)
semPaths(fit, what = "est", residuals = FALSE,
         cut=0.4, posCol=c("white", "darkblue"), negCol=c("white","red"),
         edge.label.cex = 0.75, nCharNodes = 10, sizeMan = 8)
# it also helps to save this to working directory using Rstudio

write.csv(fit$loadings, "factor loadings_example.csv") #export to excel and analyze, interpret, and name the factors

named_factor_scores <- fit$scores # create a table of the factor loadings for each respondent

names <- c("Reluctance", #naming the factors after reviewing output in excel: 
           "Affinity",
           "Consistency",
           "Confidence")

colnames(named_factor_scores) <- names #renaming the factor columns 

#merging the factor loadings into the original dataset
# later we will add cluster assignments to use and use for visualization
data_with_factors <- cbind(data_for_clustering_slim, named_factor_scores) 

# 3. Perform Clustering & Diagnostics -------------------------------------

# Create the function to plot the sum of squares (DO NOT EDIT THIS)
wssplot <- function(data, nc, seed){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 1:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, iter.max = 50, nstart = 15)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

# plot sum of squares of 1 group to N groups. Change nc=X  to however many clusters you want to see
wssplot(data_for_clustering_slim, nc=10, seed=42)

# where the elbow drops should be the cut-off for where to STOP making clusters

#*** Method 2: Statistical Criteria  ******
# Use "NbClust" Package to determine number of clusters to use
library(NbClust)
cluster.Criteria.Results <- NbClust(data_for_clustering_slim, min.nc=2, max.nc=10, method="kmeans")

barplot(table(cluster.Criteria.Results$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# this says 2 clusters is ideal, but i chose 3 for the storytelling angle. 
# kmeans is a mathematical algorithm, so the results can be interpreted by the user and
# shaped  by your discretion to achieve optimal results, appropriate to context

Optimal.Number.Of.Clusters <- 3 #choose this number based on the diagnostics above
k.means.fit <- kmeans(x = data_for_clustering_slim, centers = Optimal.Number.Of.Clusters, iter.max = 50, nstart = 15)

clusters <- k.means.fit$cluster

data_with_factors %>% add_column(clusters) -> data_with_factors_and_clusters

# 3. visualizing the results  see if this segment solution works -------------------

# code clusters as a categorical variable so the charts look right
data_with_factors_and_clusters$clusters <- as.factor(data_with_factors_and_clusters$clusters)


# looking at n per each cluster
n_by_segment <- prop.table(table(data_with_factors_and_clusters$clusters))
barplot(n_by_segment, col = rainbow(3), main = "Percent of N by Segment")

# factor results comparison bar chart
# Barriers to Going to the Dentist by segment
data_with_factors_and_clusters %>% 
  group_by(clusters) %>%
  dplyr::select(18:21) %>% # selecting the factor analysis columns. adding dplyr:: b/c other libraries masked 'select'
  summarise_all(funs(mean(., na.rm = TRUE))) %>% gather(Question, Value, 2:ncol(.)) %>%
  filter(!is.na(clusters)) %>%
  ggplot(aes(fill = clusters, y = Value, x = Question)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Factor Loadings by Segment")

#q5 comparison bar chart
#comparing Attitudes About Going to the Dentist by segment
data_with_factors_and_clusters %>% 
  group_by(clusters) %>%
  dplyr::select(starts_with("Q5")) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% gather(Question, Value, 2:ncol(.)) %>%
  filter(!is.na(clusters)) %>%
  ggplot(aes(fill = clusters, y = Value, x = Question)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Attitudes About Going to the Dentist by segment")

# q7 comparison bar chart
# Barriers to Going to the Dentist by segment
data_with_factors_and_clusters %>% 
  group_by(clusters) %>%
  dplyr::select(starts_with("Q7")) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% gather(Question, Value, 2:ncol(.)) %>%
  filter(!is.na(clusters)) %>%
  ggplot(aes(fill = clusters, y = Value, x = Question)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Barriers to Going to the Dentist by segment")




# 4. Optional: Writing segments to a single data frame for export ---------

# putting UUID as the first column so the results can be merged back into the bigger data file
data_with_hgroup <- data_with_factors_and_clusters 
data_with_hgroup[23] <- uuid_lookup[1]
colnames(data_with_hgroup)[23] <- 'uuid'

# this assigned rows with clusters two variables to be joined back into the larger data file
data_with_hgroup <- merge(data_with_hgroup, mydata[,c("uuid","hGroup")], by = "uuid",
                                              all.x = TRUE)


## appending segments and factors to full data file
file_to_join <- data_with_hgroup %>% dplyr::select(-starts_with("Q5"), -starts_with("Q7"), -starts_with("hGroup"))
merged_data <- left_join(mydata, file_to_join, by = "uuid")

WriteXLS::WriteXLS(merged_data,"full data with segments.xlsx",row.names = FALSE)

