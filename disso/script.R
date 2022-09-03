library(ggplot2)
library(tidyverse)
library(flexclust)
library(janitor)
library(cowplot)

# load dataset as 'Data'
Data = read.csv("Invistico_Airline.csv", stringsAsFactors = FALSE)

# basic observations
head(Data)
summary(Data)
dim(Data)

# inspect column types
for(i in 1:(ncol(Data))) {
  if(is.numeric(Data[, i]) == TRUE){
    cat(sprintf("column %s is numeric\n", i))
  }
  else if(is.factor(Data[, i]) == TRUE){
    cat(sprintf("column %s is factor\n", i))
  }
  else{
    cat(sprintf("column %s is char\n", i))
  }
}

# convert char columns to factor
for(i in 1:ncol(Data)){
  if(is.character(Data[, i]) == TRUE){
    Data[, i] <- as.factor(Data[, i])
    if(is.factor(Data[, i]) == TRUE){
      cat(sprintf("feature %s has been converted into factor\n", i))
      cat(sprintf("with levels: %s\n", levels(Data[, i])))
    }
  }
}

# basic plots of each feature
plot_list = list()
for(i in 1:ncol(Data)){
  var = names(Data)[i]
  if(is.numeric(Data[, i]) == TRUE){
    plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_boxplot() + labs(title = var)
    # plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_histogram(bins = 10) + labs(title = var) + coord_flip()
    print(plot_list[[i]])
  }
  else if(is.factor(Data[, i]) == TRUE){
    plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_bar() + labs(title = var) + coord_flip()
    print(plot_list[[i]])
  }
  else{
    cat(sprintf("feature %s is not graphable\n", i))
  }
}

# select first 10% of records, any bigger than ~20k will be taxing on hardware, only selects attitudinal variables
data_att <- Data[1:12988, 8:21]

# normalise values
data_att_norm <- as.data.frame(scale(data_att, center=TRUE, scale=TRUE))

# basic summary
summary(data_att_norm)

# set seed
set.seed(123)

# calculate distances for clustering
dist <- dist(data_att_norm, method = 'euclidean')

# initiate clustering
hier_clust <- hclust(dist, method = "ward.D2") 
plot(hier_clust)

# display 6 cluster solution on plot
rect.hclust(hier_clust, k = 6, border = "red")

# create 6 cluster solution
hcluster_groups <- cutree(hier_clust, k = 6)

# table of member numbers
table(hcluster_groups)

# add cluster assignment to dataframe
data_att_norm_p1t5 <- data_att_norm %>% 
  mutate(hcluster_groups = hcluster_groups)

# statistics for each cluster
data_att_norm_p1t5 %>%
  group_by(hcluster_groups) %>% # group by cluster
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  print(width = Inf) # prints all variables (all columns)

# flexclust profiles 
hier_clust_flex <- as.kcca(hier_clust, data_att_norm, k = 6)

table(hcluster_groups, clusters(hier_clust_flex))

barchart(hier_clust_flex, main = "Segment Profiles")


