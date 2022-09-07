# libraries
library(ggplot2)
library(tidyverse)
library(flexclust)
library(janitor)
library(cowplot)
library(e1071)
library(caret)
library(mclust)
library(NbClust)
library(C50)
library(Boruta)
library(corrplot)
library(ggcorrplot)

# load dataset as 'Data'
Data = read.csv("Invistico_Airline.csv", stringsAsFactors = FALSE)

# basic observations
head(Data)
summary(Data)
dim(Data)

# missing values per column check
cbind(
  lapply(
    lapply(Data, is.na)
    , sum)
)

# inspect column types - char columns exist
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

# # replace missing values with mean of column IF they are numerical
# for(i in 1:ncol(Data)){
#   if(is.numeric(Data[, i]) == TRUE){
#     for(n in 1:nrow(Data)){
#       if(is.na(Data[n, i]) == TRUE){
#         cat(sprintf("row: %s column: %f changed from NA\n", n, i))
#         Data[n, i] <- mean(Data[, i], na.rm = TRUE)
#       }
#     }
#   }
#   else{
#     cat(sprintf("%s is non-numeric\n", i))
#   }
# }

# remove observations with missing values (small number)
Data <- na.omit(Data)

# # basic plots of each feature
# plot_list = list()
# for(i in 1:ncol(Data)){
#   var = names(Data)[i]
#   if(is.numeric(Data[, i]) == TRUE){
#     plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_boxplot() + labs(title = var)
#     # plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_histogram(bins = 10) + labs(title = var) + coord_flip()
#     print(plot_list[[i]])
#   }
#   else if(is.factor(Data[, i]) == TRUE){
#     plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_bar() + labs(title = var) + coord_flip()
#     print(plot_list[[i]])
#   }
#   else{
#     cat(sprintf("feature %s is not graphable\n", i))
#   }
# }

# print correlation matrix
model.matrix(~0+., Data) %>%
  cor() %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

################################################################################

# # select first 10% of records, any bigger than ~20k will be taxing on hardware, only selects attitudinal variables
# data_att <- Data[1:12988, 8:21]
# 
# # normalise values
# data_att_norm <- as.data.frame(scale(data_att, center=TRUE, scale=TRUE))
# 
# # basic summary
# summary(data_att_norm)
# 
# # set seed
# set.seed(123)
# 
# # calculate distances for clustering
# dist <- dist(data_att_norm, method = 'euclidean')
# 
# # initiate clustering
# hier_clust <- hclust(dist, method = "ward.D2") 
# plot(hier_clust)
# 
# # display 6 cluster solution on plot
# rect.hclust(hier_clust, k = 10, border = "red")
# 
# # create 6 cluster solution
# hcluster_groups <- cutree(hier_clust, k = 10)
# 
# # table of member numbers
# table(hcluster_groups)
# 
# # add cluster assignment to dataframe
# data_att_norm_p1t5 <- data_att_norm %>% 
#   mutate(hcluster_groups = hcluster_groups)
# 
# # statistics for each cluster
# data_att_norm_p1t5 %>%
#   group_by(hcluster_groups) %>% # group by cluster
#   summarise_all(~ mean(.x)) %>% # calculate the mean per group 
#   print(width = Inf) # prints all variables (all columns)
# 
# # flexclust profiles 
# hier_clust_flex <- as.kcca(hier_clust, 
#                            data_att_norm, 
#                            k = 10)
# 
# table(hcluster_groups, clusters(hier_clust_flex))
# 
# barchart(hier_clust_flex, main = "Segment Profiles")

#####

# redefine data_att
data_att <- Data[, 8:21]
data_att_norm <- as.data.frame(scale(data_att, center=TRUE, scale=TRUE))
data_att_norm_p1t12 <- data_att_norm

# scale and center data
scaled_data = as.matrix(scale(data_att))

# initial clustering
kmm = kmeans(scaled_data, 3, nstart = 50, iter.max = 15)
kmm

# set seed
set.seed(123)

k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(data_att_norm_p1t12, k, nstart=50,iter.max = 15)$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# bayesian inference criterion
d_clust <- Mclust(as.matrix(scaled_data), G=1:15, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

# clustering
kmm = kmeans(data_att, 4, nstart = 50, iter.max = 15)
kmm

# # doesn't work due to euclidean distance (too comp expensive)
# nb <- NbClust(scaled_data, diss=NULL, distance = "euclidean", 
#               min.nc=2, max.nc=5, method = "kmeans", 
#               index = "all", alphaBeale = 0.1)
# hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

# # k means clustering
# kmeans_clust <- kmeans(data_att_norm_p1t12, 
#                        centers = 4, 
#                        iter.max = 1000,
#                        nstart = 100)
# 
# table(kmeans_clust$cluster)

# # add k means clustering groups to normalised dataset
# data_att_norm_p1t13 <- data_att_norm %>% 
#   mutate(kcluster_groups = kmeans_clust$cluster)

################################################################################

# boruta test
boruta_output <- Boruta(Data$satisfaction ~ ., data=Data[, 2:ncol(Data)], doTrace=0)
names(boruta_output)
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

# rpart test
set.seed(123)
rPartMod <- train(satisfaction ~ ., data = Data, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

# partition percentage for loop
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)

# decision tree classification
cat("Decision Tree implementation")
for(t in training_data_percentages){
  print("================================================================================================================")
  cat(sprintf("Current train-test split: %s-%1.0f\n", t*100, (1-t)*100))
  
  indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
  training_data = Data[indx_partition,]
  testing_data = Data[-indx_partition,]

  set.seed(42)
  TrainedClassifier = C5.0(x = training_data[, 2:ncol(testing_data)], y = training_data[, 1])
  Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[, 2:ncol(testing_data)])

  cm <- confusionMatrix(testing_data[, 1], Predicted_outcomes)
  print(cm)
  print("END OF RUN")
}

# nb classification
cat("Naive Bayes implementation")
for(t in training_data_percentages){
  print("================================================================================================================")
  cat(sprintf("Current train-test split: %s-%1.0f\n", t*100, (1-t)*100))

  indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
  training_data = Data[indx_partition,]
  testing_data = Data[-indx_partition,]

  set.seed(42)
  TrainedClassifier = naiveBayes(satisfaction ~ ., data = training_data, laplace=0)
  Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[,2:ncol(testing_data)])

  cm <- confusionMatrix(testing_data[, 1], Predicted_outcomes)
  print(cm)
  print("END OF RUN")
}

