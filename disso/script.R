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
library(ROCR)
library(pROC)

# load dataset as 'Data'
Data = read.csv("Invistico_Airline.csv", stringsAsFactors = FALSE)

# basic observations
head(Data)
summary(Data)
dim(Data)

# missing values per column check (~300 rows are missing value in arrivalDelay)
cbind(
  lapply(
    lapply(Data, is.na)
    , sum)
)

# remove observations with missing values (small number, appropriate)
Data <- na.omit(Data)

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

# basic plots of each feature
plot_list = list()
for(i in 1:ncol(Data)){
  var = names(Data)[i]
  if(is.numeric(Data[, i]) == TRUE){
    # plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_boxplot() + labs(title = var)
    plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_histogram(bins = 5) + labs(title = var) + coord_flip()
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

# count plots by satisfaction
# plot of age (count)
age_count <- ggplot(Data, aes(x = Data[, 4])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal()
age_count

# plot of age GROUPS (count)(by satisfaction)
# grouping ages by tens 
ageplotdata <- Data
for (i in 1:nrow(ageplotdata)){
  if (ageplotdata[i, 4] >= 0 && ageplotdata[i, 4] <= 10){
    ageplotdata[i, 4] <- "0-10"
  }
  if (ageplotdata[i, 4] >= 11 && ageplotdata[i, 4] <= 20){
    ageplotdata[i, 4] <- "11-20"
  }
  if (ageplotdata[i, 4] >= 21 && ageplotdata[i, 4] <= 30){
    ageplotdata[i, 4] <- "21-30"
  }
  if (ageplotdata[i, 4] >= 31 && ageplotdata[i, 4] <= 40){
    ageplotdata[i, 4] <- "31-40"
  }
  if (ageplotdata[i, 4] >= 41 && ageplotdata[i, 4] <= 50){
    ageplotdata[i, 4] <- "41-50"
  }
  if (ageplotdata[i, 4] >= 51 && ageplotdata[i, 4] <= 60){
    ageplotdata[i, 4] <- "51-60"
  }
  if (ageplotdata[i, 4] >= 61){
    ageplotdata[i, 4] <- "61+"
  }
}
agegroup_count[, 4] <- as.factor(agegroup_count[, 4])

# plot of age groups (count)(by satisfaction)
agegroup_count <- ggplot(ageplotdata, aes(x = ageplotdata[, 4], fill = ageplotdata[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal()
agegroup_count

# plot of gender (count)(by satisfaction)
gender_count <- ggplot(Data, aes(x = Data[, 2], fill = Data[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal()
gender_count

# plot of customer loyalty (count)(by satisfaction)
loyalty_count <- ggplot(Data, aes(x = Data[, 3], fill = Data[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal()
loyalty_count

# plot of customer travel type(count)(by satisfaction)
type_count <- ggplot(Data, aes(x = Data[, 5], fill = Data[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal()
type_count

# plot of customer seat class (count)(by satisfaction)
class_count <- ggplot(Data, aes(x = Data[, 6], fill = Data[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal()
class_count

# separate satisfaction, 
sat_Data <- Data %>% filter(satisfaction == 'satisfied')
summary(sat_Data)

# SATISFIED plots
plot_list = list()
for(i in 2:ncol(sat_Data)){
  var = names(sat_Data)[i]
  if(is.numeric(sat_Data[, i]) == TRUE){
    # plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_boxplot() + labs(title = var)
    plot_list[[i]] = ggplot(sat_Data, aes(, sat_Data[, i])) + 
      geom_histogram(bins = 5) + 
      labs(title = var) + 
      coord_flip() + 
      theme_minimal()
    print(plot_list[[i]])
  }
  else if(is.factor(sat_Data[, i]) == TRUE){
    plot_list[[i]] = ggplot(sat_Data, aes(, sat_Data[, i])) + 
      geom_bar(width = 0.5) + 
      labs(title = var) + 
      coord_flip() + 
      theme_minimal()
    print(plot_list[[i]])
  }
  else{
    cat(sprintf("feature %s is not graphable\n", i))
  }
}

dissat_Data <- Data %>% filter(satisfaction == 'dissatisfied')
summary(dissat_Data)

# DISSATISFIED plots
plot_list = list()
for(i in 2:ncol(dissat_Data)){
  var = names(dissat_Data)[i]
  if(is.numeric(dissat_Data[, i]) == TRUE){
    # plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + geom_boxplot() + labs(title = var)
    plot_list[[i]] = ggplot(dissat_Data, aes(, dissat_Data[, i])) + 
      geom_histogram(bins = 5) + 
      labs(title = var) + 
      coord_flip() + 
      theme_minimal()
    print(plot_list[[i]])
  }
  else if(is.factor(dissat_Data[, i]) == TRUE){
    plot_list[[i]] = ggplot(dissat_Data, aes(, sat_Data[, i])) + 
      geom_bar(width = 0.5) + 
      labs(title = var) + 
      coord_flip() + 
      theme_minimal()
    print(plot_list[[i]])
  }
  else{
    cat(sprintf("feature %s is not graphable\n", i))
  }
}

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
kmm = kmeans(data_att, 4, nstart = 50, iter.max = 30)
kmm

# means per cluster
aggregate(Data, by=list(cluster=kmm$cluster), mean)

# adding cluster assignments to dataset
Data_clustered <- cbind(Data, cluster = kmm$cluster)

################################################################################

# boruta working
boruta_output <- Boruta(Data$satisfaction ~ ., data=Data[, 2:ncol(Data)], doTrace=0)
names(boruta_output)
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

# rpart working
set.seed(123)
rPartMod <- train(satisfaction ~ ., data = Data, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

# partition percentage for loop
training_data_percentages <- seq(from = 0.1, to = 0.9, length.out = 9)

# decision tree classification (looping through 9 possible splits)
cat("Decision Tree implementation")
for(t in training_data_percentages){
  print("================================================================================================================")
  cat(sprintf("Current train-test split: %s-%1.0f\n", t*100, (1-t)*100))
  
  # partition setup
  indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
  training_data = Data[indx_partition,]
  testing_data = Data[-indx_partition,]
  
  # set seed for reproducability, train and receive predictions
  set.seed(42)
  TrainedClassifier = C5.0(x = training_data[, 2:ncol(testing_data)], y = training_data[, 1])
  Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[, 2:ncol(testing_data)], type = "prob")[, 2]
  
  # calculate AUC from decision confidence, draw ROC curve
  roc_score = roc(testing_data$satisfaction, Predicted_outcomes) #AUC score
  plot(roc_score, main = paste("ROC Curve for Decision Tree, Split Ratio", t*100, "-", (1-t)*100))
  
  # convert decision confidence to factor, produce confusion matrix
  Predicted_outcomes <- as.factor(ifelse(Predicted_outcomes > 0.5, "satisfied", "dissatisfied"))
  cm <- confusionMatrix(testing_data[, 1], Predicted_outcomes)
  print(cm)
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
  Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[, 2:ncol(testing_data)], type = "raw")[, 2]
  
  roc_score = roc(testing_data$satisfaction, Predicted_outcomes) #AUC score
  plot(roc_score, main = paste("ROC Curve for Decision Tree, Split Ratio", t*100, "-", (1-t)*100))
  
  Predicted_outcomes <- as.factor(ifelse(Predicted_outcomes > 0.5, "satisfied", "dissatisfied"))

  cm <- confusionMatrix(testing_data[, 1], Predicted_outcomes)
  print(cm)
}

##############################################################################
points <- data.frame(points)

for(i in 1:nrow(Data)){
  if(Data[i, 1] == 'satisfied'){
    for(j in 8:21){
      if(Data[i, (j-7)] == 4){
        points[i, (j-7)] = 1 + points[i, (j-7)]
      }
      if(Data[i, (j-7)] == 5){
        points[i, (j-7)] = 2 + points[i, (j-7)]
      }
      if(Data[i, (j-7)] == 2){
        points[i, (j-7)] = 0.5 + points[i, (j-7)]
      }
      if(Data[i, (j-7)] == 1){
        points[i, (j-7)] = 1 + points[i, (j-7)]
      }
    }
  } else {
    for(j in 8:21){
      if(Data[i, (j-7)] == 4){
        points[i, (j-7)] = 0.5 + points[i, (j-7)]
      }
      if(Data[i, (j-7)] == 5){
        points[i, (j-7)] = 1 + points[i, (j-7)]
      }
      if(Data[i, (j-7)] == 2){
        points[i, (j-7)] = 1 + points[i, (j-7)]
      }
      if(Data[i, (j-7)] == 1){
        points[i, (j-7)] = 2 + points[i, (j-7)]
      }
    }
  }
}
