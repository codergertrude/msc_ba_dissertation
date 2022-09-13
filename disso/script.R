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
library(klaR)
library(C50)
library(Boruta)
library(corrplot)
library(ggcorrplot)
library(ROCR)
library(pROC)
library(ggpubr)
library(ztable)

# load dataset as 'Data'
Data = read.csv("Invistico_Airline.csv", stringsAsFactors = FALSE)

# basic observations
head(Data)
summary(Data)
dim(Data)

# missing values per column check (~300 rows are missing value in arrivalDelay)
cbind(
  lapply(
    lapply(check, is.na)
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
    plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + 
      geom_histogram(bins = 5) + 
      labs(title = var) + 
      coord_flip() + 
      ylab(var) + 
      theme_minimal()
    print(plot_list[[i]])
  }
  else if(is.factor(Data[, i]) == TRUE){
    plot_list[[i]] = ggplot(Data, aes(, Data[, i])) + 
      geom_bar() + 
      labs(title = var) + 
      coord_flip() + 
      ylab(var) +
      theme_minimal()
    print(plot_list[[i]])
  }
  else{
    cat(sprintf("feature %s is not graphable\n", i))
  }
}

# separate satisfaction and dissatisfaction
sat_Data <- Data %>% filter(satisfaction == 'satisfied')
summary(sat_Data)

dissat_Data <- Data %>% filter(satisfaction == 'dissatisfied')
summary(dissat_Data)

# count plots by satisfaction
# plot of age (count)
age_count <- ggplot(Data, aes(x = Data[, 4])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() + 
  labs(title = "Distribution of Age") + 
  xlab("Ages (years)")
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
ageplotdata[, 4] <- as.factor(ageplotdata[, 4])

# plot of age groups (count)(by satisfaction)
agegroup_count <- ggplot(ageplotdata, aes(x = ageplotdata[, 4], fill = ageplotdata[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of age groups, by satisfaction") + 
  xlab("Age groups (years)") + 
  theme(legend.title=element_blank())
agegroup_count

# PLOTS OF CATEGORICAL/BINARY FACTOR VARIABLES
# plot of gender (count)(by satisfaction)
gender_count <- ggplot(Data, aes(x = Data[, 2], fill = Data[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of gender, by satisfaction") + 
  xlab("Genders") + 
  theme(legend.title=element_blank())
gender_count

Data %>%
  group_by(Gender) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Gender) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of customer loyalty (count)(by satisfaction)
loyalty_count <- ggplot(Data, aes(x = Data[, 3], fill = Data[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of loyalty status, by satisfaction") + 
  xlab("Loyalty status") + 
  theme(legend.title=element_blank())
loyalty_count

Data %>%
  group_by(Customer.Type) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Customer.Type) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of customer travel type(count)(by satisfaction)
type_count <- ggplot(Data, aes(x = Data[, 5], fill = Data[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of travel intent, by satisfaction") + 
  xlab("Travel intent") + 
  theme(legend.title=element_blank())
type_count

Data %>%
  group_by(Type.of.Travel) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Type.of.Travel) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of customer seat class (count)(by satisfaction)
class_count <- ggplot(Data, aes(x = Data[, 6], fill = Data[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of seat class, by satisfaction") + 
  xlab("Seat classes") + 
  theme(legend.title=element_blank())
class_count

Data %>%
  group_by(Class) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Class) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# initialize dataset with ALL factor variables (for more graphs)
factorData <- ageplotdata[1:21]

# convert flight distance to factor
for (i in 1:nrow(factorData)){
  if (factorData[i, 7] >= 0 && factorData[i, 7] <= 1000){
    factorData[i, 7] <- "0-1000"
  }
  if (factorData[i, 7] >= 1001 && factorData[i, 7] <= 2000){
    factorData[i, 7] <- "1001-2000"
  }
  if (factorData[i, 7] >= 2001 && factorData[i, 7] <= 3000){
    factorData[i, 7] <- "2001-3000"
  }
  if (factorData[i, 7] >= 3001 && factorData[i, 7] <= 4000){
    factorData[i, 7] <- "3001-4000"
  }
  if (factorData[i, 7] >= 4001){
    factorData[i, 7] <- "4001+"
  }
}
factorData[, 7] <- as.factor(factorData[, 7])

# convert Likert scales to factor 
for(i in 1:ncol(factorData)){
  if(is.numeric(factorData[, i]) == TRUE){
    factorData[, i] <- as.factor(factorData[, i])
    if(is.factor(factorData[, i]) == TRUE){
      cat(sprintf("feature %s has been converted into factor\n", i))
      cat(sprintf("with levels: %s\n", levels(Data[, i])))
    }
  }
}

# plot of flight distance (count)(by satisfaction)
flightdist_count <- ggplot(factorData, aes(x = factorData[, 7], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of flight distance, by satisfaction") + 
  xlab("Flight distance ranges (kms)") + 
  theme(legend.title=element_blank())
flightdist_count

# plot of seat comfort (count)(by satisfaction)
seatcomfort_count <- ggplot(factorData, aes(x = factorData[, 8], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of seat comfort, by satisfaction") + 
  xlab("Seat comfort score") + 
  theme(legend.title=element_blank())
seatcomfort_count

Data %>%
  group_by(Seat.comfort) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Class) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of dep and arr time convenience (count)(by satisfaction)
deparrtimeconv_count <- ggplot(factorData, aes(x = factorData[, 9], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of time convenience, by satisfaction") + 
  xlab("Time convenience score") + 
  theme(legend.title=element_blank())
deparrtimeconv_count

Data %>%
  group_by(Departure.Arrival.time.convenient) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Departure.Arrival.time.convenient) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of food and drink (count)(by satisfaction)
foodanddrink_count <- ggplot(factorData, aes(x = factorData[, 10], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of food and drink, by satisfaction") + 
  xlab("Food and drink score") + 
  theme(legend.title=element_blank())
foodanddrink_count

Data %>%
  group_by(Food.and.drink) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Food.and.drink) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of gate location (count)(by satisfaction)
gatelocation_count <- ggplot(factorData, aes(x = factorData[, 11], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of gate location, by satisfaction") + 
  xlab("Gate location score") + 
  theme(legend.title=element_blank())
gatelocation_count

Data %>%
  group_by(Gate.location) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Gate.location) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of inflight wifi (count)(by satisfaction)
inflightwifi_count <- ggplot(factorData, aes(x = factorData[, 12], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of in-flight wi-fi, by satisfaction") + 
  xlab("In-flight wi-fi score") + 
  theme(legend.title=element_blank())
inflightwifi_count

Data %>%
  group_by(Inflight.wifi.service) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Inflight.wifi.service) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of inflight entertainment (count)(by satisfaction)
inflightent_count <- ggplot(factorData, aes(x = factorData[, 13], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of in-flight entertainment, by satisfaction") + 
  xlab("In-flight entertainment score") + 
  theme(legend.title=element_blank())
inflightent_count

Data %>%
  group_by(Inflight.entertainment) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Inflight.entertainment) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of online support (count)(by satisfaction)
onlinesup_count <- ggplot(factorData, aes(x = factorData[, 14], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of online support, by satisfaction") + 
  xlab("Online support score") + 
  theme(legend.title=element_blank())
onlinesup_count

Data %>%
  group_by(Online.support) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Online.support) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of ease of online booking (count)(by satisfaction)
easeofbooking_count <- ggplot(factorData, aes(x = factorData[, 15], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of ease of online booking, by satisfaction") + 
  xlab("Ease of online booking score") + 
  theme(legend.title=element_blank())
easeofbooking_count

Data %>%
  group_by(Ease.of.Online.booking) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Ease.of.Online.booking) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of on-board service (count)(by satisfaction)
onboardserv_count <- ggplot(factorData, aes(x = factorData[, 16], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of on-board service, by satisfaction") + 
  xlab("On-board service score") + 
  theme(legend.title=element_blank())
onboardserv_count

Data %>%
  group_by(Ease.of.Online.booking) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Ease.of.Online.booking) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of leg room (count)(by satisfaction)
legroom_count <- ggplot(factorData, aes(x = factorData[, 17], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of leg room, by satisfaction") + 
  xlab("Leg room score") + 
  theme(legend.title=element_blank())
legroom_count

Data %>%
  group_by(Leg.room.service) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Leg.room.service) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of baggage handling (count)(by satisfaction)
baghandle_count <- ggplot(factorData, aes(x = factorData[, 18], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of baggage handling, by satisfaction") + 
  xlab("Baggage handling score") + 
  theme(legend.title=element_blank())
baghandle_count

Data %>%
  group_by(Baggage.handling) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Baggage.handling) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))


# plot of check-in service (count)(by satisfaction)
checkin_count <- ggplot(factorData, aes(x = factorData[, 19], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of check-in service, by satisfaction") + 
  xlab("Check-in service score") + 
  theme(legend.title=element_blank())
checkin_count

Data %>%
  group_by(Checkin.service) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Checkin.service) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of cleanliness (count)(by satisfaction)
cleanliness_count <- ggplot(factorData, aes(x = factorData[, 20], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of cleanliness, by satisfaction") + 
  xlab("Cleanliness score") + 
  theme(legend.title=element_blank())
cleanliness_count

Data %>%
  group_by(Cleanliness) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Cleanliness) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# plot of online boarding (count)(by satisfaction)
onlineboard_count <- ggplot(factorData, aes(x = factorData[, 21], fill = factorData[, 1])) +
  geom_bar(width=0.5, position = position_dodge()) + 
  theme_minimal() +
  labs(title = "Distribution of online boarding, by satisfaction") + 
  xlab("Online boarding score") + 
  theme(legend.title=element_blank())
onlineboard_count

Data %>%
  group_by(Online.boarding) %>%
  summarise(percent = 100 * n() / nrow(Data))

sat_Data %>%
  group_by(Online.boarding) %>%
  summarise(percent = 100 * n() / nrow(sat_Data))

# print correlation matrix
model.matrix(~0+., Data) %>%
  cor() %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3)

################################## CLUSTERING ##############################

# redefine data_att (must be as such, can only be used on cont variables)
clustData <- Data[, 2:23]
clustData_OH <- model.matrix(~0+., data=clustData)[,2:23]
clustData_OH_scaled <- as.data.frame(scale(clustData_OH, center=TRUE, scale=TRUE))
scaled_data <- clustData_OH_scaled

# # scale and center data
# scaled_data = as.matrix(scale(data_att))

# set seed
set.seed(123)

# elbow method
k.max <- 8
wss <- sapply(1:k.max, 
              function(k){kmeans(scaled_data, k, nstart=50,iter.max = 50)$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters",
     ylab="Sum of squares")
abline(v = 2, col="black", lwd=3, lty=2)

# bayesian inference criterion
d_clust <- Mclust(as.matrix(scaled_data), G=1:8, 
                  modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

# clustering
kmm = kmeans(scaled_data, 4, nstart = 30, iter.max = 55)
kmm

# means per cluster
aggregate(clustData_OH, by=list(cluster=kmm$cluster), mean)

# add cluster results here!!!
clustRes <- Data[, 1:23]
clustRes_OH <- model.matrix(~0+., data=clustRes)[,2:24]

# means per cluster
clustMeansDF <- aggregate(clustRes_OH, by=list(cluster=kmm$cluster), mean)

# plot of cluster satisfaction
clust1 <- ggplot(clustMeansDF, aes(x = cluster, y = satisfactionsatisfied, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Cluster means for satisfaction") + 
  xlab("Clusters") +
  ylab("Satisfaction %") + 
  theme(legend.title=element_blank())

clust2 <- ggplot(clustMeansDF, aes(x = cluster, y = GenderMale, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) +  theme_minimal() +
  labs(title = "Male customers") + 
  xlab("Clusters") + 
  ylab("Male %") + 
  theme(legend.title=element_blank())

clust3 <- ggplot(clustMeansDF, aes(x = cluster, y = clustMeansDF[, 4], fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Loyal customers") + 
  xlab("Clusters") +
  ylab("Loyal %") +
  theme(legend.title=element_blank())

clust4 <- ggplot(clustMeansDF, aes(x = cluster, y = Age, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Customer age") + 
  xlab("Clusters") +
  ylab("Age") +
  theme(legend.title=element_blank())

clust5 <- ggplot(clustMeansDF, aes(x = cluster, y = clustMeansDF[, 6], fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Personal travel customers") + 
  xlab("Clusters") + 
  ylab("Personal travel %") +
  theme(legend.title=element_blank())

clust6 <- ggplot(clustMeansDF, aes(x = cluster, y = ClassEco, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Economy class customers") + 
  xlab("Clusters") +
  ylab("Economy class %") +
  theme(legend.title=element_blank())

clust7 <- ggplot(clustMeansDF, aes(x = cluster, y = clustMeansDF[, 8], fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Economy plus class customers") + 
  xlab("Clusters") + 
  ylab("Economy plus class %") +
  theme(legend.title=element_blank())

clust8 <- ggplot(clustMeansDF, aes(x = cluster, y = Flight.Distance, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Flight distance") + 
  xlab("Clusters") + 
  ylab("Flight distance (kms)") +
  theme(legend.title=element_blank())

clust9 <- ggplot(clustMeansDF, aes(x = cluster, y = Seat.comfort, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Seat comfort") + 
  xlab("Clusters") +
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust10 <- ggplot(clustMeansDF, aes(x = cluster, y = Departure.Arrival.time.convenient, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Flight time convenience") + 
  xlab("Clusters") + 
  ylab("Score out of five") +  
  theme(legend.title=element_blank())

clust11 <- ggplot(clustMeansDF, aes(x = cluster, y = Food.and.drink, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Food and drink") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust12 <- ggplot(clustMeansDF, aes(x = cluster, y = Gate.location, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Gate location") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust13 <- ggplot(clustMeansDF, aes(x = cluster, y = Inflight.wifi.service, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "In-flight wifi") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust14 <- ggplot(clustMeansDF, aes(x = cluster, y = Inflight.entertainment, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "In-flight entertainment") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust15 <- ggplot(clustMeansDF, aes(x = cluster, y = Online.support, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Online support") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust16 <- ggplot(clustMeansDF, aes(x = cluster, y = Ease.of.Online.booking, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Online booking") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust17 <- ggplot(clustMeansDF, aes(x = cluster, y = On.board.service, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Onboard service") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust18 <- ggplot(clustMeansDF, aes(x = cluster, y = Leg.room.service, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Seat leg room") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust19 <- ggplot(clustMeansDF, aes(x = cluster, y = Baggage.handling, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Baggage handling") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust20 <- ggplot(clustMeansDF, aes(x = cluster, y = Checkin.service, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Check-in service") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust21 <- ggplot(clustMeansDF, aes(x = cluster, y = Cleanliness, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Cleanliness") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust22 <- ggplot(clustMeansDF, aes(x = cluster, y = Online.boarding, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Online boarding") + 
  xlab("Clusters") + 
  ylab("Score out of five") +
  theme(legend.title=element_blank())

clust23 <- ggplot(clustMeansDF, aes(x = cluster, y = Departure.Delay.in.Minutes, fill = cluster)) +
  geom_bar(stat = 'identity', width=0.5, position = position_dodge(), show.legend = FALSE) + 
  theme_minimal() +
  labs(title = "Departure Delay") + 
  xlab("Clusters") + 
  ylab("Minutes") +
  theme(legend.title=element_blank())

ggarrange(clust2, clust3, clust4, clust5,
          clust6, clust7, clust8, clust9, 
          clust10, clust11, clust12, clust13, 
          clust14, clust15, clust16, clust17, 
          clust18, clust19, clust20, clust21,
          clust22, clust23,
          ncol = 4, nrow = 6)

print(clust1)
# # adding cluster assignments to dataset
# Data_clustered <- cbind(Data, cluster = kmm$cluster)

################################## CLASSIFICATION ##############################

# boruta working
boruta_output <- Boruta(Data$satisfaction ~ ., data=Data[, 2:ncol(Data)], doTrace=0)
names(boruta_output)
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

roughFixMod <- TentativeRoughFix(boruta_output)
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
dtResults <- data.frame(matrix(ncol = 5, nrow = 0, dimnames = list(NULL, c("Split Ratio", "Accuracy", "Sensitivity", "Specificity", "F1-Score"))))
for(t in training_data_percentages){
  print("================================================================================================================")
  cat(sprintf("Current train-test split: %s-%1.0f\n", t*100, (1-t)*100))
  
  # partition setup
  indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
  training_data = Data[indx_partition,]
  testing_data = Data[-indx_partition,]
  
  # Set seed for reproducability of results.
  set.seed(42)
  # Train classifier using library, make predictions using trained model.
  TrainedClassifier = C5.0(x = training_data[, 2:ncol(testing_data)], y = training_data[, 1])
  Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[, 2:ncol(testing_data)], type = "prob")[, 2]
  
  # Calculate AUC from decision confidence, draw ROC curve.
  roc_score = roc(testing_data$satisfaction, Predicted_outcomes) #AUC score
  roc_score
  plot(roc_score, main = paste("ROC Curve for DT, Split Ratio", t*100, "-", (1-t)*100))
  
  # Convert decision confidence to factor, produce confusion matrix.
  Predicted_outcomes <- as.factor(ifelse(Predicted_outcomes > 0.5, "satisfied", "dissatisfied"))
  cm <- confusionMatrix(testing_data[, 1], Predicted_outcomes)
  print(cm)
  
  # Add results to table.
  dtResults[t*10, 1] <- t
  dtResults[t*10, 2] <- cm$overall[1]
  dtResults[t*10, 3] <- cm$byClass[1]
  dtResults[t*10, 4] <- cm$byClass[2]
  dtResults[t*10, 5] <- cm$byClass[7]
}

# nb classification
cat("Naive Bayes implementation")
# Initialise results table.
nbResults <- data.frame(matrix(ncol = 5, nrow = 0, dimnames = list(NULL, c("Split Ratio", "Accuracy", "Sensitivity", "Specificity", "F1-Score"))))
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
  plot(roc_score, main = paste("ROC Curve for NB, Split Ratio", t*100, "-", (1-t)*100))
  
  Predicted_outcomes <- as.factor(ifelse(Predicted_outcomes > 0.5, "satisfied", "dissatisfied"))

  cm <- confusionMatrix(testing_data[, 1], Predicted_outcomes)
  print(cm)
  
  nbResults[t*10, 1] <- t
  nbResults[t*10, 2] <- cm$overall[1]
  nbResults[t*10, 3] <- cm$byClass[1]
  nbResults[t*10, 4] <- cm$byClass[2]
  nbResults[t*10, 5] <- cm$byClass[7]
}

# Logistic regression algorithm
cat("Logistic regression implementation")
lrResults <- data.frame(matrix(ncol = 5, nrow = 0, dimnames = list(NULL, c("Split Ratio", "Accuracy", "Sensitivity", "Specificity", "F1-Score"))))
for(t in training_data_percentages){
  print("================================================================================================================")
  cat(sprintf("Current training partition: %s\n", t))

  indx_partition = createDataPartition(Data[, ncol(Data)], p = t, list = FALSE)
  training_data = Data[indx_partition,]
  testing_data = Data[-indx_partition,]

  set.seed(42)
  TrainedClassifier = glm(satisfaction ~ .,family=binomial(link='logit'), data = training_data)
  Predicted_outcomes = predict(TrainedClassifier, newdata = testing_data[, 2:ncol(testing_data)], type = "response")

  roc_score = roc(testing_data$satisfaction, Predicted_outcomes) #AUC score
  plot(roc_score, main = paste("ROC Curve for LR, Split Ratio", t*100, "-", (1-t)*100))
  
  Predicted_outcomes <- as.factor(ifelse(Predicted_outcomes > 0.5, "satisfied", "dissatisfied"))
  
  cm <- confusionMatrix(testing_data[, 1], Predicted_outcomes)
  print(cm)
  
  lrResults[t*10, 1] <- t
  lrResults[t*10, 2] <- cm$overall[1]
  lrResults[t*10, 3] <- cm$byClass[1]
  lrResults[t*10, 4] <- cm$byClass[2]
  lrResults[t*10, 5] <- cm$byClass[7]
}

# combining all results in one dataframe (allResults)
allResults <- rbind(dtResults, nbResults)
allResults <- rbind(allResults, lrResults)
allResults[1:9, 6] <- "C5.0"
allResults[10:18, 6] <- "NB"
allResults[19: 27, 6] <- "LR"
allResults[, 6] <- as.factor(allResults[, 6])
colnames(allResults)[6] <- "Algorithm"

# Plot comparing accuracy
accuplot <- ggplot(data=allResults, aes(x=Split.Ratio, y=Accuracy, group=Algorithm)) +
  geom_line(aes(color = Algorithm))+
  geom_point(aes(color = Algorithm)) +
  labs(title = "Accuracy") +
  xlab("Split ratio") +
  theme_minimal() + 
  theme(legend.position="none")

# Plot comparing sensitivity
sensplot <- ggplot(data=allResults, aes(x=Split.Ratio, y=Sensitivity, group=Algorithm)) +
  geom_line(aes(color = Algorithm))+
  geom_point(aes(color = Algorithm)) +
  labs(title = "Sensitivity") +
  xlab("Split ratio") + 
  theme_minimal() + 
  theme(legend.position="none")

# Plot comparing specificity
specplot <- ggplot(data=allResults, aes(x=Split.Ratio, y=Specificity, group=Algorithm)) +
  geom_line(aes(color = Algorithm))+
  geom_point(aes(color = Algorithm)) +
  labs(title = "Specificity") +
  xlab("Split ratio") + 
  theme_minimal()

# Plot comparing F1-score
f1plot <- ggplot(data=allResults, aes(x=Split.Ratio, y=F1.Score, group=Algorithm)) +
  geom_line(aes(color = Algorithm))+
  geom_point(aes(color = Algorithm)) +
  labs(title = "F1-Score") +
  xlab("Split ratio") + 
  ylab("F1-Score") +
  theme_minimal() + 
  theme(legend.position = "none")

ggarrange(accuplot, f1plot,
          sensplot, specplot,
          ncol = 2, nrow = 2)

##############################################################################

citation("stats")
