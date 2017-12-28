#installing packages
#for decision tree
install.packages("party")
#for decision tree
installed.packages("rpart")
#for better plotting
install.packages("rpart.plot")
#loading packages
library(party)
library(rpart)
library(rpart.plot)
#reading the cleaned data with clustering
music_cleaned <- read.csv("musicCleanedWithClusterUsingdClustering.csv", header=T) 
#removing row numebr column
music_cleaned <- music_cleaned[,-1]
music_cleaned_new <- music_cleaned
#songs with popularity of unknown, low, and popular --classification rather than regression)
music_cleaned$popularity <- ifelse(music_cleaned$song.hotttnesss > 0.5, 2,ifelse(music_cleaned$song.hotttnesss ==0, 0,1))
#checking songs based on three categories in songs
plot(music_cleaned$song.hotttnesss ~ music_cleaned$artist.hotttnesss, data = music_cleaned,col=music_cleaned$popularity==0)
plot(music_cleaned$song.hotttnesss ~ music_cleaned$artist.hotttnesss, data = music_cleaned,col=music_cleaned$popularity==1)
plot(music_cleaned$song.hotttnesss ~ music_cleaned$artist.hotttnesss, data = music_cleaned,col=music_cleaned$popularity==2)
#removing continuous target variable
music_cleaned$song.hotttnesss <- NULL 
#backing up cluster for later analysis and then removing it
music_cleaned_cluster<- music_cleaned$cluster
music_cleaned$cluster <- NULL 

str(music_cleaned)
#changing class variable to factor for classification
music_cleaned$popularity <-as.factor(music_cleaned$popularity)
#drawing the decision tree
set.seed(123)
#sampling music data to training and validating set for Estimation --to avoid overfitting--
pd <- sample(2,nrow(music_cleaned), replace=TRUE, prob= c(0.9,0.1))
trainingSet <- music_cleaned[pd==1,]
validationSet <- music_cleaned[pd==2,]
#using party package for classification
popularity_tree <- ctree(popularity ~ . , trainingSet)
plot(popularity_tree, type="simple") 
str(music_cleaned)
#training model
trainTab<-table(predict(popularity_tree), trainingSet$popularity)
trainTab  
#accuracy of trainig set
sum(diag(trainTab))/sum(trainTab)
#accuracy of classifaction in decision tree on test/validation set
validationTab<-table(predict(popularity_tree, newdata= validationSet), validationSet$popularity)
#accuracy of classifaction in decision tree on test/validation set
validationTab
accuracy_of_decision_tree = sum(diag(validationTab))/sum(validationTab)
#precision 
precision_of_decision_tree = diag(validationTab) / rowSums(validationTab)
#precision for each class
precision_of_decision_tree["0"]
precision_of_decision_tree["1"]
precision_of_decision_tree["2"]
#recall 
recall_of_decision_tree <- (diag(validationTab) / colSums(validationTab))
#recall for each class
recall_of_decision_tree["0"]
recall_of_decision_tree["1"]
recall_of_decision_tree["2"]


#decision tree with rplot and pruning it 
set.seed(123)
#sampling music data to training and validating set for Estimation --to avoid overfitting--
# 70% of training set and 30% of validation/testing set- Hold out
pd <- sample(2,nrow(music_cleaned), replace=TRUE, prob= c(0.7,0.3))
trainingSet <- music_cleaned[pd==1,]
validationSet <- music_cleaned[pd==2,]

#training the model
popularity_tree <- rpart(popularity ~ . , data=trainingSet, method = "class")
popularity_tree
rpart.plot(popularity_tree)
#printing complexity parameter 
printcp(popularity_tree)
#drawing the complexity parameter
plotcp(popularity_tree)
#classification with rpart package
prediction <- predict(popularity_tree, validationSet, type="class")
#checking the accuracy of model validation/test set
validationTab <-table(validationSet$popularity, predicted = prediction)
sum(diag(validationTab))/sum(validationTab)
#pruning decision tree by the least amount of error
ptree<- prune(popularity_tree,cp=popularity_tree$cptable[which.min(popularity_tree$cptable[,"xerror"]),"CP"])
#plotting new pruned tree
rpart.plot(ptree) 
#testing/validation with pruned decision tree
prediction <- predict(ptree, validationSet, type = "class")
#accuracy on testing/validation data
validationTab <- table(validationSet$popularity, predicted= prediction)
accuracy_of_decision_tree <- sum(diag(validationTab))/sum(validationTab)
#precision 
precision_of_decision_tree <- diag(validationTab) / rowSums(validationTab)
#precision for each class
precision_of_decision_tree["0"]
precision_of_decision_tree["1"]
precision_of_decision_tree["2"]
#recall 
recall_of_decision_tree <- (diag(validationTab) / colSums(validationTab))
#recall for each class
recall_of_decision_tree["0"]
recall_of_decision_tree["1"]
recall_of_decision_tree["2"]


#enhancing with hybrid appraoch
#using clustering as an helping factor in decision making 

set.seed(123)
#sampling data frame to 70% of training set and 30% of validation/testing set- Holdout
pd <- sample(2,nrow(music_cleaned_new), replace=TRUE, prob= c(0.7,0.3))
trainingSet <- music_cleaned_new[pd==1,]
validationSet <- music_cleaned_new[pd==2,]
#removing previous clusters 
trainingSet$cluster <- NULL
validationSet$cluster <- NULL
#normalisation using scaling
means = apply(trainingSet, 2, mean)
sds = apply(trainingSet, 2, sd)
nrml_music = scale(trainingSet, center = means, scale=sds)
means2 = apply(validationSet, 2, mean)
sds2 = apply(validationSet, 2, sd)
nrml_music2 = scale(validationSet, center = means2, scale=sds2)

#checking the elbow point for best number of k for k-means clustering
mydata <- trainingSet[1:3]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)
mydata <- validationSet[1:3]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

#clustering each set with efficient number of cluster--from The Elbow Method individually 
k <- kmeans(nrml_music,6)
k2 <- kmeans(nrml_music2 ,6)
#adding cluster number to each song of samples
trainingSet$cluster <- k$cluster
validationSet$cluster <- k2$cluster
#changing cluster numbers to be a factor
trainingSet$cluster <-as.factor(trainingSet$cluster) 
validationSet$cluster <-as.factor(validationSet$cluster) 
#definition of popularity measures
trainingSet$popularity <- ifelse(trainingSet$song.hotttnesss > 0.5, 2,ifelse(trainingSet$song.hotttnesss ==0, 0,1))
validationSet$popularity <- ifelse(validationSet$song.hotttnesss > 0.5, 2,ifelse(validationSet$song.hotttnesss ==0, 0,1))
#changing the problem from regression to classification
trainingSet$popularity <- as.factor(trainingSet$popularity)
validationSet$popularity <- as.factor(validationSet$popularity)
#removing song popularity 
trainingSet$song.hotttnesss <- NULL
validationSet$song.hotttnesss <- NULL
#trainig the model/drawing decision tree 
popularity_tree <- ctree(popularity ~ ., trainingSet)
#plotting the tree
plot(popularity_tree, type="simple") 

#checking the accuracy for training set 
trainTab<-table(predict(popularity_tree), trainingSet$popularity)
trainTab 
#accuracy of classifaction in decision tree
sum(diag(trainTab))/sum(trainTab)
validationTab<-table(predict(popularity_tree, newdata= validationSet), validationSet$popularity)
accuracy_of_decision_tree <- sum(diag(validationTab))/sum(validationTab)
#precision 
precision_of_decision_tree <- diag(validationTab) / rowSums(validationTab)
#precision for each class
precision_of_decision_tree["0"]
precision_of_decision_tree["1"]
precision_of_decision_tree["2"]
#recall 
recall_of_decision_tree <- (diag(validationTab) / colSums(validationTab))
#recall for each class
recall_of_decision_tree["0"]
recall_of_decision_tree["1"]
recall_of_decision_tree["2"]

