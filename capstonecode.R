#Opening the cleaned dataset
bike24 <- read.csv("/cloud/project/finalbike24.csv")

table(bike24$rideable_type)

head(bike24)

class(bike24$date)

#Removing ID column
bike24 <- bike24[-1]

#Editing classes
bike24$date <- as.Date(bike24$date)
bike24$rideable_type <- as.factor(bike24$rideable_type)
bike24$member_casual <- as.factor(bike24$member_casual)
bike24$season <- as.factor(bike24$season)
bike24$startneigh <- as.factor(bike24$startneigh)
bike24$startcity <- as.factor(bike24$startcity)

#Classifying neighborhood levels with minimal instances under "Other" category
startneigh_table <- table(bike24$startneigh)
badlevls <- names(startneigh_table[startneigh_table < 5])
bike24$startneigh <- as.factor(ifelse(bike24$startneigh %in% badlevls, "Other", as.character(bike24$startneigh)))

head(bike24)
names(bike24)

#Taking out non-numeric variables for vifstep
numerics <- bike24[,c(9, 10, 11, 12, 16, 20, 21, 22, 23, 25 )]

head(numerics)

#performing vifstep to identify variables with high multicollinearity
library(usdm)
vifstep(numerics, th=10)

#Taking out variables that have high multicollinearity and re-running the vifstep function on the new dataset
newnum <- bike24[,c(16, 20, 21, 22, 23, 25 )]
names(newnum)
vifstep(newnum, th=10)

head(bike24)

names(bike24)

#creating a new full datset without the multicollinearity functions but with the categorical variables.
#The columns taken out are duplicates of longitude and latitude. No unique column has multicollinearity.
#Also removing the NA-filled 'mont' variable
goodbike <- bike24[,c(-9, -10, -11, -12, -19),]
head(goodbike)
names(goodbike)

#further removing predictors that are not necessary/needed
goodbike.trim <- goodbike[,-c(1,3:4,10,12,13:14)] 
head(goodbike.trim)
names(goodbike.trim)

#creating a final dataset for analysis with relevant variables
finalbike <- goodbike.trim[, c(1, 7, 13, 12, 6, 16, 14, 8, 9, 10, 11, 17)]

head(finalbike)
names(finalbike)

#Creating a training and testing subset of 8000 and 2000 rows respectively for analysis and cross-valdiation
biketrain = finalbike[1:8000,]
bikevalidate = finalbike[8001:10000,]

#bikeglm <- glm(as.factor(rideable_type)~., data=goodbike.trim, family=binomial(link=logit))

names(finalbike)

#Creating a logistic regression model
bikeglm <- glm(as.factor(rideable_type)~.,data = biketrain, family=binomial(link=logit))

#Performing variable selection using AIC and BIC criteria
bic <- step(bikeglm, k=log(10000)) #bic = 11660.69
aic <- step(bikeglm, k=2) #aic = 10941.62

#Viewing new models
       aic #aic 8540
       bic #bic 8632
       
       
       #creating the confusion matrix for bic
       pi.hat<-predict(bic, bikevalidate, type="response")
       Y.hat <- ifelse(pi.hat>0.5,"electric","classic")
       bictab <- table(bikevalidate$rideable_type,Y.hat) #confusion matrix
       
       #creating the confusion matrix for aic
       pi.hat2<-predict(aic, bikevalidate, type="response")
       Y.hat2 <- ifelse(pi.hat2>0.5,"electric","classic")
       aictab <- table(bikevalidate$rideable_type,Y.hat2) #confusion matrix
       
       #summarizing both models
       summary(aic)
       summary(bic)
       
       
       #Building a tree model
       library(tree)
       
       #Creating a new training set using character rather than factor neighborhoods
       biketrain2 <- biketrain
       biketrain2$startneigh <- as.character(biketrain2$startneigh)
       
       biketree <- tree(rideable_type~.,data=biketrain2)
       biketree
       
       #Plotting the tree
       plot(biketree)
       text(biketree,pretty=0)
       
       #Performing ten-fold cross validation using different pruned trees
       result = cv.tree(biketree,K=10,FUN=prune.tree)
       plot(result)
       
       #The tree with four nodes seems to have the best performance
       
       #Creating the optimal tree
       biketree2 = prune.tree(biketree,best=4)
       plot(biketree2)
       text(biketree2,pretty=0)
       
       #Summarizing the tree
       summary(biketree2)
       
       #Creating a list of different thresholds ranging from 0.05 to 0.5
       threshold <- seq(from=0.05, to=0.5, by =0.05)
       
       #Creating empty lists to contain each sensitivity and specificity value for each threshold
       sensy <- rep(NA, 10)
       specy <- rep(NA, 10)
       
       #Calculating the sensitivity and specificity for each threshold for aic
       for (i in 1:length(threshold)) {
         pi.hat<-predict(aic, bikevalidate, type="response")
         Y.hat4 <- ifelse(pi.hat>threshold[i],"electric","classic")
         print(threshold[i])
         oke <- table(bikevalidate$rideable_type,Y.hat4) #confusion matrix
         print(oke)
         sensy[i] <- oke[2,2]/(oke[2,1]+oke[2,2])
         specy[i] <- oke[1,1]/(oke[1,1]+oke[1,2])
       }
       
       par(mfrow = c(1, 1))
       
       #Creating a line chart depicting sensitivity and specificity for each threshold for aic
       plot(threshold,sensy, type="b", col="blue", lwd=5, pch=17, xlab="Threshold", ylab="Value", ylim=range(specy,sensy))
       lines(threshold, specy, type="b", col="turquoise", lwd=2, pch=9)
       legend(0.05, .57, c("Sensitivity","Specificity"), lwd=c(2,2), col=c("blue","turquoise"), y.intersp=1.5)
       title("Sensitivity and Specificity for Various Thresholds AIC")
       
       #Repeating the same for the bic model
       sensy
       threshold
       
       sensy2 <- rep(NA, 10)
       specy2 <- rep(NA, 10)
       
       for (i in 1:length(threshold)) {
         pi.hat5<-predict(bic, bikevalidate, type="response")
         Y.hat5 <- ifelse(pi.hat5>threshold[i],"electric","classic")
         print(threshold[i])
         oke <- table(bikevalidate$rideable_type,Y.hat4) #confusion matrix
         print(oke)
         sensy2[i] <- oke[2,2]/(oke[2,1]+oke[2,2])
         specy2[i] <- oke[1,1]/(oke[1,1]+oke[1,2])
       }
       
       par(mfrow = c(1, 1))
       
       plot(threshold,sensy, type="b", col="blue", lwd=5, pch=17, xlab="Threshold", ylab="Value", ylim=range(specy,sensy))
       lines(threshold, specy, type="b", col="turquoise", lwd=2, pch=9)
       legend(0.05, .57, c("Sensitivity","Specificity"), lwd=c(2,2), col=c("blue","turquoise"), y.intersp=1.5)
       title("Sensitivity and Specificity for Various Thresholds BIC")
       
       #Creating a list of different thresholds ranging from 0.05 to 0.5
       threshold <- seq(from=0.05, to=0.5, by =0.05)
       
       #Creating empty lists to contain each sensitivity and specificity value for each threshold
       sensy3 <- rep(NA, 10)
       specy3 <- rep(NA, 10)
       
       #Calculating the sensitivity and specificity for each threshold for tree
  
       
       for (i in 1:length(threshold)) {
         # Predict classes
         pi.hat3 <- predict(biketree2, bikevalidate, type = "class")
         
         # Confusion matrix
         oke <- table(bikevalidate$rideable_type, pi.hat3)
         print(oke)
         
         # Sensitivity and specificity
         sensy3[i] <- oke["electric_bike", "electric_bike"] / 
           (oke["electric_bike", "electric_bike"] + oke["electric_bike", "classic_bike"])
         
         specy3[i] <- oke["classic_bike", "classic_bike"] / 
           (oke["classic_bike", "classic_bike"] + oke["classic_bike", "electric_bike"])
         
         print(threshold[i])
       }
    
       
       par(mfrow = c(1, 1))
       
       #Creating a line chart depicting sensitivity and specificity for each threshold for tree
 plot(threshold,sensy3, type="b", col="blue", lwd=5, pch=17, xlab="Threshold", ylab="Value", ylim=range(specy3,sensy3))
  lines(threshold, specy3, type="b", col="turquoise", lwd=2, pch=9)
  legend(0.05, .57, c("Sensitivity","Specificity"), lwd=c(2,2), col=c("blue","turquoise"), y.intersp=1.5)
 title("Sensitivity and Specificity for Various Thresholds Tree")
       #All thresholds seem to perform the same
       
       #Looking at its performance
       #Creating a confusion matrix for the tree model
       pi.hat3 <- predict(biketree2,newdata=bikevalidate)
       Y.hat3 <- ifelse(pi.hat3[,2]>0.5,"electric","classic")
       treetab <-table(bikevalidate$rideable_type,Y.hat3[]) #confusion matrix
       
       #Comparing all confusion matricies
       treetab
       aictab
       bictab
       
       #Calculating performance metrics for tree model
       
       senstree <- treetab[2,2]/(treetab[2,1]+treetab[2,2])
       spectree <- treetab[1,1]/(treetab[1,1]+treetab[1,2])
       acctree <- (treetab[1,1]+treetab[2,2])/2000
       
       #Calculating performance metrics for bic model
       
       sensbic <- bictab[2,2]/(bictab[2,1]+bictab[2,2])
       specbic <- bictab[1,1]/(bictab[1,1]+bictab[1,2])
       accbic <- (bictab[1,1]+bictab[2,2])/2000
       
       
       #Calculating performance metrics for tree model
       
       sensaic <- aictab[2,2]/(aictab[2,1]+aictab[2,2])
       specaic <- aictab[1,1]/(aictab[1,1]+aictab[1,2])
       accaic <- (aictab[1,1]+aictab[2,2])/2000
       
       #Comparing sensitivity
       #aic is highest
       senstree #0.180791
       sensbic #0.04519774
       sensaic #0.06026365
       
       #Comparing specificity
       #bic is higher
       spectree #0.9353302
       specbic #0.9911504
       specaic #0.9884275
       
       #Comparing accuracy
       #aic is higher
       acctree #0.735
       accbic #0.74
       accaic #0.742
       
       #Looking at ROC on the three models to consider the trade-off between sensitivity and specificity
       #also calculating the area under each curve
       library(pROC)
       
       par(mfrow = c(1, 3))
       
       plot(roc(bikevalidate$rideable_type, pi.hat)) + title("BIC") #bic
       auc(bikevalidate$rideable_type, pi.hat)
       #Area under the curve: 0.6737
       
       plot(roc(bikevalidate$rideable_type, pi.hat2)) + title("AIC") #aic
       auc(bikevalidate$rideable_type, pi.hat2)
       #Area under the curve: 0.6741
       
       plot(roc(bikevalidate$rideable_type, pi.hat3[,2])) + title("Tree") #tree
       auc(bikevalidate$rideable_type, pi.hat3[,2])
       #Area under the curve: 0.598
       
       #Creating a list of different thresholds ranging from 0.05 to 0.5
       threshold <- seq(from=0.05, to=0.5, by =0.05)
       
       #Creating empty lists to contain each sensitivity and specificity value for each threshold
       sensy <- rep(NA, 10)
       specy <- rep(NA, 10)
       
       #Calculating the sensitivity and specificity for each threshold for aic
       for (i in 1:length(threshold)) {
         pi.hat<-predict(aic, bikevalidate, type="response")
         Y.hat4 <- ifelse(pi.hat>threshold[i],"electric","classic")
         print(threshold[i])
         oke <- table(bikevalidate$rideable_type,Y.hat4) #confusion matrix
         print(oke)
         sensy[i] <- oke[2,2]/(oke[2,1]+oke[2,2])
         specy[i] <- oke[1,1]/(oke[1,1]+oke[1,2])
       }
       
       par(mfrow = c(1, 1))
       
       #Creating a line chart depicting sensitivity and specificity for each threshold for aic
       plot(threshold,sensy, type="b", col="blue", lwd=5, pch=17, xlab="Threshold", ylab="Value", ylim=range(specy,sensy))
       lines(threshold, specy, type="b", col="turquoise", lwd=2, pch=9)
       legend(0.05, .57, c("Sensitivity","Specificity"), lwd=c(2,2), col=c("blue","turquoise"), y.intersp=1.5)
       title("Sensitivity and Specificity for Various Thresholds AIC")
       
       #Repeating the same for the bic model
       sensy
       threshold
       
       sensy2 <- rep(NA, 10)
       specy2 <- rep(NA, 10)
       
       for (i in 1:length(threshold)) {
         pi.hat5<-predict(bic, bikevalidate, type="response")
         Y.hat5 <- ifelse(pi.hat5>threshold[i],"electric","classic")
         print(threshold[i])
         oke <- table(bikevalidate$rideable_type,Y.hat4) #confusion matrix
         print(oke)
         sensy2[i] <- oke[2,2]/(oke[2,1]+oke[2,2])
         specy2[i] <- oke[1,1]/(oke[1,1]+oke[1,2])
       }
       
       par(mfrow = c(1, 1))
       
       plot(threshold,sensy, type="b", col="blue", lwd=5, pch=17, xlab="Threshold", ylab="Value", ylim=range(specy,sensy))
       lines(threshold, specy, type="b", col="turquoise", lwd=2, pch=9)
       legend(0.05, .57, c("Sensitivity","Specificity"), lwd=c(2,2), col=c("blue","turquoise"), y.intersp=1.5)
       title("Sensitivity and Specificity for Various Thresholds BIC")
       
       
       #Creating a residual vs fitted plot
       plot(residuals(aic) ~ fitted(aic)) + title("Residual vs. Fitted Plot for AIC Model")
       
       #Creating a residual vs leverage plot
       plot(aic, which = 5)
       
       #Removing outliers
       finalbiketrain <- biketrain[c(-6254, -3854, -6918, -6642, -6717 ), ]
       
       #Creating a final aic model with no outliers
       finalbikeglm <- glm(as.factor(rideable_type)~.,data = finalbiketrain, family=binomial(link=logit))
       finalaic <- step(finalbikeglm, k=2) #aic = 10941.62
       finalaic
       
       #Summary statistics for the final model
       summary(finalaic)
       
       head(bike24)
#Plotting diagnostic charts for the final model
       par(mfrow = c(2, 2))
       
plot(aic)   
par(mfrow = c(2, 2))
plot(finalbikeglm)       


#Creating the correlation heatmap
library(ggplot2)
library(corrplot)
library(pheatmap)

par(mfrow = c(1, 1))

library(caret)

#Creating a new dataset with numeric numbers for correlation map
numerics2 <- numerics

names(numerics2)

#Removing tripduration, as log trip duration is already recorded
numerics2 <- numerics2[,c(-5)]
  
names(numerics2)

#Fixing column classes
numerics2$rideable_type <- as.character(bike24$rideable_type)
numerics2$member_casual <- as.character(bike24$member_casual)
numerics2$sameneigh <- as.character(bike24$sameneigh)

#Turning rideable_type into a binary numeric variable
for (i in 1:nrow(numerics2)) {
  numerics2$rideable_type[i] <- ifelse(numerics2$rideable_type[i] == "electric_bike", 1, 0)
}

#Turning member_casual and sameneigh into binary numeric variables
numerics2$member_casual <- ifelse(numerics2$member_casual == "member", 1, 0)
numerics2$sameneigh <- ifelse(numerics2$sameneigh == "TRUE", 1, 0)

#Changing the classes of these variables to numeric
numerics2$member_casual <- as.numeric(numerics2$member_casual)
numerics2$sameneigh <- as.numeric(numerics2$sameneigh)

class(numerics2$rideable_type)
numerics2$rideable_type <- as.numeric(numerics2$rideable_type)

head(numerics2)
names(numerics2)

#Removing startcoords and endcoords as these are duplicates of start and end lat
numerics2 <- numerics2[,c(-5, -6, -7, -8)]

#Omitting NA values
numerics2 <- na.omit(numerics2)

#Recording correlation matrix for remaining variables
cors <- cor(numerics2)
dim(cors)
cors

#Creating a correlation heatmap
pheatmap(cors, display_numbers = TRUE, 
         main = "Correlation Heatmap", 
         color = colorRampPalette(c("white", "turquoise", "royalblue2"))(100))

library(leaflet)
library(leaflet.extras)  

library(dplyr)

# Create a heatmap overlaid on a map of Boston for starting points with electric bikes
leaflet(numerics2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(lng = ~start_lng, lat = ~start_lat, intensity = ~rideable_type, 
             radius = 15, blur = 1, max = 1,  # max=1 since it's binary (0 or 1)
             gradient = c("turquoise", "blue", "black"))

#Doing the same for ending points with e-bikes
leaflet(numerics2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(lng = ~end_lng, lat = ~end_lat, intensity = ~rideable_type, 
             radius = 15, blur = 1, max = 1,  # max=1 since it's binary (0 or 1)
             gradient = c("turquoise", "blue"))
