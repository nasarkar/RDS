#install.packages("gridExtra")
#install.packages("Hmisc")
#install.packages("corrplot")

library(Hmisc)
library(tidyverse)
library(gridExtra)
library(knitr)
library(caret)
library(randomForest)
library(corrplot)


# Reading Titanic Survial Data
trainFile <- "train.csv" #Training data set
testFile <- "test.csv" #Test data set

#Reading training and test data files into R Data Frame

tiTanicTrainData <- read.csv(trainFile, header = TRUE, sep = ",", na.strings = c("", "NA"),stringsAsFactors = F )
tiTanicTestData <-  read.csv(testFile, header = TRUE, sep = ",", na.strings = c("", "NA"),stringsAsFactors = F )

#Data Description
str(tiTanicTrainData)

# Merging test and training data set for cleaning and feature engineering
tiTanicTestData$Survived <- NA
entData <- rbind(tiTanicTrainData,tiTanicTestData)

#Analyse variables with missing values. Blanks also imported as NA (not applicable)
sapply(entData, function(x) sum(is.na(x)) )

#Convertcategorical variables into factors
entData$Survived <-as.factor(entData$Survived)
entData$Pclass <- as.factor(entData$Pclass)
entData$Sex <- as.factor(entData$Sex)
entData$Embarked <- as.factor(entData$Embarked)

#Filter Training Data from entire data features analysis
trainData <- entData[!is.na(entData$Survived),]

ggplot(data = trainData,aes(x= Survived,fill = Survived)) + geom_bar(stat = 'count') + 
  geom_label(stat= 'count',aes(label = ..count..))

qp0 <- ggplot(trainData, aes(Sex, fill = Survived)) +
  geom_bar(stat = 'count', position = 'dodge') +
  geom_label(stat= 'count',aes(label = ..count..))

qp1<- ggplot(trainData, aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  geom_label(stat= 'count',aes(label = ..count..))

qp2<- ggplot(trainData, aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  facet_grid(~Sex)

qp3 <- ggplot(trainData, aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(y= "Percent") +
  facet_wrap(~Sex)


grid.arrange(qp0, qp1, qp2, qp3, ncol=2)


#Extracting Title and Surname from Name
#Using Title variable to estimate missing ages

entData$Surname <- sapply(entData$Name, function(x) { strsplit(x, split='[,.]')[[1]][1] } )

#correcting some surnames that also include a maiden name
entData$Surname <- sapply(entData$Surname, function(x) {strsplit(x, split='[-]')[[1]][1]})
entData$Title <- sapply(entData$Name, function(x) {strsplit(x, split='[,.]')[[1]][2]})
entData$Title <- sub(' ', '', entData$Title) #removing spaces before title
kable(table(entData$Sex, entData$Title))

# Reducing the number of titles to create better and substantial titles that
# be used in prediction

entData$Title[entData$Title %in% c("Mlle", "Ms")] <- "Miss"
entData$Title[entData$Title== "Mme"] <- "Mrs"
entData$Title[!(entData$Title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- "Misc. Title"
entData$Title <- as.factor(entData$Title)
kable(table(entData$Sex, entData$Title))

trainData <- entData[!is.na(entData$Survived),]

ggplot(trainData, aes(x = Title, fill = Survived)) +
  geom_bar(stat='count', position='stack')

#Creating family size variable (Fsize)
entData$Fsize <- entData$SibSp+entData$Parch +1

trainData <- entData[!is.na(entData$Survived),]

ggplot(trainData, aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11))

#Composing data frame with group size for each Ticket
TicketGroup <- entData %>%
  select(Ticket) %>%
  group_by(Ticket) %>%
  summarise(Tsize=n())

entData <- left_join(entData, TicketGroup, by = "Ticket")

ggplot(entData[!is.na(entData$Survived),], aes(x = Tsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) 

#taking the max of family and ticket size as the group size
entData$Group <- entData$Fsize
for (i in 1:nrow(entData)){
  entData$Group[i] <- max(entData$Group[i], entData$Tsize[i])
}

 ggplot(entData[!is.na(entData$Survived),], aes(x = Group, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) 

#display passengers with missing Embarked
kable(entData[which(is.na(entData$Embarked)),c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group') ])

entData$FarePP <- entData$Fare/entData$Tsize #creating the Fare Per Person variable

tab2 <- entData[(!is.na(entData$Embarked) & !is.na(entData$Fare)),] %>%
  group_by(Embarked, Pclass) %>%
  summarise(FarePP=median(FarePP))

kable(tab2)

#imputing missing Embarked values
entData$Embarked[entData$Ticket=='113572'] <- 'C'

#converting Embarked into a factor
entData$Embarked <- as.factor(entData$Embarked)

#display passengers with missing Fare
kable(entData[which(is.na(entData$Fare)), c('Surname', 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Group')])

#imputing FarePP (as the Fare will be dropped later on anyway)
entData$FarePP[1044] <- 7.8

#Replacing fares of value 0 by the median FarePP’s for each Pclass
tab3 <- entData[(!is.na(entData$FarePP)),] %>%
  group_by(Pclass) %>%
  summarise(MedianFarePP=median(FarePP))

entData <- left_join(entData, tab3, by = "Pclass")
entData$FarePP[which(entData$FarePP==0)] <- entData$MedianFarePP[which(entData$FarePP==0)]

# The FarePP is very skewed, this is not desirable for some algorithms
ggplot(entData, aes(x=FarePP)) +
  geom_histogram(binwidth = 5, fill='blue') + 
  scale_x_continuous(breaks= seq(0, 150, by=10))

#Note Hmisc needs to be loaded before dplyr, as the other way around errors occured due to the kernel using the Hmisc summarize function instead of the dplyr summarize function
entData$FareBins <- cut2(entData$FarePP, g=5)

ggplot(entData[!is.na(entData$Survived),], aes(x=FareBins, fill=Survived))+
  geom_bar(stat='count')  + facet_grid(.~Pclass)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Density plot for age analysis
ggplot(entData[(!is.na(entData$Survived) & !is.na(entData$Age)),], aes(x = Age)) +
  geom_density(alpha = 0.5,aes(fill = Survived)) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# Box plot analysis for Title predictor with Age 
ggplot(entData[!is.na(entData$Age),], aes(x = Title, y = Age, fill=Pclass )) +
  geom_boxplot() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

#Predicting Age with Linear Regression
set.seed(12000)
AgeLM <- lm(Age ~ Pclass + Sex + SibSp + Parch + Group + Title, data=entData[!is.na(entData$Age),])
summary(AgeLM)

entData$AgeLM <- predict(AgeLM, entData)

par(mfrow=c(1,2))
hist(entData$Age[!is.na(entData$Age)], main='Original data, non-missing', xlab='Age', col='green')
hist(entData$AgeLM[is.na(entData$Age)], main= 'LM NA predictions', xlab='Age', col='orange', xlim=range(0:80))

#display which passengers are predicted to be children (age<18) with Linear Regression.
entData[(is.na(entData$Age) & entData$AgeLM <18), c('Sex', 'SibSp', 'Parch','Title', 'Pclass', 'Survived', 'AgeLM')]

#imputing Linear Regression predictions for missing Ages
indexMissingAge <- which(is.na(entData$Age))
indexAgeSurvivedNotNA<- which(!is.na(entData$Age) & (!is.na(entData$Survived)))
entData$Age[indexMissingAge] <- entData$AgeLM[indexMissingAge]

#replacing NAs with imaginary Deck U, and keeping only the first letter of ech Cabin (=Deck)
entData$Cabin[is.na(entData$Cabin)] <- "U"
entData$Cabin <- substring(entData$Cabin, 1, 1)
entData$Cabin <- as.factor(entData$Cabin)

ggplot(entData[(!is.na(entData$Survived)& entData$Cabin!='U'),], aes(x=Cabin, fill=Survived)) +
  geom_bar(stat='count') + facet_grid(.~Pclass) + labs(title="Survivor split by class and Cabin")

c1 <- round(prop.table(table(entData$Survived[(!is.na(entData$Survived)&entData$Cabin!='U')], 
                             entData$Cabin[(!is.na(entData$Survived)&entData$Cabin!='U')]),2)*100)
kable(c1)

# What does Embarked tell us ?
d1 <- ggplot(entData[!is.na(entData$Survived),], aes(x = Embarked, fill = Survived)) +
  geom_bar(stat='count')  + labs(x = 'Embarked', y= 'Count')

d2 <- ggplot(entData[!is.na(entData$Survived),], aes(x = Embarked, fill = Survived)) +
  geom_bar(stat='count', position= 'fill')  + labs(x = 'Embarked', y= 'Percent')

grid.arrange(d1, d2, nrow=1)

ggplot(entData[indexAgeSurvivedNotNA,], aes(x = Age, fill = Survived)) +
  geom_histogram(aes(fill=factor(Survived))) + labs(title="Survival density, known-ages, and Embarked") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + facet_grid(.~Embarked)

tab1 <- rbind(table(entData$Embarked[!is.na(entData$Survived)]),table(entData$Embarked[indexAgeSurvivedNotNA]))
tab1 <- cbind(tab1, (rowSums(tab1)))
tab1 <- rbind(tab1, tab1[1,]-tab1[2,])
tab1 <- rbind(tab1, round((tab1[3,]/tab1[1,])*100))
rownames(tab1) <- c("All", "With Age", "Missing Age", "Percent Missing")
colnames(tab1) <- c("C", "Q", "S", "Total")
kable(tab1)

TicketSurvivors <- entData %>%
  group_by(Ticket) %>%
  summarize(Tsize = length(Survived),
            NumNA = sum(is.na(Survived)),
            SumSurvived = sum(as.numeric(Survived)-1, na.rm=T))

entData <- left_join(entData, TicketSurvivors)
## Joining, by = c("Ticket", "Tsize")

entData$AnySurvivors <- ''
entData$AnySurvivors[entData$Tsize==1] <- 'other'
entData$AnySurvivors[entData$Tsize>=2] <- ifelse(entData$SumSurvived[entData$Tsize>=2]>=1, 'survivors in group', 'other')
entData$AnySurvivors <- as.factor(entData$AnySurvivors)
kable(x=table(entData$AnySurvivors), col.names= c('AnySurvivors', 'Frequency'))

#splitting data into train and test set again
trainClean <- entData[!is.na(entData$Survived),]
testClean <- entData[is.na(entData$Survived),]

#Random Forest Model
set.seed(2017)

caret_matrix <- train(x=trainClean[,c('Pclass','Sex','Age','AnySurvivors','Group', 'FarePP')], 
                      y=trainClean$Survived, data=trainClean, 
                      method='rf', trControl=trainControl(method="cv", number=5))
caret_matrix

caret_matrix$results

varImpPlot(caret_matrix$finalModel, main=" Variable importance")

#Using the model to make Survival predictions on the test set
solution_rf <- predict(caret_matrix, testClean)


#Model SVM
set.seed(2017)
caret_svm <- train(Survived~ Sex+ Pclass + FarePP + AnySurvivors + Age ++Group, data=trainClean, method='svmRadial', 
                   preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=5))

caret_svm

#using the model to make Survival predictions on the test set
solution_svm <- predict(caret_svm, testClean)

#Model Boosted Logistic Regression
set.seed(2017)
caret_logit <- train(Survived~ Sex+ Pclass + FarePP + AnySurvivors + Age ++Group, data=trainClean, method='LogitBoost', 
                   preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=5))

caret_logit
solution_logit <- predict(caret_logit, testClean)


#Model GBM
set.seed(2017)
caret_boost <- train(Survived~ Sex+ Pclass + FarePP + AnySurvivors + Age + Group, data=trainClean, 
                     method='gbm', preProcess= c('center', 'scale'), 
                     trControl=trainControl(method="cv", number=7), verbose=FALSE)
caret_boost

#using the model to make Survival predictions on the test set
solution_boost <- predict(caret_boost, testClean)

#adding model predictions to test dataframe
testClean$RF <- as.numeric(solution_rf)-1
testClean$SVM <- as.numeric(solution_svm)-1
testClean$Boost <- as.numeric(solution_boost)-1
testClean$logit <- as.numeric(solution_logit) - 1


#compose correlations plot
corrplot.mixed(cor(testClean[, c('RF', 'SVM', 'Boost')]), order="hclust", tl.col="black")

#testClean$Sum <- testClean$RF + testClean$SVM + testClean$Boost
#testClean$Majority <- ifelse(testClean$Sum<=1, 0, 1)
#testClean$DisagreeSVM <- ifelse(testClean$RF==testClean$Boost & testClean$SVM != testClean$RF, testClean$RF, testClean$SVM)

#predictions of the models on the training set
trainClean$RF <- predict(caret_matrix, trainClean)
confusionMatrix(data=trainClean$RF, reference=trainClean$Survived)

trainClean$SVM <- predict(caret_svm, trainClean)
confusionMatrix(data=trainClean$SVM, reference=trainClean$Survived)

trainClean$Boost <- predict(caret_boost, trainClean)
confusionMatrix(data=trainClean$Boost, reference=trainClean$Survived)

trainClean$logit <- predict(caret_logit, trainClean)
confusionMatrix(data=trainClean$logit, reference=trainClean$Survived)

ggplot(trainClean[trainClean$Survived != trainClean$RF,], aes(x=Pclass, fill=RF)) +
  geom_bar(stat='count') + facet_wrap(~Sex) + labs(title="FP and FN, RF model")
FPN = trainClean[trainClean$Survived != trainClean$RF,c('Pclass', 'Sex', 'Age', 'SibSp','Parch','Group', 'Survived','RF')] 
arrange(FPN, Pclass, Sex, Age)

#selecting RF prediction
testClean$Select <- testClean$RF

#writing final submission file
submission_select <- data.frame(PassengerId = tiTanicTestData$PassengerId, Survived = testClean$Select)
write.csv(submission_select, file = 'Titanic_RF.csv', row.names = F)
