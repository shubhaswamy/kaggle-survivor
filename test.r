setwd("C:\\Users\\insertName\\MyFolder\\kaggle")

#var trainData
#read.csv => reads in csv file 
#header = TRUE : retain first line of file with column names 
trainData <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)

#head: displays first 6 lines of data 
head(trainData)

#plots an x-y plot about the general distribution of age 
#na.rm = TRUE means ignore all the NA's in the dataset
plot(density(trainData$Age, na.rm = TRUE))
#replaced variable age with fare 
#$columnName selects entire column of data 
plot(density(trainData$Fare, na.rm = TRUE))

#create a table called counts 
#table with gender as column names female and male 
#survived (1) and did not survive (0) for rows
counts <- table(trainData$Survived, trainData$Sex)
#prints the table 
head(counts)


#creates a bar plot of the table counts
#assigns gender to the x axis and number of people to the y axis 
# main: title 
barplot(counts, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")
#counts[1] top left of counts table 
#counts[2] bottom left of counts table 
counts[2] / (counts[1] + counts[2])
counts[4] / (counts[3] + counts[4])


#creates a table Pclass_survival with survived or not as rows and class as columns 
Pclass_survival <- table(trainData$Survived, trainData$Pclass)
head(Pclass_survival)

#prints a bar plot based on survival 
barplot(Pclass_survival, xlab = "Cabin Class", ylab = "Number of People",
        main = "survived and deceased between male and female")

#survived = white part of bar graph 
#calculates probability of survival
Pclass_survival[2] / (Pclass_survival[1] + Pclass_survival[2])
Pclass_survival[4] / (Pclass_survival[3] + Pclass_survival[4])
Pclass_survival[6] / (Pclass_survival[5] + Pclass_survival[6])


#cleaning up the data 

#-c() remove columns; remove columns 1, and 9 through 12 
trainData = trainData[-c(1,9:12)]
head (trainData)

#in the columns which defines the sex, replace "female" with 1 and "male" with 0
trainData$Sex = gsub("female", 1, trainData$Sex)
trainData$Sex = gsub("^male", 0, trainData$Sex)
head(trainData)

#grep() returns a vector of row numbers which have a specified "value"
master_vector = grep("Master.",trainData$Name, fixed=TRUE)
miss_vector = grep("Miss.", trainData$Name, fixed=TRUE)
mrs_vector = grep("Mrs.", trainData$Name, fixed=TRUE)
mr_vector = grep("Mr.", trainData$Name, fixed=TRUE)
dr_vector = grep("Dr.", trainData$Name, fixed=TRUE)

#prints all the elements in the vector 
print(master_vector)
print(miss_vector)

#use for loop to replace all names to a shortened form of their name prefix 

#for all values (row number values) in master_vector 
#replace the Name column with "Master" 
for(i in master_vector) {
  trainData$Name[i] = "Master"
}
#same as above applied to all the following vectors below
for(i in miss_vector) {
  trainData$Name[i] = "Miss"
}
for(i in mrs_vector) {
  trainData$Name[i] = "Mrs"
}
for(i in mr_vector) {
  trainData$Name[i] = "Mr"
}
for(i in dr_vector) {
  trainData$Name[i] = "Dr"
}


#replacing missing age based on average age for that title 

#get the mean of all the values under Age with the Name "Master", ignore all NA
master_age = round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
miss_age = round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits =2)
mrs_age = round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mr_age = round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
dr_age = round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)


for (i in 1:nrow(trainData)) {
  #if the age is missing 
  if (is.na(trainData[i,5])) {
    #if the title is master 
    if (trainData$Name[i] == "Master") {
      #replace with average age for master (master_age computed above)
      trainData$Age[i] = master_age
    } else if (trainData$Name[i] == "Miss") {
      trainData$Age[i] = miss_age
    } else if (trainData$Name[i] == "Mrs") {
      trainData$Age[i] = mrs_age
    } else if (trainData$Name[i] == "Mr") {
      trainData$Age[i] = mr_age
    } else if (trainData$Name[i] == "Dr") {
      trainData$Age[i] = dr_age
    } else {
      print("Uncaught Title")
    }
  }
}


#create new variables to strengthen model 

#append table trainData by creating new variable called child 
trainData["Child"]
#for 1 to last row
for (i in 1:nrow(trainData)) {
  #assign if child is under 12 
  if (trainData$Age[i] <= 12) {
    trainData$Child[i] = 1
  } else {
    #assign 2 otherwise 
    trainData$Child[i] = 2
  }
}


#create new var Family 
trainData["Family"] = NA

for(i in 1:nrow(trainData)) {
  x = trainData$SibSp[i]
  y = trainData$Parch[i]
  #total the number of sibling/spous (SibSp) and Parents/Children (Parch) 
  #minimum is 1 in case of SibSp or Parch values being = 0
  trainData$Family[i] = x + y + 1
}

#if the person is a mother 
trainData["Mother"] 
for(i in 1:nrow(trainData)) {
  #if under the category of "Mrs" and children > 0 (Parch)
  if(trainData$Name[i] == "Mrs" & trainData$Parch[i] > 0) {
    #assign 1 
    trainData$Mother[i] = 1
  } else {
    #assign 2 
    trainData$Mother[i] = 2
  }
}


#---------------------------------------Final Cleaned Up Data in TestData-----------------------------------------

PassengerId = testData[1]
testData = testData[-c(1, 8:11)]

testData$Sex = gsub("female", 1, testData$Sex)
testData$Sex = gsub("^male", 0, testData$Sex)

test_master_vector = grep("Master.",testData$Name)
test_miss_vector = grep("Miss.", testData$Name)
test_mrs_vector = grep("Mrs.", testData$Name)
test_mr_vector = grep("Mr.", testData$Name)
test_dr_vector = grep("Dr.", testData$Name)

for(i in test_master_vector) {
  testData[i, 2] = "Master"
}
for(i in test_miss_vector) {
  testData[i, 2] = "Miss"
}
for(i in test_mrs_vector) {
  testData[i, 2] = "Mrs"
}
for(i in test_mr_vector) {
  testData[i, 2] = "Mr"
}
for(i in test_dr_vector) {
  testData[i, 2] = "Dr"
}

test_master_age = round(mean(testData$Age[testData$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(testData$Age[testData$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(testData$Age[testData$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(testData$Age[testData$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(testData$Age[testData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(testData)) {
  if (is.na(testData[i,4])) {
    if (testData[i, 2] == "Master") {
      testData[i, 4] = test_master_age
    } else if (testData[i, 2] == "Miss") {
      testData[i, 4] = test_miss_age
    } else if (testData[i, 2] == "Mrs") {
      testData[i, 4] = test_mrs_age
    } else if (testData[i, 2] == "Mr") {
      testData[i, 4] = test_mr_age
    } else if (testData[i, 2] == "Dr") {
      testData[i, 4] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", testData[i,2], sep=""))
    }
  }
}

#We do a manual replacement here, because we weren't able to programmatically figure out the title.
#We figured out it was 89 because the above print statement should have warned us.
testData[89, 4] = test_miss_age

testData["Child"] = NA

for (i in 1:nrow(testData)) {
  if (testData[i, 4] <= 12) {
    testData[i, 7] = 1
  } else {
    testData[i, 7] = 1
  }
}

testData["Family"] = NA

for(i in 1:nrow(testData)) {
  testData[i, 8] = testData[i, 5] + testData[i, 6] + 1
}

testData["Mother"] = NA

for(i in 1:nrow(testData)) {
  if(testData[i, 2] == "Mrs" & testData[i, 6] > 0) {
    testData[i, 9] = 1
  } else {
    testData[i, 9] = 2
  }
}

#print 12 rows of table testData
head(testData, 12 )


#-----------------fitting logistic regression model-----------------

#geeralized linear model 
train.glm <- glm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother, family = binomial, data = trainData)

#see summary 
summary(train.glm)

p.hats <- predict.glm(train.glm, newdata = testData, type = "response")

#assign whether survived or not 
survival <- vector()
for(i in 1:length(p.hats)) {
  if(p.hats[i] > .5) {
    survival[i] <- 1
  } else {
    survival[i] <- 0
  }
}

#outputs kaggleSubmit.csv file 
kaggle.sub <- cbind(PassengerId,survival)
colnames(kaggle.sub) <- c("PassengerId", "Survived")
write.csv(kaggle.sub, file = "kaggleSubmit.csv", row.names = FALSE)
