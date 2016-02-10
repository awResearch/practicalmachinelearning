library(caret);


downloadData <- function() {
  if (!file.exists("pml-data")) {
    dir.create("pml-data");
  }
  
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                "./pml-data/pml-training.csv");
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                "./pml-data/pml-testing.csv");
}

cleanupData <- function(dataTrain) {
  
  naValues  <- apply(dataTrain, 2, function(x) {sum(is.na(x))});
  dataTrain <- dataTrain[,which(naValues == 0)];
  
  dataTrain <- dataTrain[,8:length(dataTrain)];
  
  return (dataTrain);
}



#downloadData();
plmTrain           <- read.csv("./pml-data/pml-training.csv", na.strings= c("NA",""," "));
plmTrain           <- cleanupData(plmTrain);

set.seed(3888);

indexTrain         <- createDataPartition(y = plmTrain$classe, p = 0.7, list = FALSE)
plmTrainTraining   <- plmTrain[indexTrain, ]
plmTrainCrossval   <- plmTrain[-indexTrain, ]


modelObj               <- train(classe ~., data=plmTrainTraining, method="rf");

predictCrossvalResult  <- predict(modelObj, plmTrainCrossval)
confusionMatrix(plmTrainCrossval$classe, predictCrossvalResult)


plmTest <- read.csv("./pml-data/pml-testing.csv", na.strings= c("NA",""," "))
plmTest <- cleanupData(plmTest);

predictTestResult <- predict(modelObj, plmTest)
predictTestResult


