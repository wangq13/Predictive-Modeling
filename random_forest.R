###########################################
##### Random Forest using NIAID Locus #####
#####                                 #####
##### Qinlu Wang (qinlu.wang@nih.gov) #####
###########################################

# Source codes: A A very basic introduction to Random Forests using R
#               https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/

library(randomForest)  #package for random forest
library(ROCR)          #package for ROC

# Set random seed to make results reproducible:
set.seed(17)

# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(iris)/2)

# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(iris), size = data_set_size)

# Assign the data to the correct sets
training <- iris[indexes,]
validation1 <- iris[-indexes,]

# Perform training:
rf_classifier = randomForest(Species ~ ., data=training, ntree=100, mtry=2, importance=TRUE)

# results of our classifier 
rf_classifier

# importance parameter 
varImpPlot(rf_classifier)

# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)

# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier,validation1[,-5],type="prob")

# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")

# Specify the different classes 
classes <- levels(validation1$Species)

# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(validation1[,5]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  
  png(file="Random_Forest_ROC_Curve.png")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  dev.off()
  
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}
