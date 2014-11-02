library(verification) #contains the brier score function
#explore data directory
data_directory = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
                       "ED_flu_dataset/Predictions_NBayes/aggregate_dataset",
                       sep = "")
data_directory = "C:\\Users\\Victor\\Dropbox\\DBMI\\ResearchProject\\Analyses\\ED_flu_dataset\\Predictions_NBayes\\aggregate_dataset" 
data_directory = "C:/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/ED_flu_dataset/Predictions_NBayes/aggregate_dataset" 

file_paths = list.files(path = data_directory, pattern = "\\.csv$", full.names = F)
#read predictions data (predictions in csv files)
prediction_data = list()
for(file_name in file_paths){
  file_path = paste(data_directory, file_name, sep = "/")
  prediction_data[file_name] = list(read.csv(file_path))
}
#compute BS and BSS
scores = list()
for (i in 1:length(prediction_data)){
  scores[names(prediction_data[i])] = list(verify(obs = prediction_data[[i]]$actual, 
                                             pred = prediction_data[[i]]$NaiveBayes_prob_T))
}

for (i in 1:length(scores)){
  print(names(scores[i]))
  summary(scores[[i]])
}


data_path = "/Users/Victor/Desktop/output.csv"
relation = actual ~ prediction
prediction_data = read.csv(data_path)
prediction_data["actual"] = as.integer(prediction_data[["actual"]] == "1:T")
prediction_data["predicted"] = (prediction_data[["predicted"]] == "1:T")
prediction_data["prediction"] = abs(-1*(!prediction_data[["predicted"]]) + prediction_data[["prediction"]])
N = dim(prediction_data)[1]
#Computing it just with vectorial operations
BS = (1/N)*sum((prediction_data[["prediction"]] - prediction_data[["actual"]])**2)
BS
#Using the scoring library (not sure what else it's capable of...)
BS2 = mean(brierscore(relation, data = prediction_data))
BS2
#BSS (Brier skill sore)
library(verification)
BSS = verify(obs = prediction_data$actual, pred = prediction_data$prediction)
summary(BSS)

