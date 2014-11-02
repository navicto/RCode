library(verification) #contains the brier score function
#explore data directory
data_directory = paste("C:/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
                       "Feature_selection/data_both_filter_False_allreports_True_logic_picktrue_NLP_MedLEE_missing2False_F,
                       /Evaluation/Post_processed",
                       sep = "")
data_directory = "C:/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/Predictions_NBayes/Reported_scenarios/feature_selection_Including1R/"
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
  print(names(prediction_data[i]))
  scores[names(prediction_data[i])] = list(verify(obs = prediction_data[[i]]$actual, 
                                                  pred = prediction_data[[i]]$NaiveBayes_prob_T))
}

for (BSS in scores){
  print(c(BSS$bs,BSS$ss))
}