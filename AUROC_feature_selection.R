
#This script reads csv files containing weka's prediction output, it returns pROC
#objects that do provide statistical analysis (CI, etc), and the progression in 
#AUROC as the classifier uses more features
#By: Victor Ruiz
library(pROC)
library(ggplot2)
#par(mfrow=c(1,2))
auc_prog <- function(data_directory, plot_prog){
  # 1.csv
  file_paths = list.files(path = data_directory, pattern = "\\.csv$", full.names = F)
  #get predictions data
  prediction_data = list()
  for(file_name in file_paths){
    file_path = paste(data_directory, file_name, sep = "/")
    prediction_data[file_name] = list(read.csv(file_path))
  }
  #pROC objects, in a list indexed by scneario(file name)
  ROC_objects = list()
  for (i in 1:length(prediction_data)){
    ROC = roc(actual ~ p_T, prediction_data[[i]], plot=F, smooth=F, 
              ci=T, auc.polygon=T)
    ROC_objects[names(prediction_data[i])] = list(ROC)
  }
  
  aurocs = c()
  for (i in 1:length(ROC_objects)){
    file_name = names(ROC_objects[i])
    file_name = unlist(strsplit(file_name, ""))
    index_pos = grep(pattern="\\d", file_name)
    index = paste(file_name[index_pos], collapse="")
    index = as.integer(index)
    aurocs[index] = ROC_objects[[i]][["auc"]][1]
  }
  
  #Plot the AUROC as a function of the number of features used by the classifier
  if (plot_prog == TRUE){
  plot(aurocs, ylim=c(0.5, 1), type="l", xlab='N Features', ylab='AUROC',main='AUROC progression vs. Model size')
  max_pos = order(aurocs, decreasing=T)[1]
  max_value = max(aurocs)
  points(x=max_pos, y=max_value, type="p", pch=19, col="red")
  max_legend = paste("N features:", toString(max_pos), "\n", "AUROC:",
                     substr(toString(max_value), start=1, stop=6), sep = " ")
  identify(x=max_pos, y=max_value, labels=c(max_legend)) #for the legend to show up
  #you have to click on the red dot in the plot
  }
  return(list(ROC_objects, aurocs))
}

min_features <- function(auc_progression, best_model, significance){ #best_model should be integer index
  valid_subsets = c()
  p_vals = c()
  for(i in best_model:2){
    model = paste(toString(best_model), 'csv', sep='.')
    previous_model = paste(toString(i-1), 'csv', sep='.')
    print(c(model, previous_model))
    pval = roc.test(auc_progression[[1]][[model]], auc_progression[[1]][[previous_model]], method='delong')$p.val
    p_vals = c(p_vals, pval)
    if(pval > significance){
      valid_subsets = c(valid_subsets, c(previous_model))
    }
  }
  return(list(valid_subsets, p_vals))
}

auc_increase <- function(auc_prog){
  last_greedy = 0
  last_cons = 0
  conservative = c()
  greedy = c()
  for(i in 1:length(auc_prog)){
    if(auc_prog[i] > last_greedy){
      greedy = c(greedy, i)
      conservative = c(conservative, i)
      last_greedy = auc_prog[i]
    } else if(auc_prog[i] > last_cons){
      conservative = c(conservative, i)
    }
    last_cons = auc_prog[i]
  }
  return(list(greedy, conservative))
}

#main()
predictions_DRG = "D://ResearchData//Readmission_AllCause//FeatureSelection//DRG//Predictions//formatted//"

significance = 0.05

auc_DRG = auc_prog(predictions_DRG, TRUE)
best_DRG = order(auc_DRG[[2]], decreasing=TRUE)[1]
print(c('Best performance with DRG data: ', best_DRG))
auc_DRG[[1]][paste(toString(best_DRG), '.csv', sep='')]
valid_subsets_DRG = min_features(auc_DRG, best_DRG, significance)
excluding_DRG = auc_increase(auc_DRG[[2]])

df = data.frame(AUROC=auc_DRG[[2]], Index = 1:length(auc_DRG[[2]]))
ggplot(df, aes(Index, AUROC)) + geom_line(stat='identity') +
  xlab('Classifier size (N Features)') + ggtitle('Incremental model AUROC') +
  theme(plot.title=element_text(face='bold'))