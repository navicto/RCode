#This R package has functions to analyze ROC curves, and provide AUROC analysis

auc_prog <- function(data_directory=NULL, auc_files=NULL, p_label="p_T", actual_label="actual", plot_prog=TRUE){
  require(gtools, quietly = T)
  require(pROC, quietly = T)
  # Get files with predictions (must be csv if we are using a data_directory instead of absolute paths)
  if (!is.null(auc_files)){
    file_paths = auc_files
  } else if (!is.null(data_directory)){
    file_paths = list.files(path = data_directory, pattern = "\\.csv$", full.names = T)
    file_paths = mixedsort(file_paths) #mixedsort also sorts based on numeric value ("50" comes before "100")
  } else {
    stop("Either data_directory or auc_files must be specified")
  }
  
  #get predictions data
  prediction_data = list()
  for(file_name in file_paths){
    prediction_data[file_name] = list(read.csv(file_name))
  }
  
  #Create a pROC object from each dataframe in prediction_data
  ROC_objects = list()
  for (i in 1:length(prediction_data)){
    ROC = roc(as.formula(paste(actual_label, "~", p_label)), prediction_data[[i]], plot=F, smooth=F, 
              ci=T, auc.polygon=T)
    roc_name = tail(strsplit(names(prediction_data[i]), split='/')[[1]], 1)
    ROC_objects[roc_name] = list(ROC)
  }
  
  #create vector with AUROCs of predictions data
  aurocs = c()
  for (i in 1:length(ROC_objects)){
    aurocs = c(aurocs, ROC_objects[[i]][["auc"]][1])
  }
  
  #Plot the AUROC as a function of the number of features used by the classifier
  if (plot_prog == TRUE){
    x_range = c(0, ceiling(1.3 * length(aurocs)))
    y_range = c(0.9*min(aurocs), 1.1*max(aurocs))
    if (y_range[2] > 1){ #limit y_range to 1
      y_range[2] = 1
    }
    y_values = c(0, aurocs) #values for y axis
    y_values
    x_values = 0:length(aurocs)
    x_values
    plot(x_values, y_values, xlim=x_range, ylim=y_range, type="l", xlab='N Features', ylab='AUROC', main='AUROC Progression')
    max_pos = order(aurocs, decreasing=T)[1] #index of highest AUROC
    max_value = max(aurocs) #highest AUROC
    points(x=max_pos, y=max_value, type="p", pch=19, col="red") #print a big red dot to highlight highest AUROC
    max_legend = paste("N features:", toString(max_pos), "\n", "AUROC:",
                       toString(round(max_value, digits=2)), sep = " ")
    identify(x=max_pos, y=max_value, labels=c(max_legend)) #for the legend to show up
    #you have to click on the red dot in the plot
  }
  return(list(ROC_objects, aurocs))
}

pairwise_test = function(ROC_objects, alternative_hyp){
  #Conducts pairwise statistical tests to test the significance of difference between
  #correlated AUROCs. 
  #input ROC_objects: series of ROC objects, containing the ROCs of each classifier
  #input alternative_hyp: either 'two-sided', 'greater', or 'lower'. AUROC pairs are
  #chosen in the order of ROC_objects. when testing the pair (ROC[1], ROC[2]), an
  #alternate_hyp of 'greater' means that the test will assess the hypothesis that
  #AUROC[2] > AUROC[1]
  
  #generate all possible pairs of ROC object names to create scenarios (tests)
  scenario_combs = combn(names(ROC_objects), 2, simplify = F)

  #pairwise testing
  pairwise_tests = matrix(data=NA, nrow=length(ROC_objects), ncol=length(ROC_objects), 
                          dimnames=list(names(ROC_objects), names(ROC_objects)))
  for (i in 1:length(scenario_combs)){
    test_object = roc.test(ROC_objects[[scenario_combs[[i]][1]]],
                           ROC_objects[[scenario_combs[[i]][2]]], method='delong',
                           alternative=alternative_hyp)
    #save test
    print(test_object)
    pairwise_tests[scenario_combs[[i]][1], scenario_combs[[i]][2]] = test_object$p.val
  }
  return(pairwise_tests)
}


###test auc_prog
p_label="p_T"; actual_label="actual"
path_predictions = "/Users/Victor/Desktop/Temp/Predictions/Reformatted/"
x = auc_prog(data_directory = path_predictions, p_label = p_label, actual_label = actual_label)

###test pairwise_test
y = pairwise_test(x[[1]], 'two.sided')
