get_quantiles <- function(probabilities, nQuantiles = 10){
  #Returns nQuantiles from a vector of probabilities
  step = 1 / nQuantiles
  cuts = seq(from = 0, to = 1, by = step)
  return(as.numeric(quantile(x = probabilities, probs = cuts)))
}

contingency <- function(data, threshold, class_label, T_value, F_value, prediction_label){
  positives = data[data[prediction_label] >= threshold ,]
  negatives = data[data[prediction_label] < threshold ,]
  TP = 0; FP = 0; TN = 0; FN = 0 #in case data.frame is empty
  if(nrow(positives) != 0){
    TP = nrow(positives[positives[class_label] == T_value ,])
    FP = nrow(positives) - TP    
  }
  if(nrow(negatives) != 0){
    TN = nrow(negatives[negatives[class_label] == F_value ,])
    FN = nrow(negatives) - TN
  }
    
  sensitivity = TP / (TP + FN)
  specificity = TN / (TN + FP)
  
  return(data.frame(threshold = threshold, TP = TP, FP = FP, TN = TN, FN = FN, 
                    sensitivity = sensitivity, specificity = specificity))
}

operating_points <- function(data, n_bins, class_label, T_value, F_value, prediction_label){
  cut_points = get_quantiles(probabilities = data[[prediction_label]], nQuantiles = n_bins)
  op_points = data.frame(threshold = numeric(), TP = integer(), FP = integer(), TN = integer(), 
                         FN = integer(), sensitivity = numeric(), specificity = numeric())
  #get operating point per cut point
  for(cut in cut_points){
    op_points = rbind(op_points,
                      contingency(data, cut, class_label, T_value, F_value, prediction_label))
  }
  return(op_points)
}

#test instance
data_path = '/Users//Victor/Desktop/Temp/Predictions/Reformatted/8_demog_lab_meds_vitals_nurse_nlp_los_dx_reformatted.csv'
data = read.csv(data_path)
class_label = 'actual'
prediction_label = 'p_T'
T_value = T
F_value = F
n_bins = 100

x = operating_points(data, n_bins, class_label, T_value, F_value, prediction_label)

