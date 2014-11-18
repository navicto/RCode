#Purpose: Analyse/compute the distribution of features in readmission data
library(ggplot2)

filter_data <- function(data, by_type=NULL, by_index=NULL, by_name=NULL){
  #return a subset of data, filtered by either datatype, index, or column name
  if (!is.null(by_type)){
    attr_types = sapply(data, class) #data type by column (attr)
    type_match = (attr_types == by_type) #whether columns match type by_type
    keep_attrs = type_match[type_match == TRUE] #only attrs that match by_type 
    filtered_data = data[names(keep_attrs)] #data filtered by by_type
  }else if (!is.null(by_index)){
    filtered_data = data[,by_index]
  }else if (!is.null(by_name)){
    filtered_data = data[,by_name]
  }
  return(filtered_data)
}

binary_attr_freq <- function(data, sort=TRUE, invert=TRUE, normalize=TRUE){
  #returns a dataset with the [relative]frequencies of binary attributes in data
  attr_freq = data.frame(attr=names(data), freq=sapply(data, sum), row.names=NULL)
  if (sort){
    attr_order = order(attr_freq$freq, decreasing=T)
    attr_freq = attr_freq[attr_order,]
  }
  if (normalize){
    attr_freq$freq = attr_freq$freq / length(data[,1])
  }
  return(attr_freq)
}


#path to data
data_dir = "/Users/Victor/Desktop/"
data_file = paste(data_dir, "DRG_processed.csv", sep="")
data = read.csv(data_file)

#filter data by data types
numeric_data = filter_data(data=data, by_type="integer")
binary_data_pos = filter_data(data=subset(data, READMITTED==T), by_type="logical")
binary_data_neg = filter_data(data=subset(data, READMITTED==F), by_type="logical")

#relative frequencies of binary data
binary_freq_pos = binary_attr_freq(binary_data_pos, sort=T, invert=T, normalize=T)
binary_freq_neg = binary_attr_freq(binary_data_neg, sort=T, invert=T, normalize=T)
head(binary_freq_pos)
head(binary_freq_neg)

data_prime = data.frame(attr=NULL, value=NULL)
for (attr in names(numeric_data)){
  #print(head(numeric_data[attr]))
  data_prime = rbind(data_prime, data.frame(attr=attr, value=numeric_data[,attr]))
}
head(data_prime)

p = ggplot(data, aes(x=RISK_OF_MORTALITY, fill=READMITTED))
p + geom_histogram(position = "dodge", aes(y=..density..), binwidth=0.5)
