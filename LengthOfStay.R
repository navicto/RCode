library(ggplot2)
data_path = 'C:/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/Discharge_times/'
data_cases = read.csv(paste(data_path, 'Positives.csv', sep=''))
data_controls = read.csv(paste(data_path, 'Negatives.csv', sep=''))
data_combined = read.csv(paste(data_path, 'Combined_singleColumn.csv', sep=''))

summary(data_cases)
summary(data_controls)

custom.quartile <- function(x){
  out <- quantile(x, probs = c(0.99))
  names(out) <- c("0.99q")
  return(out) 
}

#barplot:
p=ggplot(data_combined, aes(Type, Duration))
p=p+geom_boxplot(outlier.size=0, width=0.1)
p=p+ylab("ED visits duration")
p=p+xlab("Visit type")
p=p+theme_grey(base_size = 18)
p=p+coord_cartesian(ylim = c(0,10))
p=p+stat_summary(fun.y=custom.quartile,geom="point", size=3)
p=p+stat_summary(fun.y=mean,geom="point", size=3)
p