#Multiplot function, multiple plots in the same window:
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
} 
#take dataset and convert columns col_names to time datatype
str2time <- function(data, col_names, date_form){
  data_copy = data
  for (name in col_names){
    data_copy[[name]] = strptime(x = data[[name]], format = date_form)
  }
  return(data_copy)
}

#add time_delta column to a dataset that contains time columns
add_delta <- function(data, col_names, delta_name){
  data_copy = data
  data_copy[[delta_name]] = data[[col_names[1]]] - data[[col_names[[2]]]]
  return(data_copy)
}

#get received time of a report, given all share the same case ID
get_rcvd <- function(data, seq){
  report = data[data$SEQ == seq,]
  if(nrow(report) == 0){
    return(NA)
  }else{
    return(report$DATE_RECEIVED[1])
  }
}

#convert timediff to numeric in hours
timediff_format <- function(delta, format){
  if(is.na(delta)){
    return(NA)
  }
  delta_copy = delta
  units(delta_copy) = format
  return(as.numeric(delta_copy))
}

#get admisison and discharge time for cases in data set
get_times <- function(data){
  times = data.frame(ID = NA, DATE_ADMITTED = NA, DATE_DISCHARGED = NA,
                     DURATION = NA, DELTA_1 = NA, DELTA_2 = NA, DELTA_3 = NA,
                     DELTA_4 = NA)[numeric(0),]
  row_index = 1
  for(id in unique(data$ID)){
    reports = subset(data, ID == id)
    admission = min(reports$DATE_ADMITTED)
    #print(admission)
    discharge = max(reports$DATE_DISCHARGED)
    #print(discharge)
    duration = discharge - admission
    #units(duration) = "hours"
    delta_1 = timediff_format(discharge - get_rcvd(data = reports, seq = 1), "hours")
    #print(delta_1)
    delta_2 = timediff_format(discharge - get_rcvd(data = reports, seq = 2), "hours")
    #print(delta_2)
    delta_3 = timediff_format(discharge - get_rcvd(data = reports, seq = 3), "hours")
    #print(delta_3)
    delta_4 = timediff_format(discharge - get_rcvd(data = reports, seq = 4), "hours")
    #print(delta_4)
    #print(duration)
    times[row_index, 'ID'] = id
    times[row_index, 'DATE_ADMITTED'] = as.POSIXct(admission)
    times[row_index, 'DATE_DISCHARGED'] = as.POSIXct(discharge)
    times[row_index, 'DURATION'] = duration
    times[row_index, 'DELTA_1'] = delta_1
    times[row_index, 'DELTA_2'] = delta_2
    times[row_index, 'DELTA_3'] = delta_3
    times[row_index, 'DELTA_4'] = delta_4
    row_index = row_index + 1
    
  }
  return(times)
}

#% of ED reports available vs. time waited after discharge
wait4reports <- function(deltas, offsets){
  #remove NAs from deltas
  df_deltas = deltas[complete.cases(deltas),]
  availability = c()
  for(offset in offsets){
    available = (df_deltas$Delta + offset) > 0
    availability = c(availability, mean(available)*100)
  }
  return(availability)
}


data_dir = paste('C://Users/Victor/Dropbox/DBMI/ResearchProject/Data/',
                 'ED_flu_dataset/time_stamps/prelims/Discharge_data/',
                 sep='')
out_dir = paste('C://Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/',
                 'Discharge_times/',
                 sep='')
#load data
positives = read.csv(paste(data_dir, 'Positives_reformated.csv', sep=''))
negatives = read.csv(paste(data_dir, 'Negatives_reformated.csv', sep=''))

#convert dates from string to time objects
date_format = "%Y-%m-%d %H:%M:%S"
cols = c('DATE_ADMITTED', 'DATE_DISCHARGED', 'DATE_RECEIVED')
positives = str2time(data = positives, col_names = cols, date_form = date_format)
negatives = str2time(data = negatives, col_names = cols , date_form = date_format)

#compute time deltas
length_of_stay = c('DATE_DISCHARGED', 'DATE_ADMITTED')
discharge_delta = c('DATE_DISCHARGED', 'DATE_RECEIVED')
positives = add_delta(data = positives, col_names = length_of_stay, delta_name = 'DURATION')
negatives = add_delta(data = negatives, col_names = length_of_stay, delta_name = 'DURATION')
positives = add_delta(data = positives, col_names = discharge_delta, delta_name = 'DELTA')
negatives = add_delta(data = negatives, col_names = discharge_delta, delta_name = 'DELTA')

#save data as csv files
write.csv(file = paste(out_dir, 'Positives.csv'), x = positives)
write.csv(file = paste(out_dir, 'Negatives.csv'), x = negatives)

#Compute length of stay, and time delta between discharge and availability of 1st-4th report
times = get_times(positives)
times_neg = get_times(negatives)[1 : (9 * length(times$DELTA_1)),]
times = rbind(times, times_neg)
font = 17
#plot deltas
df_1st = data.frame(Delta = times$DELTA_1, Report = 'First')
df_2nd = data.frame(Delta = times$DELTA_2, Report = 'Second')
df_3rd = data.frame(Delta = times$DELTA_3, Report = 'Third')
#df_4th = data.frame(Delta = times$DELTA_4, Report = 'Fourth')
df_deltas = rbind(df_1st, df_2nd, df_3rd)
df_deltas = df_deltas[complete.cases(df_deltas),]
h = ggplot(df_deltas, aes(Delta, colour = Report, fill = Report))
h = h + geom_histogram(binwidth=3)
h = h + geom_density(alpha = 0.2, size=1)
h = h + coord_cartesian(xlim = c(-100, 75))
h = h + geom_vline(xintercept = c(0), linetype = 'solid')
h = h + xlab('Hours (Discharge - Report available)')
h = h + ylab('% ED visits')
h = h + theme(axis.title=element_text(face="bold",size=as.character(font)),
              axis.text=element_text(size=as.character(font-2)),
              plot.title = element_text(face="bold",size=as.character(font)),
              legend.title=element_text(face="bold",size=as.character(font)),
              legend.text=element_text(face="bold",size=as.character(font-1)),
              legend.position='bottom')
leg = g_legend(h)
h = h + guides(color=F, fill=F)

#plot availability vs. waiting
wait4 = seq(1, 24, 1/60)
av_1st = data.frame(wait = wait4, Availability = wait4reports(df_1st, wait4), Report = 'First')
av_2nd = data.frame(wait = wait4, Availability = wait4reports(df_2nd, wait4), Report = 'Second')
av_3rd = data.frame(wait = wait4, Availability = wait4reports(df_3rd, wait4), Report = 'Third')
#av_4th = data.frame(wait = wait4, Availability = wait4reports(df_4th, wait4), Report = 'Fourth')
availability = rbind(av_1st, av_2nd, av_3rd)
p = ggplot(availability, aes(x = wait, y = Availability, colour = Report)) +
  geom_line()
p = p + xlab('Time after patient discharge (Hours)') + 
  ylab('Reports available before discharge (%)')
p = p + theme(axis.title=element_text(face="bold",size=as.character(font)),
              axis.text=element_text(size=as.character(font-2)),
              plot.title = element_text(face="bold",size=as.character(font)),
              legend.title=element_text(face="bold",size=as.character(font)),
              legend.text=element_text(face="bold",size=as.character(font-1)),
              legend.position='bottom')
p = p + guides(color=F,fill=F)

win.metafile('C:/Users/Victor/Desktop/DischargeTimes.wmf', width=10, height=5.5)
multiplot(h,p,cols=2)
dev.off()
win.metafile('C:/Users/Victor/Desktop/DischargeTimesLegend.wmf', width=10, height=5.5)
grid.draw(leg)
dev.off()
