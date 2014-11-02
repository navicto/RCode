  #Confict distribution
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, row_height=NULL, col_width=NULL) {
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
    if(is.null(row_height)==TRUE & is.null(col_width)==TRUE){
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    }else if (is.null(row_height)==FALSE & is.null(col_width)==TRUE){
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout),heights = row_height)))
    }else if (is.null(row_height)==TRUE & is.null(col_width)==FALSE){
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout),widths =  col_width)))
    }else {
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout),heights = row_height,widths =  col_width)))
    }
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Extract Legend 
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

  hist_flu = function (x, prob, prob2 = NULL, prob3 = NULL, xlab = "x", xaxs.label = NULL, 
            yaxs.label = NULL, bar.width = NULL, freq = FALSE, prob.col = "blue", 
            prob2.col = "red", prob3.col = "gray", ...) 
  {
    if (!missing(x) && missing(prob)) {
      prob <- table(x)
      x <- sort(unique(x))
    }
    if (length(x) != length(prob)) {
      stop("Length of 'x' must be the same as the length of 'prob'")
    }
    if (!freq) {
      prob <- prob/sum(prob)
      prob2 <- prob2/sum(prob2)
      prob3 <- prob3/sum(prob3)
      ylab <- "ED visits (%)"
    }
    else {
      ylab <- "ED reports (%)"
    }
    if (is.numeric(x)) {
      x.values <- sort(unique(x))
      n.x.values <- length(x.values)
      if (is.null(bar.width)) {
        gaps <- x.values[2:n.x.values] - x.values[1:(n.x.values - 
                                                       1)]
        bar.width <- min(gaps) * 0.2
      }
      par(mar = c(3, 3, 4, 1), mgp = c(1.7, 0.5, 0), tck = -0.01)
      plot(range(x) + c(-2, 2) * bar.width, c(0, max(prob, 
                                                     prob2, prob3)), xlab = xlab, ylab = ylab, xaxs = "i", 
           xaxt = "n", yaxs = "i", yaxt = ifelse(is.null(yaxs.label), 
                                                 "s", "n"), bty = "l", type = "n", ...)
      if (is.null(xaxs.label)) {
        axis(1, x.values)
      }
      else {
        axis(1, xaxs.label[[1]], xaxs.label[[2]])
      }
    }
    else {
      x.values <- unique(x)
      n.x.values <- length(x.values)
      if (is.null(bar.width)) {
        bar.width <- 0.2
      }
      par(mar = c(3, 3, 4, 1), mgp = c(1.7, 0.5, 0), tck = -0.01)
      plot(c(1, n.x.values) + c(-2, 2) * bar.width, c(0, max(prob, 
                                                             prob2, prob3)), xlab = xlab, ylab = ylab, xaxs = "i", 
           xaxt = "n", yaxs = "i", yaxt = ifelse(is.null(yaxs.label), 
                                                 "s", "n"), bty = "l", type = "n", ...)
      if (is.null(xaxs.label)) {
        axis(1, 1:n.x.values, x.values)
      }
      else {
        axis(1, xaxs.label[[1]], xaxs.label[[2]])
      }
      x <- 1:length(x)
    }
    if (!is.null(yaxs.label)) {
      axis(2, yaxs.label[[1]], yaxs.label[[2]])
    }
    offset <- rep(0, 3)
    if (length(prob2) != 0 & length(prob3) != 0) {
      offset[1] <- -bar.width
      offset[2] <- 0
      offset[3] <- bar.width
    }
    if (length(prob2) > 0 & length(prob3) == 0) {
      offset[1] <- -bar.width/2
      offset[2] <- bar.width/2
      offset[3] <- 0
    }
    for (i in 1:length(x)) {
      polygon(x[i] + c(-1, -1, 1, 1) * bar.width/2 + offset[1], 
              c(0, prob[i], prob[i], 0), border = prob.col, col = prob.col)
      if (!is.null(prob2)) {
        polygon(x[i] + c(-1, -1, 1, 1) * bar.width/2 + offset[2], 
                c(0, prob2[i], prob2[i], 0), border = prob2.col, 
                col = prob2.col)
      }
      if (!is.null(prob3)) {
        polygon(x[i] + c(-1, -1, 1, 1) * bar.width/2 + offset[3], 
                c(0, prob3[i], prob3[i], 0), border = prob3.col, 
                col = prob3.col)
      }
    }
  }
  
  
  conflict_dist <-function(path, title){
    data_pos = read.table(paste(path, 'case_conflicts_pos.txt', sep='\\'))
    data_neg = read.table(paste(path, 'case_conflicts_neg.txt', sep='\\'))
    max_conf = max(data_pos[1,], data_neg[,1])
    intervals = c(0, seq(from=0.99, to=(max_conf+1), by=1))
    x = hist(as.numeric(data_pos[,1]), breaks=intervals, plot=FALSE)
    y = hist(as.numeric(data_neg[,1]), breaks=intervals, plot=FALSE)
    height = c(0, 100*max(x$density, y$density) + 5)
    hist_flu(round(intervals[1:length(intervals)-1]), prob=100*(x$density), 
                       prob2=100*(y$density), ylim=height, prob.col='brown1', 
                       prob2.col='deepskyblue1', xlab='N conflicts', main=title, freq=T)
    return(list(pos=data_pos,neg=data_neg))
  }
  
  layout(matrix(c(1,2,3,3), nrow=2, ncol=2, byrow=T), heights=c(1,0.3))
  layout.show(n=3)
  par(mar=c(4,4,0,1),oma=c(0,0,3,1))
  
  #path_allcodes = 'C:\\Users\\Victor\\Dropbox\\DBMI\\ResearchProject\\Analyses\\Conflicts\\MedLEE_allcodes_rawNLP_1Rep'
  #path_31codes = 'C:\\Users\\Victor\\Dropbox\\DBMI\\ResearchProject\\Analyses\\Conflicts\\MedLEE_31codes_rawNLP_1Rep'
  path_allcodes = 'C:\\Users\\Victor\\Dropbox\\DBMI\\ResearchProject\\Analyses\\Conflicts\\MedLEE_allcodes_rawNLP_multRep'
  path_31codes = 'C:\\Users\\Victor\\Dropbox\\DBMI\\ResearchProject\\Analyses\\Conflicts\\MedLEE_31codes_rawNLP_multRep'
#   path_allcodes = 'C:\\Users\\Victor\\Dropbox\\DBMI\\ResearchProject\\Analyses\\Conflicts\\MedLEE_allcodes_both'
#   path_31codes = 'C:\\Users\\Victor\\Dropbox\\DBMI\\ResearchProject\\Analyses\\Conflicts\\MedLEE_31codes_both'
  conf_31=conflict_dist(path_31codes, '(c) Intra-report, 31 findings')
  conf_all=conflict_dist(path_allcodes, '(d) Intra-report, all findings')
  
  mtext("Distribution of number of conflicts", side=3, line=0, outer=TRUE, cex=1, font=2)
  legend('topright', legend=c('Influenza cases', 'Non-Influenza controls'), 
  fill=c('skyblue4', 'gray'), bty='n')
  
  
  #with ggplot:
  library(ggplot2)
  font=17
  df_conf_31 = data.frame(nconf=conf_31$pos$V1,VisitType='Influenza')
  df_conf_31_control = data.frame(nconf=conf_31$neg$V1,VisitType='Non-Influenza')
  df_conf_all = data.frame(nconf=conf_all$pos$V1,VisitType='Influenza')
  df_conf_all_control = data.frame(nconf=conf_all$neg$V1,VisitType='Non-Influenza')
  df_31 = rbind(df_conf_31,df_conf_31_control)
  df_all = rbind(df_conf_all,df_conf_all_control)

  p_31 = ggplot(df_31, aes(x=nconf)) + 
    geom_histogram(aes(fill=VisitType,y=50*..density..), binwidth=0.5, position='dodge') +
    #scale_y_continuous(formatter = 'percent') +
    theme(axis.title=element_text(face='bold',size=as.character(font)),
          axis.title=element_text(face='bold', size=as.character(font)),
          axis.text=element_text(size=as.character(font-2)),
          legend.title=element_text(size=as.character(font)),
          legend.text=element_text(size=as.character(font-1)),
          legend.position='bottom', plot.title=element_text(face='bold')) +
    xlab('N conflicts per visit') + ylab('% ED Visits') + ggtitle('(a) 31 Findings')
  leg = g_legend(p_31)
  p_31 = p_31 + guides(fill=F, color=F)

  p_all = ggplot(df_all, aes(x=nconf)) + 
  geom_histogram(aes(fill=VisitType,y=50*..density..), binwidth=0.5, position='dodge') +
  #scale_y_continuous(formatter = 'percent') +
  theme(axis.title=element_text(face='bold',size=as.character(font)),
        axis.title=element_text(face='bold', size=as.character(font)),
        axis.text=element_text(size=as.character(font-2)),
        legend.title=element_text(size=as.character(font)),
        legend.text=element_text(size=as.character(font-1)),
        legend.position='bottom', plot.title=element_text(face='bold')) +
  xlab('N conflicts per visit') + ylab('% ED Visits') + ggtitle('(b) All findings')
  p_all = p_all + guides(color=F, fill=F)

win.metafile('C:/Users/Victor/Desktop/Conflicts_intra.wmf', width=10, height=5.5)
multiplot(p_31, p_all, cols=2)
dev.off()

win.metafile('C:/Users/Victor/Desktop/Conflicts_legend.wmf', width=10, height=5.5)
grid.draw(leg)
dev.off()

x=subset(df_all, nconf==0 & VisitType=='Non-Influenza')
length(x$nconf)/length(df_all[df_all$VisitType=='Non-Influenza',]$VisitType)
  