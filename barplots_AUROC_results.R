library(ggplot2)
library(gridExtra)
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
    if(is.null(row_height)==FALSE & is.null(col_width)==FALSE){
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    }else if (is.null(row_height)==TRUE & is.null(col_width)==FALSE){
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout),heights = row_height)))
    }else if (is.null(row_height)==FALSE & is.null(col_width)==TRUE){
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

#results of multiple vs single reports
#NLP=MedLEE
results=data.frame(AUROC=c(0.883,0.911,0.913,0.865,0.899,0.901),
           Reports=c('Single','Multiple + "Pick true"','Multiple + "Most recent"',
                     'Single','Multiple + "Pick true"','Multiple + "Most recent"'),
           Classifier=c('BN-EM-Topaz','BN-EM-Topaz','BN-EM-Topaz','BN-EM-MedLEE',
                        'BN-EM-MedLEE','BN-EM-MedLEE'))
b_medlee=ggplot(results, aes(Classifier,AUROC))
b_medlee = b_medlee + geom_bar(stat='identity', aes(fill=rev(Reports)), position='dodge')
b_medlee = b_medlee + guides(color=F, fill=F)

#NLP=Topaz
results=data.frame(AUROC=c(0.883,0.907,0.91,0.87,0.898,0.899),
                   Reports=c('Single','Multiple + "Pick true"','Multiple + "Most recent"',
                             'Single','Multiple + "Pick true"','Multiple + "Most recent"'),
                   Classifier=c('BN-EM-Topaz','BN-EM-Topaz','BN-EM-Topaz','BN-EM-MedLEE',
                                'BN-EM-MedLEE','BN-EM-MedLEE'))
b_topaz=ggplot(results, aes(Classifier,AUROC))
b_topaz = b_topaz + geom_bar(stat='identity', aes(fill=rev(Reports)), position='dodge')
leg = g_legend(b_topaz)
b_topaz = b_topaz + guides(color=F, fill=F)
b_topaz = b_topaz + coord_cartesian(ylim=c(0.5,0.92))

#plot both graphs
#multiplot(b_medlee, b_topaz, cols=2)
#grid.draw(leg)

#function to insert a break in the y axis
axis_break <- function(ggplot_obj, range_top, range_bottom){
  plot_top = ggplot_obj + coord_cartesian(ylim=range_top) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank())
  plot_bottom = ggplot_obj + coord_cartesian(ylim=range_bottom) +
    theme(axis.title.y=element_text(colour='white')) + 
    scale_y_continuous(breaks = seq(0,max(range_bottom),by = max(range_bottom)))
  return(list(plot_top, plot_bottom))
}

medlee_break = axis_break(ggplot_obj = b_medlee, range_top = c(0.7,1), range_bottom = c(0,0.1))
topaz_break = axis_break(ggplot_obj = b_topaz, range_top = c(0.7,1), range_bottom = c(0,0.1))
multiplot(medlee_break[[1]], medlee_break[[2]], topaz_break[[1]], topaz_break[[2]],cols=2,row_height = c(1,1/3))
