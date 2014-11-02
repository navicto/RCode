library(ggplot2)
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

positions <- function(df){
  pos_2rep = (cumsum(subset(df,nrep==2)$prog) - 0.5*subset(df,nrep==2)$prog)*100
  print(pos_2rep)
  pos_3rep = (cumsum(subset(df,nrep==3)$prog) - 0.5*subset(df,nrep==3)$prog)*100
  print(pos_3rep)
  pos_4rep = (cumsum(subset(df,nrep==4)$prog) - 0.5*subset(df,nrep==4)$prog)*100
  pos = c(pos_2rep,pos_3rep,pos_4rep)
  return(pos)
}

prog_all_2 = data.frame(prog=c(0.628,0.371),nrep='2',features='All possible findings')
prog_all_3 = data.frame(prog=c(0.513,0.361,0.125),nrep='3',features='All possible findings')
prog_all_4 = data.frame(prog=c(0.562,0.266,0.132,0.037),nrep='4',features='All possible findings')
prog_31_2 = data.frame(prog=c(0.707,0.292),nrep='2',features='31 expert-defined findings')
prog_31_3 = data.frame(prog=c(0.626,0.276,0.096),nrep='3',features='31 expert-defined findings')
prog_31_4 = data.frame(prog=c(0.628,0.198,0.127,0.044),nrep='4',features='31 expert-defined findings')

df_all = rbind(prog_all_2,prog_all_3,prog_all_4)
df_all$Report=c('First','Second','First','Second','Third','First','Second','Third','Fourth')
df_all$pos = positions(df_all)
df_all$lab = paste(as.character(df_all$prog*100),'',sep='%')
df_31 = rbind(prog_31_2,prog_31_3,prog_31_4)
df_31$Report=c('First','Second','First','Second','Third','First','Second','Third','Fourth')
df_31$pos = positions(df_31)
df_31$lab = paste(as.character(df_31$prog*100),'',sep='%')

generate_plot <- function(df, title, legend_pos, title_size, percent_size){
  p = ggplot(df, aes(nrep,100*prog,fill=Report))
  p = p + geom_bar(stat='identity', color='white') +
    theme(axis.title=element_text(face="bold",size=as.character(title_size)),
          plot.title=element_text(lineheight=0.8,face="bold",size=as.character(title_size)),
          legend.position=legend_pos, legend.text=element_text(size=title_size),
          legend.title=element_text(size=title_size)) + 
    xlab('Reports per case') + ylab('Unique findings (%)') + 
    scale_fill_brewer(palette = "Paired") + 
    ggtitle(title) +
    geom_text(aes(label = lab, y = pos), size = percent_size)
  return(p)
}

title_size = 17; percent_size = 5; legend_pos = 'bottom'
p_all = generate_plot(df = df_all, title='(b) All possible findings\n', 
                      legend_pos = legend_pos, title_size = title_size, 
                      percent_size = percent_size)
p_31 = generate_plot(df = df_31, title = '(a) 31 Expert-defined findings\n', 
                      legend_pos = legend_pos, title_size = title_size, 
                     percent_size = percent_size)

graph_legend = g_legend(p_31)
p_31 = p_31 + guides(color=F, fill=F)
p_all = p_all + guides(color=F, fill=F)

win.metafile(file='C:/Users//Victor/Desktop/code_prog.wmf', width = 10, height = 5.5)
multiplot(p_31,p_all,cols=2)
dev.off()
win.metafile(file='C:/Users//Victor/Desktop/code_prog_leg.wmf')
grid.draw(graph_legend)
dev.off()

