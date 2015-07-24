# Analysis of ROC curves
#By: Victor Ruiz
#Two chunks of codes:
# 1)Just plot ROC curves
# 2)Compute CI of AUROC curves, and test if differences in AUROCs are significant
library(pROC)
library(ggplot2)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, row_height=NULL, col_width=NULL) {
  require(grid)
  #plot multiple graphs on a grid
  
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


#----------------------------------Using pROC----------------------------------------
#This script reads csv files containing weka's prediction output, it returns pROC
#objects that do provide statistical analysis (CI, etc)
#for a sample of the CSV file see: 32codes_allreports_first.arff.csv
# data_directory = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
#                        "ED_flu_dataset/Predictions_NBayes/aggregate_dataset",
#                        sep = "") #folder with the CSV files
data_directory = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
                       "Predictions_NBayes/Reported_scenarios/",
                       "PrelimsVsFinals/mapped/",
                       sep = "") #folder with the CSV files
# data_directory = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
#                        "Predictions_NBayes/Reported_scenarios/",
#                        "PrelimsVsFinals/mapped/9to1Controls/",
#                        sep = "") #folder with the CSV files
# data_directory = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
#                        "Predictions_NBayes/Reported_scenarios/",
#                        "feature_selection_Including1R/",
#                        sep = "") #folder with the CSV files
# data_directory = paste("C:/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
#                        "Predictions_NBayes/Reported_scenarios/",
#                        "31findings_bothNLP/Evaluation_Poster/",
#                        sep = "") #folder with the CSV files
# data_directory = paste("/Users/Victor/Dropbox/DBMI/ResearchProject/Analyses/",
#                        "Predictions_NBayes/Reported_scenarios/",
#                        "31findings_bothNLP/Evaluation/9_to_1_controls/",
#                        sep = "") #folder with the CSV files
scenario_id_len = 4 #number of digits that proceed prediction files

file_paths = list.files(path = data_directory, pattern = "\\.csv$", full.names = F)
#get predictions data
prediction_data = list()
for(file_name in file_paths){
  file_path = paste(data_directory, file_name, sep = "/")
  prediction_data[file_name] = list(read.csv(file_path))
}
#pROC objects, in a list indexed by scenaario(file name)
ROC_objects = list()
for (i in 1:length(prediction_data)){
  #It's gonna plot all curves, not very useful but I include the sample in case
  #I need to plot just one I have a sample...
  ROC = roc(actual ~ NaiveBayes_prob_T, prediction_data[[i]], plot=F, smooth=F, 
            ci=T, auc.polygon=T)
  ROC_objects[names(prediction_data[i])] = list(ROC)
}
#generate all combns of scnearios for pair-wise testing
pairwise_significance = function(ROC_objects, alternative_hyp){
  scenario_combs = combn(names(ROC_objects), 2, simplify = F)
  scenario_names = c()
  for(name in names(ROC_objects)){scenario_names = c(scenario_names, substr(name, 1, scenario_id_len))}
  #pairwise testing
  pairwise_tests = matrix(data=NA, nrow=length(ROC_objects), ncol=length(ROC_objects), 
                          dimnames=list(scenario_names, scenario_names))
  for (i in 1:length(scenario_combs)){
    test_object = roc.test(ROC_objects[[scenario_combs[[i]][1]]],
                           ROC_objects[[scenario_combs[[i]][2]]], method='delong',
                           alternative=alternative_hyp)
    #test_object
    #test_index = paste(scenario_combs[[i]], collapse = " VS. ")
    #pairwise_tests[test_index] = list(test_object)
    pairwise_tests[substr(scenario_combs[[i]][1], 1, scenario_id_len), 
                   substr(scenario_combs[[i]][2], 1, scenario_id_len)] = test_object$p.val
  }
  return(pairwise_tests)
}
tests_twosided = pairwise_significance(ROC_objects, 'two.sided')
tests_greater = pairwise_significance(ROC_objects, 'greater')
tests_less = pairwise_significance(ROC_objects, 'less')

for (i in 1:length(ROC_objects)){
  print(paste(names(ROC_objects[i]), ROC_objects[[i]]$auc, sep=' '))
}

for (i in 1:length(ROC_objects)){
  print(c(substr(names(ROC_objects[i])[1], 1,scenario_id_len), ROC_objects[[i]]$auc[1]))
  print(ROC_objects[[i]]$ci)
}

#--------------------------------------------------------------------------------------------
plot_featureSelection = FALSE
plot_FinalvsPrelim = FALSE
plot_AUROC_finals = FALSE
path_plots = 'C:/Users/Victor/Desktop/'

#-------------------------Plot - AUROCs for final reports------------------------------------
if(plot_AUROC_finals == TRUE){
  #NLP:MedLEE
  BNT_single_med = data.frame(SENSITIVITY=ROC_objects[[1]]$sensitivities, 
                              SPECIFICITY=1-ROC_objects[[1]]$specificities, 
                              Experiment='1 Report/visit')
  BNT_mult_pick_med = data.frame(SENSITIVITY=ROC_objects[[4]]$sensitivities, 
                              SPECIFICITY=1-ROC_objects[[4]]$specificities, 
                              Experiment='Multiple reports + Pick true')
  BNT_mult_most_med = data.frame(SENSITIVITY=ROC_objects[[7]]$sensitivities, 
                              SPECIFICITY=1-ROC_objects[[7]]$specificities, 
                              Experiment='Multiple reports + Most recent')
  BNM_single_med = data.frame(SENSITIVITY=ROC_objects[[2]]$sensitivities, 
                              SPECIFICITY=1-ROC_objects[[2]]$specificities, 
                              Experiment='1 Report/visit')
  BNM_mult_pick_med = data.frame(SENSITIVITY=ROC_objects[[5]]$sensitivities, 
                                 SPECIFICITY=1-ROC_objects[[5]]$specificities, 
                                 Experiment='Multiple reports + Pick true')
  BNM_mult_most_med = data.frame(SENSITIVITY=ROC_objects[[8]]$sensitivities, 
                                 SPECIFICITY=1-ROC_objects[[8]]$specificities, 
                                 Experiment='Multiple reports + Most recent')
  BNF_single_med = data.frame(SENSITIVITY=ROC_objects[[3]]$sensitivities, 
                              SPECIFICITY=1-ROC_objects[[3]]$specificities, 
                              Experiment='1 Report/visit')
  BNF_mult_pick_med = data.frame(SENSITIVITY=ROC_objects[[6]]$sensitivities, 
                                 SPECIFICITY=1-ROC_objects[[6]]$specificities, 
                                 Experiment='Multiple reports + Pick true')
  BNF_mult_most_med = data.frame(SENSITIVITY=ROC_objects[[9]]$sensitivities, 
                                 SPECIFICITY=1-ROC_objects[[9]]$specificities, 
                                 Experiment='Multiple reports + Most recent')
  #NLP:Topaz
  BNT_single_top = data.frame(SENSITIVITY=ROC_objects[[7]]$sensitivities, 
                              SPECIFICITY=1-ROC_objects[[7]]$specificities, 
                              Experiment='1 Report/visit')
  BNT_mult_pick_top = data.frame(SENSITIVITY=ROC_objects[[9]]$sensitivities, 
                                 SPECIFICITY=1-ROC_objects[[9]]$specificities, 
                                 Experiment='Multiple reports + Pick true')
  BNT_mult_most_top = data.frame(SENSITIVITY=ROC_objects[[10]]$sensitivities, 
                                 SPECIFICITY=1-ROC_objects[[10]]$specificities, 
                                 Experiment='Multiple reports + Most recent')
  BNM_single_top = data.frame(SENSITIVITY=ROC_objects[[8]]$sensitivities, 
                              SPECIFICITY=1-ROC_objects[[8]]$specificities, 
                              Experiment='1 Report/visit')
  BNM_mult_pick_top = data.frame(SENSITIVITY=ROC_objects[[11]]$sensitivities, 
                                 SPECIFICITY=1-ROC_objects[[11]]$specificities, 
                                 Experiment='Multiple reports + Pick true')
  BNM_mult_most_top = data.frame(SENSITIVITY=ROC_objects[[12]]$sensitivities, 
                                 SPECIFICITY=1-ROC_objects[[12]]$specificities, 
                                 Experiment='Multiple reports + Most recent')
  
  generate_plot <- function(df, title, legend_pos, title_size){
    p =ggplot(df) + geom_line(aes(SPECIFICITY, SENSITIVITY, color=Experiment), stat='Identity') +
      theme(axis.title=element_text(face="bold",size=as.character(title_size)),
            plot.title=element_text(lineheight=0.8,face="bold",size=as.character(title_size)),
            legend.position=legend_pos, legend.text=element_text(size=title_size),
            legend.title=element_text(size=title_size)) + 
      xlab('1-Specificity') + ylab('Sensitivity') + 
      #scale_fill_brewer(palette = "Paired") + 
      ggtitle(title) #+ theme(legend.key.size=unit(2,"cm"))
    leg=g_legend(p)
    p = p + guides(color=F, fill=T)
    return(list(p=p,leg=leg))
  }
  generate_plot_white_y <- function(df, title, legend_pos, title_size){
    p =ggplot(df) + geom_line(aes(SPECIFICITY, SENSITIVITY, color=Experiment), stat='Identity') +
      theme(axis.title.y=element_text(face="bold",size=as.character(title_size),colour = 'white'),
            axis.title=element_text(face="bold",size=as.character(title_size)),
            plot.title=element_text(lineheight=0.8,face="bold",size=as.character(title_size)),
            legend.position=legend_pos, legend.text=element_text(size=title_size),
            legend.title=element_text(size=title_size)) + 
      xlab('1-Specificity') + ylab('Sensitivity') + 
      #scale_fill_brewer(palette = "Paired") + 
      ggtitle(title) #+ theme(legend.key.size=unit(2,"cm"))
    leg=g_legend(p)
    p = p + guides(color=F, fill=T)
    return(list(p=p,leg=leg))
  }
  generate_plot_white_x <- function(df, title, legend_pos, title_size){
    p =ggplot(df) + geom_line(aes(SPECIFICITY, SENSITIVITY, color=Experiment), stat='Identity') +
      theme(axis.title.x=element_text(face="bold",size=as.character(title_size),colour = 'white'),
            axis.title=element_text(face="bold",size=as.character(title_size)),
            plot.title=element_text(lineheight=0.8,face="bold",size=as.character(title_size)),
            legend.position=legend_pos, legend.text=element_text(size=title_size),
            legend.title=element_text(size=title_size)) + 
      xlab('1-Specificity') + ylab('Sensitivity') + 
      #scale_fill_brewer(palette = "Paired") + 
      ggtitle(title) #+ theme(legend.key.size=unit(2,"cm"))
    leg=g_legend(p)
    p = p + guides(color=F, fill=T)
    return(list(p=p,leg=leg))
  }
  generate_plot_white_both <- function(df, title, legend_pos, title_size){
    p =ggplot(df) + geom_line(aes(SPECIFICITY, SENSITIVITY, color=Experiment), stat='Identity') +
      theme(axis.title=element_text(face="bold",size=as.character(title_size),colour = 'white'),
            plot.title=element_text(lineheight=0.8,face="bold",size=as.character(title_size)),
            legend.position=legend_pos, legend.text=element_text(size=title_size),
            legend.title=element_text(size=title_size)) + 
      xlab('1-Specificity') + ylab('Sensitivity') + 
      #scale_fill_brewer(palette = "Paired") + 
      ggtitle(title) #+ theme(legend.key.size=unit(2,"cm"))
    leg=g_legend(p)
    p = p + guides(color=F, fill=T)
    return(list(p=p,leg=leg))
  }
  
  #join roc data
  df_BNM_med = rbind(BNM_single_med, BNM_mult_pick_med, BNM_mult_most_med)
  df_BNT_med = rbind(BNT_single_med, BNT_mult_pick_med, BNT_mult_most_med)
  df_BNF_med = rbind(BNF_single_med, BNF_mult_pick_med, BNF_mult_most_med)
  df_BNM_top = rbind(BNM_single_top, BNM_mult_pick_top, BNM_mult_most_top)
  df_BNT_top = rbind(BNT_single_top, BNT_mult_pick_top, BNT_mult_most_top)
  #create ggplots
  title_size = 17; legend_pos = 'bottom'
  
  p_BNM_med = generate_plot_white_x(df = df_BNM_med, title = 'BN-EM-MedLEE',
                            legend_pos = legend_pos, title_size = title_size)
  p_BNT_med = generate_plot_white_y(df = df_BNT_med, title = 'BN-EM-Topaz',
                            legend_pos = legend_pos, title_size = title_size)
  p_BNF_med = generate_plot_white(df = df_BNF_med, title = 'BN-FeatureSelection',
                            legend_pos = legend_pos, title_size = title_size)
  win.metafile(file='C:/Users/Victor/Desktop/NLP_MedLEE.wmf', width = 10, height = 4)
  multiplot(p_BNM_med$p,p_BNT_med$p,p_BNF_med$p,cols=3)
  dev.off()
  win.metafile(file='C:/Users/Victor/Desktop/AUROCS_legend2.wmf', width = 10, height = 4)
  grid.draw(p_BNM_med$leg) ;dev.off()

  p_BNM_top = generate_plot(df = df_BNM_top, title = 'BN-EM-MedLEE classifier',
                            legend_pos = legend_pos, title_size = title_size)
  p_BNT_top = generate_plot(df = df_BNT_top, title = 'BN-EM-Topaz classifier',
                            legend_pos = legend_pos, title_size = title_size)
  win.metafile(file='C:/Users/Victor/Desktop/NLP_Topaz.wmf', width = 10, height = 5.5)
  multiplot(p_BNM_top$p,p_BNT_top$p,cols=2) ;dev.off()
  
  win.metafile(file='C:/Users/Victor/Desktop/AUROCS_legend.wmf', width = 10, height = 5.5)
  grid.draw(p_BNM_top$leg) ;dev.off()
  
  
}
#--------------------------------------------------------------------------------------------

#-------------------------Plot - comparison between final and prelims------------------------
if(plot_FinalvsPrelim == TRUE){
  auc_final_med = c()
  for(obj in ROC_objects[1:6]){
    auc_final_med = c(auc_final_med,unclass(obj$auc)[1])
  }
  df_final_med = data.frame(auc_final = auc_final_med)
  
  auc_prelim_med = c()
  for(obj in ROC_objects[13:18]){
    auc_prelim_med = c(auc_prelim_med,unclass(obj$auc)[1])
  }
  df_prelim_med = data.frame(auc_prelim = auc_prelim_med)
  
  auc_final_top = c()
  for(obj in ROC_objects[7:12]){
    auc_final_top = c(auc_final_top,unclass(obj$auc)[1])
  }
  df_final_top = data.frame(auc_final = auc_final_top)
  
  auc_prelim_top = c()
  for(obj in ROC_objects[19:24]){
    auc_prelim_top = c(auc_prelim_top,unclass(obj$auc)[1])
  }
  df_prelim_top = data.frame(auc_prelim = auc_prelim_top)
  #joint dataframes for ggplot
  df_medlee = cbind(df_final_med, df_prelim_med, NLP='MedLEE')
  df_topaz = cbind(df_final_top, df_prelim_top, NLP='Topaz')
  #ggplot time
  library(ggplot2)
  df_both = rbind(df_medlee, df_topaz)
  p = ggplot(df_both, aes(auc_final, auc_prelim)) + 
    geom_point(stat='Identity', shape=16, aes(color=NLP), size=4) +
    geom_abline(intercept=0, slope=1) #+ coord_cartesian(ylim=c(0,1), xlim=c(0,1))
  p + xlab('AUROC - Authenticated') + ylab('AUROC - Preliminary') + theme(legend.position='top')
}
#--------------------------------------------------------------------------------------------

#-------------------------------PLOT - FEATURE SELECTION AUROC-------------------------------
if(plot_featureSelection == TRUE){
  aucs = c()
  for(i in c(1,3,5)){
    aucs = c(aucs, unclass(ROC_objects[[i]]$auc)[1])
  }
  aucs
  features = c('31 Features', 'First 35', 'Best 28')
  df_fs = data.frame(AUROC = aucs, Features = features)
  df_fs$Features = factor(df_fs$Features,levels=c("31 Features","First 35","Best 28"))
  p_fs = ggplot(df_fs) + geom_bar(aes(Features, AUROC,fill=Features), stat='Identity')
  p_fs = p_fs + coord_cartesian(ylim=c(0.5,0.93)) + xlab('Feature subset')
  leg = g_legend(p_fs)
  p_fs = p_fs + guides(color=F, fill=F)
  
  #ROCs:
  roc_31 = data.frame(Sensitivity = ROC_objects[[1]]$sensitivities,
                      Specificity = 1-ROC_objects[[1]]$specificities,
                      Features = '31 Features')
  roc_first = data.frame(Sensitivity = ROC_objects[[2]]$sensitivities,
                      Specificity = 1-ROC_objects[[2]]$specificities,
                      Features = 'First 35')
  roc_best = data.frame(Sensitivity = ROC_objects[[4]]$sensitivities,
                      Specificity = 1-ROC_objects[[4]]$specificities,
                      Features = 'Best 28')
  rocs = rbind(roc_31, roc_first, roc_best)
  
  roc_p = ggplot(rocs) + 
    geom_line(aes(Specificity, Sensitivity, color=Features), stat='Identity')
  #roc_p = roc_p + guides(color=F, fill=F)
  
  multiplot(p_fs, roc_p, cols=2)
  
  win.metafile('C:/Users/Victor/Desktop/FS_pure.wmf', width=5, height=5.5)
  roc_p + theme(axis.title=element_text(face="bold",size=as.character(font)),
        axis.text=element_text(size=as.character(font-2)),
        plot.title = element_text(face="bold",size=as.character(font)),
        legend.title=element_text(size=as.character(font-2)),
        legend.text=element_text(size=as.character(font-2)),
        legend.justification=c(1,0), legend.position=c(1,0))
  dev.off()
}
#--------------------------------------------------------------------------------------------