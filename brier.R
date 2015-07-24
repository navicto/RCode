library(verification)
# library(pROC)

get_bss <-function(predictions_path, obs_label='actual', pred_label='p_T', prev=NULL) {
  predictions = read.csv(predictions_path)
  if(!is.null(prev)){
#     base = sample(x=c(T,F), size=length(predictions[[pred_label]]), replace=T, prob=c(prev, 1-prev))
#     print(length(base))
    base = prev
#     base = rep(x = prev, times = length(predictions[[pred_label]]))
    bs = brier(obs=predictions[[obs_label]], pred=predictions[[pred_label]], baseline = base)
  } else{
    bs = brier(obs=predictions[[obs_label]], pred=predictions[[pred_label]])
  }
  return(list(bs, predictions))
}

prevalence = NULL
pred_label = 'p_T'
prediction_dir = '/Users//Victor/Desktop/Temp/Predictions/Reformatted/'
prediciton_files = list.files(path = prediction_dir, full.names = T)
scores = list()
for(f in prediciton_files){
  scores[[f]] = get_bss(f, prev=prevalence, pred_label = pred_label)[[1]]$ss
}
# bs_demog = get_bss(demog_path, prev=prevalence, pred_label=pred_label)
# ss_demog = bs_demog[[1]]
# predictions_demog = bs_demog[[2]]
# print(ss_demog$ss)


