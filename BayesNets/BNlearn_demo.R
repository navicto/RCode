library(bnlearn)
library(pROC)
training = read.arff("/Users//Victor/Desktop/weather.nominal.arff")
# training[training == "?"] = NA
test = read.arff("/Users//Victor/Desktop/weather.nominal.arff")
# test[test == "?"] = NA
bn = naive.bayes(training, "READMITTED")

pred = predict(bn, test, prob=TRUE)

pred = attributes(pred)

p_T = pred$prob["Y",]

obs = as.numeric(as.logical(data$READMITTED == "Y"))

auroc = auc(roc(response = obs, predictor = p_T))

ci.auc(auroc)

