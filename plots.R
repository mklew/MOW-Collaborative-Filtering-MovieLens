library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)

perf1 <- performance(pred, "prec", "rec")
plot(perf1)

perf1 <- performance(pred, "sens", "spec")
plot(perf1)
