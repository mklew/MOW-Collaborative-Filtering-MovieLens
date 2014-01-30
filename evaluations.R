scheme = evaluationScheme(allData, method="split",train=0.8, goodRating = 0)

rUbcf =  Recommender(getData(scheme,"known"), method = "UBCF",
                     param=list(normalize="center", method="cosine", nn=50, minRating=0, sample=F))


algorithms <- list(
  "popular items" = list(name="POPULAR", param=NULL),
  "UBCF Cosine center nn50" = list(name="UBCF", param=list(method="cosine", normalize='center', nn=50, minRating=0))
)

ubcfParamsCosine = list(method="Cosine", normalize='center', nn=25, minRating=4)

#n=c(1,3,5) - variantsc for various sizes of topNList
evalResults = evaluate(scheme,algorithms,n=c(1,3,5),progress=TRUE,keepModel=TRUE)

#calc errors for 1st algorithm
r1 = Recommender(getData(scheme, "known"), method = "UBCF")
#confusion matrices for all runs for 1st algorithm
confMatrix1 = getConfusionMatrix(evalResults[[1]])