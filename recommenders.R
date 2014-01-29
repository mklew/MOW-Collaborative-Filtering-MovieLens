#UBCF recommender
e1 <- evaluationScheme(allData, method="split", train=0.8)
rUbcf =  Recommender(trainData[1:100], method = "UBCF")

rUbcf =  Recommender(trainData, method = "UBCF",
                     param=list(normalize="center", method="cosine", nn=50, minRating=3, sample=F))
rUbcf =  Recommender(trainData, method = "UBCF",
                     param=list(normalize="center", method="pearson", nn=50, minRating=3, sample=F))

rUbcf =  Recommender(trainData, method = "UBCF",
                     param=list(normalize="Z-score", method="cosine", nn=50, minRating=3, sample=F))

rUbcf =  Recommender(trainData, method = "UBCF",
                     param=list(normalize="Z-score", method="pearson", nn=50, minRating=3, sample=F))

predict(rUbcf,type="ratings", getData(e1,"known")[1:10])

#IBCF recommender

#rIbcf =  Recommender(getData(e1,"train"), method = "IBCF",param=list(normalize="center", method="cosine", minRating=3))

t1 = system.time(rIbcf <-  Recommender(getData(e1,"train"), method = "IBCF"))

p1 = predict(rIbcf,type="ratings", getData(e1,"known"))
