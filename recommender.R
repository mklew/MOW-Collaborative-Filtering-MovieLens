source('functions.R')
library('recommenderlab')
library('ROCR')
library('xtable')
library('ggplot2')
data(MovieLense)        #load MovieLense 100k data into workspace from recommender package (optional - we have our data)

#read all data from file and convert it into recommenderlab structure
allData = MovieLense 
# allData = createUsersMatrix(read.table(file.path("data", "u.data")))

#read trainig and test data (80% - 20%)
#trainData = createUsersMatrix(read.table(file.path("data","u1.base")))
#testData = createUsersMatrix(read.table(file.path("data","u1.test")))

#recommendations
#recommender =  Recommender(trainData, method = "UBCF")   #create recommender, also UBCF, IBCF, RANDOM, POPULAR
#recommender2 =  Recommender(trainData, method = "IBCF")
#recoms = predict(recommender,type="topNList", testData[1:10], n=3)  #predict best 3 movies for first 10 test users
#recoms = predict(recommender,type="ratings", testData[1:10])  #predict all movie ratings for first 10 test users
#recomsMatrix = as(recoms,"matrix")[,1:10] #clip rating matrix to first 10 movies

scheme = evaluationScheme(allData, method="split",train=0.8, goodRating = 4)
algorithms <- list(
   "popular items" = list(name="POPULAR", param=NULL),
   "user-based CF Cosine center nn50" = list(name="UBCF", param=list(method="Cosine", normalize='center', nn=20)),
   "user-based CF Pearson center nn50" = list(name="UBCF", param=list(method="pearson", normalize='center', nn=20)), 
   "user-based CF jaccard center nn50" = list(name="UBCF", param=list(method="jaccard", normalize='center', nn=20)),   
   "user-based CF jaccard center nn50" = list(name="UBCF", param=list(method="jaccard", normalize='center', nn=20)),   
   "user-based CF Pearson z-index nn50" = list(name="UBCF", param=list(method="pearson", normalize='Z-score', nn=20)),
   "item-based CF Cosine center" = list(name="IBCF", param=(list(k = 30, method="Cosine", normalize = "center",  alpha = 0.5))),
   "item-based CF pearson center" = list(name="IBCF", param=(list(k = 5, method="pearson", normalize = "center",  alpha = 0.5))),   		
	"item-based CF jaccard center" = list(name="IBCF", param=(list(k = 5, method="jaccard", normalize = "center",  alpha = 0.5))),	
	"item-based CF Cosine Z-score" = list(name="IBCF", param=(list(k = 6, method="Cosine", normalize = "Z-score",  alpha = 0.5))),
    "item-based CF pearson Z-score" = list(name="IBCF", param=(list(k = 7, method="pearson", normalize = "Z-score",  alpha = 0.5))),
   	"item-based CF jaccard Z-score" = list(name="IBCF", param=(list(k = 5, method="jaccard", normalize = "Z-score",  alpha = 0.5)))		
   )

#evalResults = evaluate(scheme,algorithms,n=c(1,3,5,10,20))


#
#	Metody podobieństwa
#
similarityMethods.benchmark <- function(scheme) {	
	# Porównanie tylko UBCF między sobą, 
	ubcfParamsCosine = list(method="Cosine", normalize='center', nn=25)
	ubcfParamsPearson = list(method="pearson", normalize='center', nn=25)
	ubcfParamsJaccard = list(method="jaccard", normalize='center', nn=25)

	ibcfParamsCosine = list(method="Cosine", normalize='center', k=30, alpha=0.5)
	ibcfParamsPearson = list(method="pearson", normalize='center', k=30, alpha=0.5)
	ibcfParamsJaccard = list(method="jaccard", normalize='center', k=30, alpha=0.5)

	trainingData <- getData(scheme, "train")
	knownData <- getData(scheme, "known")	
	
	ubcf.cosine <- Recommender(trainingData, method="UBCF", parameter=ubcfParamsCosine)
	ubcf.pearson <- Recommender(trainingData, method="UBCF", parameter=ubcfParamsPearson)
	ubcf.jaccard <- Recommender(trainingData, method="UBCF", parameter=ubcfParamsJaccard)

	ubcf.cosine.predictions <- predict(ubcf.cosine, knownData, type="ratings")
	ubcf.pearson.predictions <- predict(ubcf.pearson, knownData, type="ratings")
	ubcf.jaccard.predictions <- predict(ubcf.jaccard, knownData, type="ratings")

	unknownData <- getData(scheme, "unknown")	

	ubcfErrors <- rbind(
		calcPredictionError(ubcf.cosine.predictions, unknownData),
		calcPredictionError(ubcf.pearson.predictions, unknownData),
 		calcPredictionError(ubcf.jaccard.predictions, unknownData)
	)
	rownames(ubcfErrors) <- c("UBCF cosine","UBCF Pearson", "UBCF Jaccard")

	ibcf.cosine <- Recommender(trainingData, method="IBCF", parameter=ibcfParamsCosine)
	ibcf.pearson <- Recommender(trainingData, method="IBCF", parameter=ibcfParamsPearson)
	ibcf.jaccard <- Recommender(trainingData, method="IBCF", parameter=ibcfParamsJaccard)

	ibcf.cosine.predictions <- predict(ibcf.cosine, knownData, type="ratings")
	ibcf.pearson.predictions <- predict(ibcf.pearson, knownData, type="ratings")
	ibcf.jaccard.predictions <- predict(ibcf.jaccard, knownData, type="ratings")

	ibcfErrors <- rbind(
		calcPredictionError(ibcf.cosine.predictions, unknownData),
		calcPredictionError(ibcf.pearson.predictions, unknownData),
 		calcPredictionError(ibcf.jaccard.predictions, unknownData)
	)
	rownames(ibcfErrors) <- c("IBCF cosine","IBCF Pearson", "IBCF Jaccard")

	list(ubcf.errors=ubcfErrors, ubcf.cosine = ubcf.cosine, ubcf.pearson=ubcf.pearson, ubcf.jaccard=ubcf.jaccard,
		ibcf.errors=ibcfErrors, ibcf.cosine = ibcf.cosine, ibcf.pearson=ibcf.pearson, ibcf.jaccard=ibcf.jaccard)
}

similarityMethods.benchmark.saveData <- function(resultsMatrix, fileName, caption) {
	ibcfSMTable <- xtable(data.frame(resultsMatrix), caption=caption)
	ibcfSMTableFile <- file(file.path("doc", fileName))
	writeLines(print(ibcfSMTable, type="latex"), ibcfSMTableFile)
	close(ibcfSMTableFile)
}


#
#	Przeprowadza test parametru nn algorytmu UBCF i zwraca wyniki pozwalające na rysowanie wykresów
#
# z tego można narysować wykresy:
#	- jak się zmienia błąd (MAE, MSE, RMSE) w zależności od nn
# 	- jak się zmienia czas w zależności od nn
#	- jak wyglądają krzywe ROC dla ewaluacji algorytmów z różnym nn
#
#	zwraca liste
#		$errorsAndTime to jest macierz z kolumnami [nn, czas, MAE, MSE, RMSE]
#		$evaluationResults to wyniki evaluate dla różnych wartości nn przy reszcie parametrów takich samych
ubcf.nn.benchmark <- function(scheme, startNN, step, maxNN) {
	ubcfCommonParameters = list(method="Cosine", normalize='center')
	trainingData <- getData(scheme, "train")
	knownData <- getData(scheme, "known")	
	unknownData <- getData(scheme, "unknown")	
	# od nn=5 ze skokiem 20, aż do całkowitej liczby użytkowników
	nns <- seq(startNN, maxNN, step)
	algorithms <- list()
	errorsAndTime <- c()
	for(nn in nns) {
	# for(nn in 5:10) {
		algorithmParameters <- c(ubcfCommonParameters, nn=nn)
		algorithms[[paste("UBCF nn=", nn)]] = list(name="UBCF", param=algorithmParameters)
		ubcf <- 0
		timeToBuildRecommender <- system.time({
			ubcf <- Recommender(trainingData, method="UBCF", parameter=algorithmParameters)
		})
		ubcf.predictions <- 0
		timeToPredict <- system.time({
				ubcf.predictions <- predict(ubcf, knownData, type="ratings")	
		})		
		errors <- calcPredictionError(ubcf.predictions, unknownData)
		errorsAndTime <- rbind(c(nn, timeToBuildRecommender[3], timeToPredict[3], errors), errorsAndTime)
	}
	colnames(errorsAndTime) <- c("nn", "czas_budowy", "czas_predykcji", "MAE", "MSE", "RMSE")
	evaluationResults <- evaluate(scheme,algorithms,n=c(1,3,5,10,20))
	list(errorsAndTime = errorsAndTime, evaluationResults = evaluationResults)
}

# Funcka przeprowadza test parametry k dla algorytmu IBCF. Parametr k to ile podobnych przedmiotów 
# ma być branych pod uwagę.
#	
#	Zwraca listę
#		$errorsAndTime to macierz z kolumnami "k", "czas", "MAE", "MSE", "RMSE"
#		$evaluationResults to wyniki evaluate dla różnych k algorytmu IBCF przy reszcie parametrow takich samych
#
ibcf.k.benchmark <- function(scheme) {
	ibcfCommonParameters = list(method="Cosine", normalize='center', alpha=0.5)
	trainingData <- getData(scheme, "train")
	knownData <- getData(scheme, "known")	
	unknownData <- getData(scheme, "unknown")	
	# od k=5 ze skokiem 20 aż do arbitralnego 150
	ks <- seq(5, 150, 20)
	algorithms <- list()
	errorsAndTime <- c()
	for(k in ks) {
	# for(k in 5:10) {
		print(paste("IBCF k=", k))
		algorithmParameters <- c(ibcfCommonParameters, k=k)
		algorithms[[paste("IBCF k=", k)]] = list(name="IBCF", param=algorithmParameters)
		ibcf <- 0
		timeToBuildRecommender <- system.time({
			ibcf <- Recommender(trainingData, method="IBCF", parameter=algorithmParameters)
		})
		ibcf.predictions <- 0
		timeToPredict <- system.time({
			ibcf.predictions <- predict(ibcf, knownData, type="ratings")	
		})		
		errors <- calcPredictionError(ibcf.predictions, unknownData)
		errorsAndTime <- rbind(c(k, timeToBuildRecommender[3], timeToPredict[3], errors), errorsAndTime)
	}
	colnames(errorsAndTime) <- c("k", "czas_budowy","czas_predykcji", "MAE", "MSE", "RMSE")
	evaluationResults <- evaluate(scheme,algorithms,n=c(1,3,5,10,20))
	list(errorsAndTime = errorsAndTime, evaluationResults = evaluationResults)
}


#results <- similarityMethods.benchmark(scheme)

#similarityMethods.benchmark.saveData(results$ibcf.errors, "ibcfSimilarityTableErrors.tex", c('Porównanie metod podobieństwa dla IBCF'))
#similarityMethods.benchmark.saveData(results$ubcf.errors, "ubcfSimilarityTableErrors.tex", c('Porównanie metod podobieństwa dla UBCF'))


# results <- ubcf.nn.benchmark(scheme)
#results <- ibcf.k.benchmark(scheme)

# Tworzy wykresy bazując na wynikach z ibcf.k.benchmark
ibcf.k.benchmark.saveGraphs <- function(ibcf.k.benchmark.results) {
	errorsAndTimeDF <- data.frame(ibcf.k.benchmark.results$errorsAndTime)
	q <- qplot(k, MAE, data=errorsAndTimeDF, geom="line", xlab="parametr k", main="MAE(k)")	
	ggsave(file.path("doc", "img", "ibcf-K-MAE.pdf"))
	q <- qplot(k, MSE, data=errorsAndTimeDF, geom="line", xlab="parametr k", main="MSE(k)")	
	ggsave(file.path("doc", "img", "ibcf-K-MSE.pdf"))
	q <- qplot(k, RMSE, data=errorsAndTimeDF, geom="line", xlab="parametr k", main="RMSE(k)")	
	ggsave(file.path("doc", "img", "ibcf-K-RMSE.pdf"))
	q <- qplot(k, czas_budowy, data=errorsAndTimeDF, geom="line", xlab="parametr k", main="czas budowy(k)")	
	ggsave(file.path("doc", "img", "ibcf-K-czas-budowy.pdf"))

	q <- qplot(k, czas_predykcji, data=errorsAndTimeDF, geom="line", xlab="parametr k", main="czas predykcji(k)")	
	ggsave(file.path("doc", "img", "ibcf-K-czas-predykcji.pdf"))

	pdf(file.path("doc", "img", "ibcf-K-ROC.pdf"))
	plot(ibcf.k.benchmark.results$evaluationResults)
	dev.off()

	pdf(file.path("doc", "img", "ibcf-K-PREC-REC.pdf"))
	plot(ibcf.k.benchmark.results$evaluationResults, "prec/rec", annotate=TRUE)
	dev.off()
}


ubcf.nn.benchmark.saveGraphs <- function(ubcf.nn.benchmark.results, prefix = "") {
	errorsAndTimeDF <- data.frame(ubcf.nn.benchmark.results$errorsAndTime)
	q <- qplot(nn, MAE, data=errorsAndTimeDF, geom="line", xlab="parametr nn", main="MAE(nn)")	
	ggsave(file.path("doc", "img", paste(prefix,"ubcf-NN-MAE.pdf") ))
	q <- qplot(nn, MSE, data=errorsAndTimeDF, geom="line", xlab="parametr nn", main="MSE(nn)")	
	ggsave(file.path("doc", "img", paste(prefix, "ubcf-NN-MSE.pdf") ))
	q <- qplot(nn, RMSE, data=errorsAndTimeDF, geom="line", xlab="parametr nn", main="RMSE(nn)")	
	ggsave(file.path("doc", "img", paste(prefix, "ubcf-NN-RMSE.pdf")  ))
	q <- qplot(nn, czas_budowy, data=errorsAndTimeDF, geom="line", xlab="parametr nn", main="czas budowy(nn)")	
	ggsave(file.path("doc", "img", paste(prefix, "ubcf-NN-czas-budowy.pdf") ))

	q <- qplot(nn, czas_predykcji, data=errorsAndTimeDF, geom="line", xlab="parametr nn", main="czas predykcji(nn)")	
	ggsave(file.path("doc", "img", paste(prefix, "ubcf-NN-czas-predykcji.pdf") ))

	pdf(file.path("doc", "img", paste(prefix, "ubcf-NN-ROC.pdf") ))
	plot(ubcf.nn.benchmark.results$evaluationResults)
	dev.off()

	pdf(file.path("doc", "img", paste(prefix, "ubcf-NN-PREC-REC.pdf") ))
	plot(ubcf.nn.benchmark.results$evaluationResults, "prec/rec", annotate=TRUE)
	dev.off()
}

