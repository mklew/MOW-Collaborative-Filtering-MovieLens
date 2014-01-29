data(MovieLense)        #load MovieLense 100k data into workspace from recommender package (optional - we have our data)
source('functions.R')
library('recommenderlab')
library('ROCR')
library('xtable')

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

scheme = evaluationScheme(allData[1:300], method="split",train=0.8, goodRating = 4)
algorithms <- list(
   "popular items" = list(name="POPULAR", param=NULL),
   "user-based CF Cosine center nn50" = list(name="UBCF", param=list(method="Cosine", normalize='center', nn=20, minRating=4)),
   "user-based CF Pearson center nn50" = list(name="UBCF", param=list(method="pearson", normalize='center', nn=20, minRating=4)), 
   "user-based CF jaccard center nn50" = list(name="UBCF", param=list(method="jaccard", normalize='center', nn=20, minRating=4)),   
   "user-based CF jaccard center nn50" = list(name="UBCF", param=list(method="jaccard", normalize='center', nn=20, minRating=4)),   
   "user-based CF Pearson z-index nn50" = list(name="UBCF", param=list(method="pearson", normalize='Z-score', nn=20, minRating=4)),
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
	ubcfParamsCosine = list(method="Cosine", normalize='center', nn=25, minRating=4)
	ubcfParamsPearson = list(method="pearson", normalize='center', nn=25, minRating=4)
	ubcfParamsJaccard = list(method="jaccard", normalize='center', nn=25, minRating=4)

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

# z tego można narysować wykresy:
#	- jak się zmienia błąd (MAE, MSE, RMSE) w zależności od nn
# 	- jak się zmienia czas w zależności od nn
#	- jak wyglądają krzywe ROC dla ewaluacji algorytmów z różnym nn
ubcf.nn.benchmark <- function(scheme) {
	ubcfCommonParameters = list(method="Cosine", normalize='center', minRating=4)
	trainingData <- getData(scheme, "train")
	knownData <- getData(scheme, "known")	
	unknownData <- getData(scheme, "unknown")	
	# od nn=5 ze skokiem 20, aż do całkowitej liczby użytkowników
	nns <- seq(5, dim(trainingData)[1], 20)
	algorithms <- list()
	errorsAndTime <- c()
	# TODO odkomentować tą pętle
	# for(nn in nns) {
	for(nn in 5:10) {
		print(nn)
		algorithmParameters <- c(ubcfCommonParameters, nn=nn)
		algorithms[[paste("UBCF nn=", nn)]] = list(name="UBCF", param=algorithmParameters)
		ubcf <- 0
		# TODO ten czas wyświetla się jako user_time system_time elapsed_time a to nam nie potrzebne
		#  jak zrobić z tego pojedynczą wartość np. w milisekundach??
		timeToBuildRecommender <- system.time({
			ubcf <- Recommender(trainingData, method="UBCF", parameter=algorithmParameters)
		})
		ubcf.predictions <- predict(ubcf, knownData, type="ratings")
		errors <- calcPredictionError(ubcf.predictions, unknownData)
		errorsAndTime <- rbind(c(nn, timeToBuildRecommender, errors), errorsAndTime)
	}
	evaluationResults <- evaluate(scheme,algorithms,n=c(1,3,5,10,20))
	list(errorsAndTime = errorsAndTime, evaluationResults = evaluationResults)
}

#results <- similarityMethods.benchmark(scheme)

#similarityMethods.benchmark.saveData(results$ibcf.errors, "ibcfSimilarityTableErrors.tex", c('Porównanie metod podobieństwa dla IBCF'))
#similarityMethods.benchmark.saveData(results$ubcf.errors, "ubcfSimilarityTableErrors.tex", c('Porównanie metod podobieństwa dla UBCF'))


results <- ubcf.nn.benchmark(scheme)

