data(MovieLense)        #load MovieLense 100k data into workspace from recommender package (optional - we have our data)
source('functions.R')
library('recommenderlab')
library('ROCR')

#read all data from file and convert it into recommenderlab structure
allData = createUsersMatrix(read.table(file.path("data", "u.data")))

#read trainig and test data (80% - 20%)
trainData = createUsersMatrix(read.table(file.path("data","u1.base")))
testData = createUsersMatrix(read.table(file.path("data","u1.test")))

#recommendations
recommender =  Recommender(trainData, method = "UBCF")   #create recommender, also UBCF, IBCF, RANDOM, POPULAR
recommender2 =  Recommender(trainData, method = "IBCF")
#recoms = predict(recommender,type="topNList", testData[1:10], n=3)  #predict best 3 movies for first 10 test users
recoms = predict(recommender,type="ratings", testData[1:10])  #predict all movie ratings for first 10 test users
recomsMatrix = as(recoms,"matrix")[,1:10] #clip rating matrix to first 10 movies

scheme = evaluationScheme(allData, method="split",train=0.8)
algorithms <- list(
   "random items" = list(name="RANDOM", param=NULL),   "popular items" = list(name="POPULAR", param=NULL),
   "user-based CF" = list(name="UBCF", param=list(method="Cosine", nn=50, minRating=5))
   )
evalResults = evaluate(scheme,algorithms,n=c(1,3,5,10,20))

