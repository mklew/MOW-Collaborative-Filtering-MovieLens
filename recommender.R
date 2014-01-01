#data(MovieLense)        #load MovieLense 100k data into workspace from recommender package (optional - we have our data)
users = 943
movies = 1664

#read all data from file and convert it into recommenderlab structure
dataTable <- read.table("data\\u.data") #read data from file as table
userRatings = createUsersMatrix(dataTable)

#read trainig and test data (80% - 20%)
data1 <- read.table("data\\u1.base")
test1 <- read.table("data\\u1.test")
trainData = createUsersMatrix(data1)
testData = createUsersMatrix(test1)

#recommendations
recommender =  Recommender(trainData, method = "POPULAR")   #create recommender, also UBCF, IBCF, RANDOM
recoms = predict(recommender, testData[1:10], n=3)  #predict best 3 movies for first 10 test users
as(recoms,"list")

