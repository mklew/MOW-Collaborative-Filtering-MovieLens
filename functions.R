#user id | item id | rating | timestamp
createUsersMatrix<-function(dataTable)
{
  #uniqueSortedId = sort(levels(factor(dataTable[,2])))
  #movies = length(uniqueSortedId)
  movies = 1682
  #usersUniqueSortedId = sort(levels(factor(dataTable[,1])))
  #users = length(usersUniqueSortedId)
  users = 943
  
  #read movie names
  movieInfo = read.csv(file="data\\u.item",sep="|",header=FALSE) #read movies info from file as table
  movieMatrix = as(movieInfo,"matrix") #matrix is better to read
  movieNames = movieMatrix[,2]
  
  #create matrix for ratings
  usersRatings = matrix(rep(NA,movies*users),ncol=movies,dimnames=list(user=paste("u",1:users,sep=''),movie=movieNames))
  colnames(do.NULL=FALSE,usersRatings,uniqueSortedId)
  
  #fill matrix with user ratings
  for (i in 1:length(dataTable[,1]))
  {
    userId = dataTable[i,1]
    movieId = dataTable[i,2]
    #print(cat(i, ", ", userId, ", ", movieId))
    usersRatings[userId,movieId] = dataTable[i,3] #userId
  }
  
  #convert matrix to recommenderlab structure
  dataReal = as(usersRatings, "realRatingMatrix")
  return (dataReal)
}
