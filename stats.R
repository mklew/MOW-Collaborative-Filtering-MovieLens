library('reshape2')
userRatingsFile = "u1.base"

getFilePath <- function(filename, prefix)
{
	pathToImages = file.path("doc", "img")
	fnNoDots <- gsub(".", "", filename, fixed=T)
	return (file.path(pathToImages, paste(prefix,"--",fnNoDots,sep='')))
}


# createHistograms <- function(userRatingsFile)
# {
# 	data = createUsersMatrix(read.table(file.path("data",userRatingsFile)))
# 	plot = qplot(getRatings(data), binwidth = 1, 
#       geom="histogram", xlab="Oceny", ylab="Częstość",main="Histogram ocen")
# 	ggsave(plot,file=getFilePath("histogram_ocen.pdf", userRatingsFile))

# 	plot2 = qplot(getRatings(normalize(data, method = "Z-score")),
#       geom="histogram", xlab="Oceny", ylab="Częstość",main="Histogram ocen znormalizowanych")
# 	ggsave(plot2,file=getFilePath("histogram_ocen_norm.pdf", userRatingsFile))
#}
#createHistograms(userRatingsFile)


# getRatings reads all non missing ratings as vector


data = createUsersMatrix(read.table(file.path("data",userRatingsFile)))
plot = qplot(getRatings(data), binwidth = 1, 
  geom="histogram", xlab="Oceny", ylab="Freq",main="Histogram ocen", fill= ..count..)

# przy zapisie nie ma polskich znaków ; (
ggsave(plot,file=getFilePath("histogram_ocen.pdf", userRatingsFile))

plot2 = qplot(getRatings(normalize(data, method = "Z-score")),
  geom="histogram", xlab="Oceny", ylab="Freq",main="Histogram ocen znormalizowanych", fill=..count..)
ggsave(plot2,file=getFilePath("histogram_ocen_norm.pdf", userRatingsFile))


# basic stats
summary(getRatings(data))

minimalNumberOfRatings <- min(rowCounts(data)) ## Najmniejsza liczba ocen u jakiegoś usera
print(paste("Najmniejsza liczba ocen", minimalNumberOfRatings))
maximalNumberOfRatings <- max(rowCounts(data)) ## Największa liczba ocen u jakeigoś usera
print(paste("Największa liczba ocen", maximalNumberOfRatings))

minimalAverageRating <- min(rowMeans(data))
## Najmniejsza średnia ocena u jakiegoś użytkownika
print(paste("Najmniejsza średnia ocena", minimalAverageRating))
## Największa średnia ucena u jakiegoś użytkownika
maximalAverageRating <- max(rowMeans(data))
print(paste("Największa średnia ocena", maximalAverageRating))

averageNumberOfRatedMovies <- mean(rowCounts(data))
print(paste("Średnia liczba filmów oceniona przez użytkownika", averageNumberOfRatedMovies))


