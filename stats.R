library('reshape2')
library('ggplot2')
userRatingsFile = "u1.base"

# getRatings reads all non missing ratings as vector


data = createUsersMatrix(read.table(file.path("data",userRatingsFile)))
plot1 = qplot(getRatings(data), binwidth = 1, 
  geom="histogram", xlab="Oceny", ylab="Freq",main="Histogram ocen", fill= ..count..)

# przy zapisie nie ma polskich znaków ; (
ggsave(plot=plot,file="histogram_ocen.pdf")

plot2 = qplot(getRatings(normalize(data, method = "Z-score")),
  geom="histogram", xlab="Oceny", ylab="Freq",main="Histogram ocen znormalizowanych", fill=..count..)
ggsave(plot2,file="histogram_ocen_norm.pdf")


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


