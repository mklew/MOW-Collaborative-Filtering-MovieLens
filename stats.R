library('reshape2')
library('ggplot2')
library('recommenderlab')
library('xtable')

data(MovieLense)
userRatingsFile = "u1.base"

# getRatings reads all non missing ratings as vector


#data = createUsersMatrix(read.table(file.path("data",userRatingsFile)))
data=MovieLense
plot1 = qplot(getRatings(data), binwidth = 1, 
  geom="histogram", xlab="Oceny", ylab="Freq",main="Histogram ocen", fill= ..count..)

# przy zapisie nie ma polskich znaków ; (
ggsave(plot=plot1,file=file.path("doc", "img", "histogram_ocen.pdf") )

plot2 = qplot(getRatings(normalize(data, method = "Z-score")),
  geom="histogram", xlab="Oceny", ylab="Freq",main="Histogram ocen znormalizowanych", fill=..count..)
ggsave(plot2,file=file.path("doc", "img",  "histogram_ocen_norm.pdf") )


# basic stats
s <- summary(getRatings(data))

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

dt <- data.frame()
dt <- rbind(dt, c(s[3]))
dt <- rbind(dt, c(s[4]))
dt <- rbind(dt, c(minimalNumberOfRatings))
dt <- rbind(dt, c(maximalNumberOfRatings))
dt <- rbind(dt, c(minimalAverageRating))
dt <- rbind(dt, c(maximalAverageRating))
dt <- rbind(dt, c(averageNumberOfRatedMovies))
names(dt) <- c("wartość")

row.names(dt) <- c("Mediana ocen", "Średnia ocena", "Najmniejsza liczba ocen", "Największa liczba ocen", "Najmniejsza średnia ocena", "Największa średnia ocena", "Średnia liczba filmów oceniona przez użytkownika")

dtTable <- xtable(dt, caption="statystyki zbioru danych")
	dtTableFile <- file(file.path("doc", "statystyki-zbioru.tex"))
	writeLines(print(dtTable, type="latex"), dtTableFile)
	close(dtTableFile)


