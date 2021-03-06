#run-nn-parameter-test.R
library('ggplot2')
source('recommender.R')

trainingData <- getData(scheme, "train")
ubcfNNBenchmarkResults <- ubcf.nn.benchmark(scheme, startNN = 30, step = 30, maxNN= 200)
ubcf.nn.benchmark.saveGraphs(ubcfNNBenchmarkResults, "30-max-50-no-minRating")