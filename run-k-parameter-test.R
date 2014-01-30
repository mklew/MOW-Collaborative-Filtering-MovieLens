# run-k-parameter-test.R
library('ggplot2')
source('recommender.R')

ibcfKBenchmarkResults <- ibcf.k.benchmark(scheme)
ibcf.k.benchmark.saveGraphs(ibcfKBenchmarkResults)