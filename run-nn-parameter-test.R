#run-nn-parameter-test.R

source('recommender.R')

ubcfNNBenchmarkResults <- ubcf.nn.benchmark(scheme)
ubcf.nn.benchmark.saveGraphs(ubcfNNBenchmarkResults)