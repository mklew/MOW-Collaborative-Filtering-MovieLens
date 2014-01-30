# run-k-parameter-test.R

source('recommender.R')

ibcfKBenchmarkResults <- ibcf.k.benchmark(scheme)
ibcf.k.benchmark.saveGraphs(ibcfKBenchmarkResults)