#run-similarity-methods-test.R

source('recommender.R')

results <- similarityMethods.benchmark(scheme)

similarityMethods.benchmark.saveData(results$ibcf.errors, "ibcfSimilarityTableErrors.tex", c('Metody podobieństwa dla IBCF'))
similarityMethods.benchmark.saveData(results$ubcf.errors, "ubcfSimilarityTableErrors.tex", c('Metody podobieństwa dla UBCF'))
