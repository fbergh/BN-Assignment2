################
### PACKAGES ###
################

# Import functions from tools.R file
source("tools.R")


####################
### LOADING DATA ###
####################

# Convert columns to double such that the algorithms work
ff = read.csv("data/ff_preprocessed.csv", header = TRUE, colClasses = rep("double", 11))
head(ff)


################
### ANALYSIS ###
################

result_experiment = run_experiment(ff, is_plot=TRUE, algorithms=c("tabu","hiton"), 
                        exp_values=list(c("loglik-g","bic-g","bge"),c(0.01,0.05,0.1,0.25)))

# result can be indexed like so (the first three indices/entries are from tabu and the other four from si.hiton.pc)
# result$bn_structures; result$adjacency_mats; result$betweenness; result$degree; result$hamming
result_comparison = compare_to_original(result_experiment$adjacency_mats)