################
### PACKAGES ###
################

library(bnlearn)
source("tools.R")


####################
### LOADING DATA ###
####################

# Read the data from the csv file
ff = read.csv("data/ff_preprocessed.csv", header = TRUE, colClasses = rep("double", 11))

# Show head of dataframe
head(ff)


###################
### TABU SEARCH ###
###################
# See https://www.bnlearn.com/documentation/man/hc.html

# To run HC instead, replace "tabu" with "hc"; some parameters change (see link above)

# Standard settings for tabu search: 
# start = NULL (initial DAG), 
# whitelist = NULL (edges that need to be included), 
# blacklist = NULL (edges that are not allowed to be included), 
# score = NULL (scoring function; see https://www.bnlearn.com/documentation/man/network.scores.html),
# debug = FALSE (printing debugging output), 
# tabu = 10 (length of tabu list), 
# max.tabu = tabu (max. nr of iterations without improving score), 
# max.iter = Inf (max. nr of iterations), 
# maxp = Inf (max. nr of parents for a node), 
# optimized = TRUE (whether or not to use score caching)

bn_structure = tabu(ff, score="bge")
plot(bn_structure)

dagitty_structure = bnlearn2dagitty(bn_structure)
plot(dagitty_structure)
