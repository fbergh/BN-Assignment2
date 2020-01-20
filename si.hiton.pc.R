################
### PACKAGES ###
################

library(bnlearn)


####################
### LOADING DATA ###
####################

# Read the data from the csv file
ff = read.csv("data/ff_preprocessed.csv", header = TRUE, colClasses = rep("double", 11))

# Show head of dataframe
head(ff)


###################
### SI.HITON.PC ###
###################
# See https://books.google.nl/books?id=Jp4MBwAAQBAJ&pg=PA30&lpg=PA30

result = si.hiton.pc(ff, alpha=0.01)
plot(result)
