# Uncomment and run the next line if it NetworkDistance is not installed
# install.packages("NetworkDistance")
library(NetworkDistance)
library(dagitty)
library(bnlearn)

# Convert a "bn class" object from bnlearn to a dagitty object
# Undirected edges in the "bn class" object are represented in the dagitty object by a <-> connection
bnlearn2dagitty = function(structure) {
    dagitty_str = 'dag{
                    area [pos="0,4"]
                    ISI [pos="-1,3"]
                    weekend [pos="1,3"]
                    FFMC [pos="-1,2"]
                    DMC [pos="0,2"]
                    DC [pos="1,2"]
                    wind [pos="-2,1"]
                    temp [pos="-1,1"]
                    RH [pos="0,1"]
                    avg_wind [pos="-2,0"]
                    avg_temp [pos="-0.5,0"]'
    for (row in 1:nrow(structure$arcs)) {
        arc = structure$arcs[row,]
        dagitty_str = paste(dagitty_str, arc[1], " -> ", arc[2])
    }
    dagitty_str = paste(dagitty_str, "}")
    g_dag = dagitty(dagitty_str)
    g_dag
}

# Runs the tabu and si.hiton.pc algorithms with specified parameters
# The function returns an object with the resulting bnlearn structures, the corresponding adjacency matrices, and the evaluation metrics (betweenness, degree, and hamming). These are accessible by indexing the resulting object with $[key]
run_experiment = function(data, algorithms=c("tabu","hiton"), 
                          exp_values=list(c("loglik-g","bic-g","bge"),c(0.01,0.05,0.1,0.25)), is_plot=FALSE) {
    bn_structures = vector("list",length(exp_values[1])+length(exp_values[2]))
    adjacency_mats = vector("list",length(exp_values[1])+length(exp_values[2]))
    
    # Loop over both algorithms and their experimental values and save the resulting networks
    index_1d = 1 # exp_values is 2D so keep track of 1D index for bn_structures/adjacency_mats
    for (i in 1:length(algorithms)) {
        for (j in 1:length(exp_values[[i]])) {
            bn_structures[[index_1d]] = if (algorithms[i]=="hiton") si.hiton.pc(data, alpha=exp_values[[i]][j]) 
                                        else                        tabu(data, score=exp_values[[i]][j])
            adjacency_mats[[index_1d]] = amat(bn_structures[[index_1d]])
            
            if(is_plot) {
                png(filename=paste("img/net-",algorithms[i],"-",exp_values[[i]][j],".png",sep=""))
                plot(bnlearn2dagitty(bn_structures[[index_1d]]))
                dev.off()
            }
            index_1d = index_1d + 1
        }
    }
    
    # Compute evaluation metrics hamming (see: https://en.wikipedia.org/wiki/Hamming_distance), 
    #                            betweenness and degree (see: https://en.wikipedia.org/wiki/Centrality)
    # Set out.dist=FALSE to obtain full distance matrices, instead of shortened once
    betweenness = nd.centrality(adjacency_mats,mode="Between",out.dist=FALSE)
    degree = nd.centrality(adjacency_mats,mode="Degree",out.dist=FALSE)
    hamming = nd.hamming(adjacency_mats,out.dist=FALSE)
    
    # Print and return output
    print(paste("The first",length(exp_values[[1]]),"entries/indices belong to tabu, the rest belong to si.hiton.pc"))
    print("--- BETWEENNESS ---");print(betweenness)
    print("--- DEGREE ---");print(degree)
    print("--- HAMMING ---");print(hamming)
    
    return_object = list("bn_structures"=bn_structures, "adjacency_mats"=adjacency_mats, 
                         "betweenness"=betweenness, "degree"=degree, "hamming"=hamming)
    return_object
}