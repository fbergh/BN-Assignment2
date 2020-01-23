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