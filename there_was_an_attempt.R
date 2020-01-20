library(dagitty)

# Read the data from the csv file
ff = read.csv("data/ff_preprocessed.csv", header = TRUE, colClasses = rep("double", 11))

# Create unconnected graph
g_unconnected = dagitty('
            dag{
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
                avg_temp [pos="-0.5,0"]
            }
            ')
plot(g_unconnected)

# Create list of all pairs of variables that are significantly & notably correlated
lt = localTests(g_unconnected,ff)
corr_lt = subset(lt, p.value<0.05 & abs(estimate)>0.1)[order(abs(corr_lt$estimate)),]; corr_lt
pairs = strsplit(rownames(corr_lt), " _\\|\\|_ ")


# Create graph from Assignment 1
g_final = dagitty('
            dag{
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
                avg_temp [pos="-0.5,0"]
                ISI -> area
                DMC -> area
                DC -> area
                weekend -> area
                wind -> ISI
                FFMC -> ISI
                wind -> FFMC
                avg_temp -> FFMC
                RH -> FFMC
                avg_temp -> DMC 
                RH -> DMC
                avg_wind -> DC
                avg_temp -> DC
                temp <-> wind
                temp -> RH
                avg_temp -> RH
                avg_temp -> temp
                avg_wind -> wind
                avg_temp <-> avg_wind
            }
            ')
e = edges(g_final)


# Function for scoring graphs: number of edges that match the highly correlated pairs (normalized)
eval_function <- function(graph, list){
  e = edges(g_final)
  v = e$v
  w = e$w
  score = 0
  for(i in 1:dim(e)[1]){
    for(row in pairs){
      if(identical(row, c(toString(e$v[i]), toString(e$w[i]))) | 
         identical(row, c(toString(e$w[i]), toString(e$v[i])))){
        score = score + 1
      }
    }
  }
  score/length(list)
}

# Run eval function on graph from Assignment 1
eval_function(g_final, pairs)


# Another idea: compare graph to unconnected graph... but I don't know how
