library(GA)

monitor <- function(obj){
                  
  iter <- obj@iter                       
  if (iter <= maxGenerations){          
    fitness <- obj@fitness              

    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }  
  else{                               
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}

runGA <- function(noRuns = 30, problem = "feature"){

  if (problem == "feature"){
    maxGenerations <<- 20    
    popSize = 40
    pcrossover = 0.8
    pmutation = 0.1
    type = "binary"
    crossover = 
    data <- getData()
    xx <- data[,-ncol(data)]
    yy <- data[,ncol(data)]
    fitness = classfeatureFitness             
  }
  else if (problem == "tsp"){
    maxGenerations <<- 50
  
    popSize = 50
    pcrossover = 0.8
    pmutation = 0.2
    type = "permutation"
    data = getData()
    min = 1                             
    max = nrow(getData())               
    fitness = classfeatureFitness                
  }
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
  
  
    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) 
  resultsMatrix = matrix(1:maxGenerations, ncol = 1) 
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    if (problem == "feature")
      GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx), 
               names = colnames(xx), seed=i, popSize = popSize, 
               pcrossover = pcrossover, pmutation = pmutation, 
               maxiter = maxGenerations, monitor= monitor)
    else if (problem == "tsp")
      GA <- ga(type = type, fitness = fitness, distMatrix = data, 
                     min = min, max = max, popSize = popSize, maxiter = maxGenerations,
                     pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
    else if (problem == "vehicle")
      GA <- ga(type = type, fitness = fitness, 
               min = min, max = max, popSize = popSize, maxiter = maxGenerations,
               pcrossover=pcrossover, pmutation = pmutation, monitor= monitor, seed = i )
      
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (resultsMatrix)
}

getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}

