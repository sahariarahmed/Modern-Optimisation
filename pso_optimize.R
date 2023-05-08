### sphere-psoptim.R file ###
library(GA)
library(pso)
library(e1071) # load pso

dataDT <<- read.csv("C:\\creditcard1.csv")


cfeatureFitness <- function(string,xx,yy) {
  
  inc <- which(string >= 0.5)  #'inc' includes those features/variables for which 'string' contains 1
  
  if(sum(inc)==0)                          
    return(0)                          
  
  
  outcome <-"Class"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- rpart(formula = fRpart, method="class", control = rpart.control(minsplit = 3),
              data = dataDT)
  
  t_pred = predict(DT,dataDT, type='class')
  acc=mean(dataDT$Class == t_pred)
  #Maximise accuracy
  #return(-acc)
  print('accuracy = ')
  print(mean(dataDT$Class == t_pred))
  print('feature = ')
  print((1 - sum(string == 1)/length(string) ))
  #Maximise accuracy and minimise the number of features
  return( -(mean(dataDT$Class == t_pred) * (1 - sum(string == 1)/length(string) )) )
}



runPSO <-function(){
  
  noRuns<-30
  xx <- dataDT[,-ncol(dataDT)]
  yy <- dataDT[,ncol(dataDT)]
  print(ncol(xx))
  fitness = classfeatureFitness
  lower.bounds <- c(rep(0, ncol(xx)))
  upper.bounds <- c(rep(1, ncol(xx)))
  control <- list(trace=1,maxit=20,REPORT=1,trace.stats=1,s=50) 
  
  best_fitness <- matrix(nrow = 20, ncol = 30)
  bestfitnessPSO <<- 0
  bestSolutionPSO <<- NULL
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    pso.result <- psoptim(par = rep(0, ncol(xx)),lower = lower.bounds, upper = upper.bounds, fn = cfeatureFitness, 
                          control = control, xx=xx, yy=yy)
    best_fitness[,i]<- pso.result$stats$error
    #print("best value")
    #print(pso.result$value)
    if (bestfitnessPSO < abs(pso.result$value)){
      bestfitnessPSO <<- abs(pso.result$value)
      bestSolutionPSO <<- pso.result$par
    }
    print(bestfitnessPSO)
    print(bestSolutionPSO)
  }
  return(best_fitness) 
}  
getBestFitnessPSO<-function(){
  return(bestfitnessPSO)
}

getBestSolutionPSO<-function(){
  return(bestSolutionPSO)
}


parse_pso_result<-function(d, noRuns=30, noIter=30){
  data<-abs(d)
  pdata <- matrix(nrow = 30, ncol = 3)
  for (i in 1:noIter){
    pdata[i,1] = i
    pdata[i,2] = mean(data[i,])
    pdata[i,3] = 1.96*sd((data[i, -1]))/sqrt(noRuns)   #compute the length of error bar. 
  }
  
  return (pdata)
}  
