getData <- function(){
  #data(iris)
  dataDT <<- read.csv('C:\\creditcard1.csv')
  #DTdata <- cbind(iris[,-ncol(iris)],iris[,-ncol(iris)],  iris)
  #names(DTdata) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Time",	"V1",	"V2",	"V3",	"V4",	"V5",	"V6",	"V7",	"V8",	"V9",	"V10",	"V11",	"V12",	"V13",	"V14",	"V15",	"V16",	"V17",	"V18",	"V19",	"V20",	"V21",	"V22",	"V23", "V24",	"V25",	"V26",	"V27",	"V28",	"Amount",	"Class")
  return (dataDT)
  
}

classfeatureFitness <- function(string,xx,yy) {
 
  inc <- which(string == 1)  #'inc' includes those features/variables for which 'string' contains 1
 
  if(sum(inc)==0)                          
  return(0)                         
  
   
  outcome <-"Class"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- rpart(formula = fRpart, method="class", control = rpart.control(minsplit = 3),
              data = dataDT)
  
  t_pred = predict(DT,dataDT, type='class')
  
  #Maximise accuracy
  return( mean(dataDT$Class == t_pred))
  
  #Maximise accuracy and minimise the number of features
  #return( mean(dataDT$Species == t_pred) * (1 - sum(string == 1)/length(string) ) )
}

