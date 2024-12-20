library(ggplot2)
library(rpart)
library(class)
library(MASS)
install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)
install.packages("kernlab")
library(kernlab)
install.packages("gridExtra")
library(gridExtra)

#---------------------------------------------------------------------------
#random forest error estimation (OOB) for greedy search
#---------------------------------------------------------------------------
rf.est <- function(x.train,y,equalize.classes=TRUE,tot.trees=500,mtry=0)
{
	if(mtry<1) mtry<-floor(sqrt(dim(x.train)[2]))
	prop.samples<-table(y)
	if(equalize.classes) prop.samples<-rep(min(prop.samples),length(prop.samples))
	return( randomForest(x.train,y,mtry=mtry,ntree=tot.trees,sampsize=prop.samples)$err.rate[tot.trees] )
}

#---------------------------------------------------------------------------
#LDA error estimation (LOO) for greedy search
#---------------------------------------------------------------------------
lda.est <- function(x.train,y)
{
	m.lda <- lda(x.train,y,CV=TRUE)
	return(error.rate( y , m.lda$class))
}
error.rate <- function(dataA, dataB) sum( dataA != dataB ) / length(dataB)

#---------------------------------------------------------------------------
#SVM error estimation (internal CV) for greedy search
#---------------------------------------------------------------------------
svm.est <- function(x.train,y,type="C-svc",kernel="vanilladot",C=1,cross = 4)
{
  invisible(capture.output(model <- ksvm(as.matrix(x.train), as.factor(y), type=type,kernel=kernel,C=C,cross = cross)))
  return ( model@cross )
}


#---------------------------------------------------------------------------
#random forest ranking method for rfe.
#---------------------------------------------------------------------------
imp.rf <- function(x.train,y,equalize.classes=TRUE,tot.trees=500,mtry=0)
{
	if(mtry<1) mtry<-floor(sqrt(dim(x.train)[2]))
	prop.samples<-table(y)
	if(equalize.classes) prop.samples<-rep(min(prop.samples),length(prop.samples))

	m.rf<-randomForest(x.train,y,ntree=tot.trees,mtry=mtry,sampsize=prop.samples,importance=TRUE)
	imp.mat<-importance(m.rf)
	imp.col<-dim(imp.mat)[2]-1
	rank.list<-sort(imp.mat[,imp.col],decreasing=FALSE,index=T)
	return(list(feats=rank.list$ix,imp=rank.list$x))
}


#---------------------------------------------------------------------------
#linear svm ranking method for rfe. Using kernlab. Multiclass
#---------------------------------------------------------------------------
imp.linsvm <- function(x.train,y,C=100)
{
  num.feat<-dim(x.train)[2]
  tot.problems<-nlevels(y)*(nlevels(y)-1)/2

  invisible(capture.output(m.svm <- ksvm(as.matrix(x.train), y, type="C-svc",kernel="vanilladot",C=C)))

  w<-rep(0.0,num.feat)
  for(i in 1:tot.problems) for(feat in 1:num.feat)
      w[feat]<-w[feat]+abs(m.svm@coef[[i]] %*% m.svm@xmatrix[[i]][,feat])
  rank.list<-sort(w,decreasing=FALSE,index=T)
  return(list(feats=rank.list$ix,imp=rank.list$x))
}

forward.ranking <- function(x,y,method,verbosity=0,... )
{

	max.feat<-dim(x)[2]		#total de features
	num.feat<-1				#numero actual de features
	list.feat<-1:max.feat   #lista para guardar los features elegidos, inicializo como llegaron

	#ranking inicial: elijo la variable con menor error de prediccion
	class.error<-double(max.feat)   #inicializo el vector para guardar el error de cada modelo
	#para cada i, creo el dataset con esa variable sola, entreno un modelo y le mido el error, que lo guardo en class.error[i]
	for(i in 1:max.feat){
		x.train<-x[,i,drop=F]
		class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
	}
	#guardo la variable con minimo error como primera en mi lista de elegidas. Guardo una lista keep.feat con las que me quedan para seguir eligiendo.
	list.feat[1]<-which.min(class.error)
	keep.feat<-sort(class.error,decreasing=FALSE,index=T)$ix[-1]

	if(verbosity>1) cat("\nFirst feature: ",list.feat[1],"\n")

    #loop principal. A cada paso agrego todas las variables disponibles, de a una, le mido el error y me quedo con la de minimo error. Hasta llegar a meter todas.
	while(num.feat<max.feat){
        #class.error guarda el error de cada modelo. Son max.feat-num.feat modelos.
		class.error<-double(max.feat-num.feat)
		#para cada variable que me queda, la agrego a la lista del paso anterior, entreno el modelo y le mido el error.
		for(i in 1:(max.feat-num.feat)){
		    features<-c(list.feat[1:num.feat],keep.feat[i])
			x.train<-x[,features]
			class.error[i] <- do.call(method, c(list(x.train, y), list(...)) )
		}
		if(verbosity>2) cat("\nFeatures:\n",keep.feat,"\nErrors:\n",class.error)
		#me quedo con el modelo de minimo error, guardo ese feature en la lista de las elegidas, lo saco de la lista de las que quedan.
		best.index<-which.min(class.error)
		list.feat[num.feat+1]<-keep.feat[best.index]
		if(verbosity>1) cat("\n---------\nStep ",1+num.feat,"\nFeature ",best.index)

		keep.feat<-keep.feat[-best.index]
		if(verbosity>2) cat("\nNew search list: ",keep.feat)
		num.feat<-num.feat+1 #proximo paso
	}

	if(verbosity>1){
		cat("\n---------\nFinal ranking ",num.feat," features.")
		cat("\nFeatures: ",list.feat,"\n")
	}

 	return(list.feat)

}

backward.ranking <- function(x, y, method, verbosity = 0, ...) {

  max.feat <- dim(x)[2]    # Total number of features
  list.feat <- 1:max.feat  # Initialize the list of features
  keep.feat <- double(max.feat)  # Store the order of eliminated features

  # Main loop: Keep removing the least important feature until only one is left
  while(length(list.feat) > 1){

    # Initialize a vector to store the error for each model (one per feature)
    class.error <- double(length(list.feat))

    # Train models by removing one feature at a time and compute error
    for(i in 1:length(list.feat)){
      x.train <- as.matrix(x[, list.feat[-i]])
      class.error[i] <- do.call(method, c(list(x.train, y), list(...)))
    }

    # Find the feature whose removal results in the smallest increase in error
    worst.feat <- which.min(class.error)

    # Store this feature as the next to be removed
    keep.feat[length(list.feat)] <- list.feat[worst.feat]

    # Remove this feature from the list of remaining features
    list.feat <- list.feat[-worst.feat]

    # Verbose output if needed
    if(verbosity > 1) {
      cat("\nStep ", max.feat - length(list.feat), "\nRemoved Feature: ", worst.feat, "\n")
    }
  }

  # The last remaining feature is the most important
  keep.feat[1] <- list.feat

  # Return the final result
  return(keep.feat)
}

kruskal.ranking <- function(x, y, verbosity = 0) {

  max.feat <- dim(x)[2]  # Total number of features
  p.values <- double(max.feat)  # Vector to store p-values

  # Calculate Kruskal-Wallis p-values for each feature
  for (i in 1:max.feat) {
    p.values[i] <- kruskal.test(x[, i], y)$p.value

    # Print verbose output if required
    if (verbosity > 1) {
      cat("Feature", i, "- p-value:", p.values[i], "\n")
    }
  }

  # Rank the features based on their p-values (ascending order)
  list.feat <- order(p.values)

  # Verbose output to print the final ranking
  if (verbosity > 1) {
    cat("\nFinal ranking of features based on Kruskal-Wallis test:\n")
    cat("Features ranked (by index):", list.feat, "\n")
  }

  # Return the ranked features
  return(list.feat)
}

rfe.ranking <- function(x, y, method, verbosity = 0, ...) {

  max.feat <- dim(x)[2]  # Total number of features
  list.feat <- 1:max.feat  # Initialize the list of all features
  rank.list <- double(max.feat)  # To store the ranking of features

  # Main loop: Remove one feature at a time based on the method
  for (i in 1:max.feat) {
    # Train the model using the remaining features
    x.train <- as.matrix(x[, list.feat])

    # Use the specified method to rank the features
    rank_ <- do.call(method, c(list(x.train, y), list(...)))

    # Get the feature with the lowest importance
    last <- rank_$feats[1]

    # Store the removed feature in the rank list
    rank.list[max.feat - i + 1] <- list.feat[last]

    # Remove the least important feature from the list
    list.feat <- list.feat[-last]

    # Verbose output for intermediate steps
    if (verbosity > 1) {
      cat("Step", i, "- Removed feature:", last, "\n")
    }
  }

  # Verbose output to print the final ranking
  if (verbosity > 1) {
    cat("\nFinal ranking of features using RFE:\n")
    cat("Features ranked (by index):", rank.list, "\n")
  }

  # Return the ranked features
  return(rank.list)
}

diagonales <- function(n, d, C) {
  half <- floor(n / 2)

  mClass0 <- rep(-1, d)
  mClass1 <- rep(1, d)
  covM <- diag(C^2 * d, d)

  class0 <- MASS::mvrnorm(n = half, mu = mClass0, Sigma = covM)
  class1 <- MASS::mvrnorm(n = n - half, mu = mClass1, Sigma = covM)

  labels0 <- matrix(0, nrow = half, ncol = 1)
  labels1 <- matrix(1, nrow = n - half, ncol = 1)

  data0 <- cbind(class0, labels0)
  data1 <- cbind(class1, labels1)

  data <- rbind(data0, data1)

  columns <- c(paste0("dim_", 1:d), "class")
  dataFrame <- as.data.frame(data)
  colnames(dataFrame) <- columns
  dataFrame$class <- as.factor(dataFrame$class)

  return(dataFrame)
}

datosDiagRuido <- function() {
  # Generar el dataset con 50 puntos por clase y 10 variables
  datosDiag <- diagonales(100, 10, 2)

  # Agregar 90 variables de ruido (uniforme entre -1 y 1)
  n <- nrow(datosDiag)  # Número de filas (100 puntos en total)
  ruido <- matrix(runif(n * 90, min = -1, max = 1), nrow = n, ncol = 90)

  # Convertir el ruido a un data frame y asignar nombres de columnas
  colnames(ruido) <- paste0("r_", 1:90)

  # Combinar el dataset original con las variables de ruido
  datosDiag_ruido <- cbind(datosDiag[,-11], ruido, class = datosDiag[,11])

  return (datosDiag_ruido)
}

# Inicializamos la matriz donde guardaremos los resultados
n_iters <- 30  # Número de iteraciones
p <- 10        # Número de características relevantes
results_matrix <- matrix(0, nrow = n_iters, ncol = 7)  # Matriz para almacenar rankings (7 métodos)

# Iteramos 30 veces
for (i in 1:n_iters) {
  # Generamos un nuevo conjunto de datos
  datos <- datosDiagRuido()
  datos.x <- datos[,-101]
  datos.y <- datos[,101]

  results_matrix[i, 1] <- sum(forward.ranking(datos.x, datos.y, "rf.est",tot.trees=100,equalize.classes=F)[1:10] <= p)
  results_matrix[i, 2] <- sum(forward.ranking(datos.x, datos.y, method="svm.est")[1:10] <= p)
  results_matrix[i, 3] <- sum(backward.ranking(datos.x, datos.y, "rf.est",tot.trees=100,equalize.classes=F)[1:10] <= p)
  results_matrix[i, 4] <- sum(backward.ranking(datos.x, datos.y, method="svm.est")[1:10] <= p)
  results_matrix[i, 5] <- sum(kruskal.ranking(datos.x, datos.y)[1:10] <= p)
  results_matrix[i, 6] <- sum(rfe.ranking(datos.x, datos.y, "imp.rf",tot.trees=100,equalize.classes=F)[1:10] <= p)
  results_matrix[i, 7] <- sum(rfe.ranking(datos.x, datos.y, method="imp.linsvm")[1:10] <= p)
  cat("Iteracion: ", i, "\n")
}

columns <- c("F RF", "F SVM", "B RF", "B SVM", "K", "RFE RF", "RFE LINSVM")
dfEj3 <- as.data.frame(results_matrix)
colnames(dfEj3) <- columns

save(dfEj3, file = "dfEj3.RData")