library(MASS)
library(cluster)
library(e1071)

# Step 2: Define a helper function to create and display confusion matrices with optimized matching
compare_clusters <- function(true_labels, predicted_clusters, title) {
    # Convert true labels to factors if they are not already
    true_labels <- as.factor(true_labels)
    
    # Create a confusion matrix
    confusion_matrix <- table(True = true_labels, Predicted = predicted_clusters)
    
    # Use matchClasses to find the optimal permutation of predicted clusters
    class.match <- matchClasses(as.matrix(confusion_matrix), method = "exact")
    
    # Reorder the confusion matrix using the optimal match
    optimized_confusion_matrix <- confusion_matrix[, class.match]
    
    # Calculate accuracy using the optimized confusion matrix
    accuracy <- sum(diag(optimized_confusion_matrix)) / sum(optimized_confusion_matrix)
    
    print(title)
    print(optimized_confusion_matrix)
    print("----------------------------------------------------")
}

# # Cargar el dataset
load("lampone.Rdata")

# summary(lampone)

# Separar las características y las clases
lampone.feat <- lampone[, -c(1, 143, 144)]
lampone.classes <- lampone[, c(1, 143)]

lampone.feat <- lampone.feat[, apply(lampone.feat, 2, var) != 0]

lampone.pca <- prcomp(lampone.feat, scale = TRUE)
biplot(lampone.pca, main = "Lampone PCA")


lampone.log <- log(lampone.feat)
lampone.scaled <- scale(lampone.log)

lampone.scaled <- lampone.scaled[, apply(lampone.scaled, 2, var) != 0]

lampone.scaled.pca <- prcomp(lampone.scaled, scale = FALSE)
lampone.pca.scaled <- scale(prcomp(lampone.log, scale = FALSE)$x)


Código R con ejemplo de como calcular el score de estabilidad de dos soluciones de clustering:
x<-iris[,-5]
n<-dim(x)[1]
#fijo el numero de clusters
k=3
#creo dos indices al azar y hago los clusters
ind1<-sample(n,0.9*n)
cc1<-kmeans(x[ind1,],k,nsta=10)$cluster
ind2<-sample(n,0.9*n)
cc2<-kmeans(x[ind2,],k,nsta=10)$cluster
#pongo los clusters de nuevo en longitud n - quedan 0 los puntos fuera del sample.
#Sumo 5 a las etiquetas para que valga el truco que la raiz de multiplicar las "clases" es un numero entero solo cuando tienen el
mismo numero, vale para la cantidad de clusters que buscamos siempre.
v1<-v2<-rep(0,n)
v1[ind1]<-cc1+5
v2[ind2]<-cc2+5
#creo una matriz m con 1 donde los dos puntos estan en el mismo cluster, -1 en distinto cluster y 0 si alguno no esta, para cada
clustering
a<-sqrt(v1%*%t(v1))
m1<-a / -a + 2*(a==round(a))
m1[is.nan(m1)]<-0
a<-sqrt(v2%*%t(v2))
m2<-a / -a + 2*(a==round(a))
m2[is.nan(m2)]<-0
#calculo el score, los pares de puntos que estan en la misma situacion en los dos clustering dividido el total de pares validos.
validos<-sum(v1*v2>0)
score<-sum((m1*m2)[upper.tri(m1)]>0)/(validos*(validos-1)/2)
print(score)


#Funcion para generar n datasets con distribuciones uniformes para usar de referencia
crea.referencias <- function(original.dataset, n){
    #Dimensiones del dataset
    rows <- dim(original.dataset)[1]
    cols <- dim(original.dataset)[2]

    #Calculamos la pca
    pca <- prcomp(original.dataset)
    #Obtenemos los rangos de las variables, range me da el minimo y el maximo
    rangos <- apply(pca$x, 2, range)

    #Dataset de referencia, tiene que ser una lista para soportar las multiples dimensiones
    reference.datasets <- list()

    #Generamos los n datasets de referencia
    for(i in 1:n){
        #Generamos un dataset de referencia vacio
        reference.dataset <- c()

        #Generamos las columnas de uno de los datasets
        for(j in 1 : min(rows, cols)){
			    uniformColumn = runif(rows, rangos[1, j], rangos[2, j])
			    reference.dataset = cbind(reference.dataset, uniformColumn)
		    }	
        
        #Añadimos el dataset de referencia a la lista
        reference.datasets[[i]] <- reference.dataset
    }

    return (reference.datasets)
}


gap.statistic <- function(dataset, maxK, B){
    #Vector Wkb de las referencias
    Wkb.star <- double(maxK)
    #Vetor para almacenar el gap
    gap <- double(maxK)
    #Vector para las desviaciones estandar
    sk <- double(maxK)
    uniform.references <- crea.referencias(dataset, B)

    for(k in 2:maxK){
        #Medida de dispersion de cluster de tamaño k
        Wk <- kmeans(dataset, k)$tot.withinss
        #Busco las medidas de dispersion de hacer kmeans de cada referencia
        for(b in 1:B){
            Wkb.star[b] <- kmeans(uniform.references[[b]],k)$tot.withinss
        }
        #Obtenemos la gap statistic
        gap[k] <- (1/B) * sum(log(Wkb.star)) - log(Wk)
        lhat <- (1/B) * sum(log(Wkb.star))
        sd.k <- sqrt(1/B * sum((log(Wkb.star) - lhat)^2))
        sk[k] <- sd.k * sqrt(1 + 1/B)
    }

    #Buscamos la cantidad de clusters adecuada según la desigualdad
    suggested.clusters <- 2
	  while(suggested.clusters < maxK && 
	      gap[suggested.clusters] < gap[suggested.clusters + 1] - sk[suggested.clusters + 1]){
          
		suggested.clusters = suggested.clusters + 1
	  }
    return(suggested.clusters)
}



#Funcion magica provista por Pablo Granitto
stability.score <- function(n, ind1, ind2, cc1, cc2){
    v1 <- v2 <- rep(0,n)
    v1[ind1] <- cc1
    v2[ind2] <- cc2
    #creo una matriz m con 1 donde los dos puntos estan en el mismo cluster, -1 en distinto cluster y 0 si alguno no esta, para cada clustering
    a <- sqrt(v1%*%t(v1))
    m1 <- a / -a + 2*(a==round(a))
    m1[is.nan(m1)]<- 0
    a <- sqrt(v2%*%t(v2))
    m2 <- a / -a + 2*(a==round(a))
    m2[is.nan(m2)] <- 0
    #calculo el score, los pares de puntos que estan en la misma situacion en los dos clustering dividido el total de pares validos.
    validos <- sum(v1*v2>0)
    score <- sum((m1*m2)[upper.tri(m1)]>0)/(validos*(validos-1)/2)
    return(score)
}

stability.method <- function(dataset, k, nRep){
    #Cantidad de filas
    rows <- dim(dataset)[1]

    #Generamos nRep muestras del dataset subsampleadas al 90%
    subsamples <- list()
    for(i in 1:nRep){
        subsamples[[i]] <- sample(rows, rows*0.9)
    }

    #Calculamos el valor de estabilidad segun la cantidad de clusters
    stability <- list()
    stability.mean <- c()
    for(cluster.number in 2:k){
        #Calculamos el clustering para cada subsample
        kmeans.subsamples <- list()
        for(r in 1:nRep){
            kmeans.subsamples[[r]] <- kmeans(dataset[subsamples[[r]], ], cluster.number)$cluster
        }

        #Calculamos la estabilidad comparando cada referencia creada
        stability.scores <- c()
        for(ind1 in 1 : (nRep - 1)){
            for(ind2 in (ind1 + 1) : nRep){
                stability.scores <- c(stability.scores, stability.score(rows, subsamples[[ind1]], subsamples[[ind2]], kmeans.subsamples[[ind1]], kmeans.subsamples[[ind2]]))
            }
        }
        stability[[cluster.number]] <- stability.scores
        #Sacamos la media de los scores de estabilidad para obtener la cantidad preferida de clusters (tambien se puede hacer viendo histogramas)
        stability.mean <- c(stability.mean, mean(stability.scores))
    }
    l <- list(stability = stability, mean.stability = stability.mean)
    return(l) 
}



# Generar n datasets de distribucion uniforme basados en un PCA del original
generate_reference_datasets <- function(original_dataset, n) {
    rows <- nrow(original_dataset)
    cols <- ncol(original_dataset)

    pca <- prcomp(original_dataset)

    ranges <- apply(pca$x, 2, range)
    reference_datasets <- vector("list", n)

    for (i in 1:n) {
        # Initialize a matrix to store a single reference dataset
        reference_dataset <- matrix(0, nrow = rows, ncol = min(rows, cols))

        # Populate each column with uniform values within the component ranges
        for (j in 1:min(rows, cols)) {
            reference_dataset[, j] <- runif(rows, ranges[1, j], ranges[2, j])
        }

        reference_datasets[[i]] <- reference_dataset
    }

    return(reference_datasets)
}

gap_statistic <- function(dataset, max_clusters, num_references) {
    gap <- numeric(max_clusters)
    sk <- numeric(max_clusters)

    # Generate B reference datasets
    reference_datasets <- generate_reference_datasets(dataset, num_references)

    # Iterate over each possible number of clusters
    for (k in 2:max_clusters) {
        # Calculate within dispersion for the dataset
        Wk <- kmeans(dataset, centers = k)$tot.withinss

        # Calculate within dispersion for all references
        Wk_references <- sapply(reference_datasets, function(ref) kmeans(ref, centers = k)$tot.withinss)

        # Compute the Gap Statistic for k clusters
        gap[k] <- mean(log(Wk_references)) - log(Wk)

        # Compute the standard deviation for Wk references
        sd_k <- sqrt(mean((log(Wk_references) - mean(log(Wk_references)))^2))
        sk[k] <- sd_k * sqrt(1 + 1 / num_references)
    }

    # Determine the optimal number of clusters by checking the Gap inequality
    optimal_clusters <- 2
    while (optimal_clusters < max_clusters &&
           gap[optimal_clusters] < gap[optimal_clusters + 1] - sk[optimal_clusters + 1]) {
        optimal_clusters <- optimal_clusters + 1
    }

    return(optimal_clusters)
}


Código R con ejemplo de como calcular el score de estabilidad de dos soluciones de clustering:
x<-iris[,-5]
n<-dim(x)[1]
#fijo el numero de clusters
k=3
#creo dos indices al azar y hago los clusters
ind1<-sample(n,0.9*n)
cc1<-kmeans(x[ind1,],k,nsta=10)$cluster
ind2<-sample(n,0.9*n)
cc2<-kmeans(x[ind2,],k,nsta=10)$cluster
#pongo los clusters de nuevo en longitud n - quedan 0 los puntos fuera del sample.
#Sumo 5 a las etiquetas para que valga el truco que la raiz de multiplicar las "clases" es un numero entero solo cuando tienen el
mismo numero, vale para la cantidad de clusters que buscamos siempre.
v1<-v2<-rep(0,n)
v1[ind1]<-cc1+5
v2[ind2]<-cc2+5
#creo una matriz m con 1 donde los dos puntos estan en el mismo cluster, -1 en distinto cluster y 0 si alguno no esta, para cada
clustering
a<-sqrt(v1%*%t(v1))
m1<-a / -a + 2*(a==round(a))
m1[is.nan(m1)]<-0
a<-sqrt(v2%*%t(v2))
m2<-a / -a + 2*(a==round(a))
m2[is.nan(m2)]<-0
#calculo el score, los pares de puntos que estan en la misma situacion en los dos clustering dividido el total de pares validos.
validos<-sum(v1*v2>0)
score<-sum((m1*m2)[upper.tri(m1)]>0)/(validos*(validos-1)/2)
print(score)
