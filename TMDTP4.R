# Cargar librerías
install.packages("randomForest")
install.packages("xgboost")
install.packages("e1071")
install.packages("caret")
library(randomForest)
library(xgboost)
library(e1071)
library(caret)

# Cargar el dataset
load("lampone.RData")

# Definir variables
# X <- lampone[, !names(lampone) %in% "n_tipo"]  # Variables predictoras
# y <- lampone$n_tipo  # Variable objetivo

X <- lampone[, -c(1, 143, 144)]
y <- lampone[, c(143)]

X <- X[, apply(X, 2, var) != 0]

# Escalar los datos para SVM
X_scaled <- scale(X)
# lampone.feat <- lampone.feat[, apply(lampone.feat, 2, var) != 0]

# Dividir el dataset en entrenamiento y prueba
set.seed(123)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]
X_train_scaled <- X_scaled[train_index, ]
X_test_scaled <- X_scaled[-train_index, ]

# Resultados iniciales
results <- data.frame(Model = character(), Error = numeric(), stringsAsFactors = FALSE)

# 1. Random Forest
set.seed(123)
rf_model <- randomForest(X_train, y_train, ntree = 500, mtry = sqrt(ncol(X_train)))
rf_pred <- predict(rf_model, X_test)
rf_error <- mean(rf_pred != y_test)
results <- rbind(results, data.frame(Model = "Random Forest", Error = rf_error))

# 2. XGBoost
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = as.numeric(y_train) - 1)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test))
xgb_model <- xgboost(
    data = xgb_train,
    objective = "multi:softmax",
    num_class = length(unique(y_train)),
    nrounds = 100,
    eta = 0.1,
    max_depth = 6,
    verbose = 0
)
xgb_pred <- predict(xgb_model, xgb_test)
xgb_error <- mean(xgb_pred != (as.numeric(y_test) - 1))
results <- rbind(results, data.frame(Model = "XGBoost", Error = xgb_error))

# 3. SVM con Kernel RBF
svm_rbf_tune <- tune(
    svm,
    y,
    data = cbind(X_train_scaled, y_train),
    kernel = "radial",
    ranges = list(cost = 10^(-2:2), gamma = 10^(-3:1))
)
svm_rbf_model <- svm_rbf_tune$best.model
svm_rbf_pred <- predict(svm_rbf_model, X_test_scaled)
svm_rbf_error <- mean(svm_rbf_pred != y_test)
results <- rbind(results, data.frame(Model = "SVM (RBF Kernel)", Error = svm_rbf_error))

# 4. SVM con Kernel Polinomial
svm_poly_tune <- tune(
    svm,
    y,
    data = cbind(X_train_scaled, y_train),
    kernel = "polynomial",
    ranges = list(cost = 10^(-2:2), degree = 2:4, gamma = 10^(-3:1))
)
svm_poly_model <- svm_poly_tune$best.model
svm_poly_pred <- predict(svm_poly_model, X_test_scaled)
svm_poly_error <- mean(svm_poly_pred != y_test)
results <- rbind(results, data.frame(Model = "SVM (Polynomial Kernel)", Error = svm_poly_error))

# Comparar resultados
print(results)

# Graficar los resultados
barplot(
    results$Error,
    names.arg = results$Model,
    col = "skyblue",
    xlab = "Modelos",
    ylab = "Error de Clasificación",
    main = "Comparación de Modelos"
)
