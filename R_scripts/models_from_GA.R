# libraries
require(Metrics)
require(dplyr)
require(gaselect)
library(stringr)

## Creating the output dataframes
# Formulas
colec_formulas <- data.frame(matrix(ncol = 1, nrow = 0))
colnames(colec_formulas) <- "Formula"

# Metrics
colec_metricas <- data.frame(matrix(ncol = 11, nrow = 0))
colnames(colec_metricas) <- c("R2", "R2test", "AdjR2", "RMSE", "RMSEpred",
                              "MAE", "MAEpred", "Est_F", "DF1", "DF2", "SEED")

# Probably models
colec_prov_model <- data.frame(matrix(ncol = 12, nrow = 0))
colnames(colec_prov_model) <- colnames(colec_metricas)

## Setting number of cycles (10000 for GA and 2000 for bestsubsets)
nciclo <- xxx ## Set the number of cycles

for (ciclo in 1:nciclo) {
  
  randseed <- as.integer(runif(1, min = 0, max = 100000))
  VarGL <- as.matrix(Treino[, 5:length(Treino)])
  ctrl <- genAlgControl(populationSize = 250, numGenerations = 500, minVariables = 5,
                        maxVariables = 12, mutationProbability = 0.05)
  LimEvalu <- evaluatorLM(statistic = "r.squared")
  
  ## Performing GA for PCE (Training$PCE)
  result <- genAlg(Treino$PCE, VarGL, control = ctrl, evaluator = LimEvalu, seed = randseed)
  
  # Obtaining matrices with iteration models
  subsetsGA <- as.data.frame(result@subsets)
  row.names(subsetsGA) <- colnames(VarGL)
  
  # Metrics matrix for iteration models
  Matriz_Formulas <- data.frame(matrix(ncol = 1, nrow = 0))
  colnames(Matriz_Formulas) <- "Formula"
  # Unacceptable VIF matrix for a model
  ValorVIFNA <- data.frame(matrix(ncol = 1, nrow = 0))
  # Matrix of unacceptable p-values for a model
  ValorPNA <- data.frame(matrix(ncol = 1, nrow = 0))
  
  # Matrix with model metrics
  Matriz_Metricas <- data.frame(matrix(ncol = 11, nrow = 0))
  colnames(Matriz_Metricas) <- c("R2", "R2test", "AdjR2", "RMSE", "RMSEpred",
                                 "MAE", "MAEpred", "Est_F", "DF1", "DF2", "SEED")
  
  # Matrix with model formulas
  Matriz_Formulas <- data.frame(matrix(ncol = 1, nrow = 0))
  colnames(Matriz_Formulas) <- "Formula"
  
  #Generates a vector of length 15, but with null values. Vector used to identify the variables in the model
  Var_mod <- rep("", 15)
  ### Loop that gets the metrics for all generated models
  for (k in 1:length(result@rawFitness)){
    i <- 1
    str_conc <- "1" #String used to obtain the expression
    # This loop returns the expression of the k-th model within the iteration
    for(j in 1:nrow(subsetsGA)) {
      if(subsetsGA[j, k] == "TRUE") {
        Var_mod[i] <- row.names(subsetsGA[j, ])
        str_conc <- str_c(str_conc, Var_mod[i], sep = " + ")
        i <- i + 1
      }
    }
    formula <- as.formula(str_c("PCE ~ ", str_conc)) # Formula do modelo k
    Matriz_Formulas[k, ] <- str_c("PCE ~ ", str_conc)
    
    # Training and obtaining metrics (R2; Q2; R2test; RMSEs; MAEs)
    # Linear Models
    lmmod <- lm(formula, data=Treino)
    sum_lmmod <- summary(lmmod)
    
    # Training Set Metrics
    ## - R2, AdjR2, F, RMSE, MAE, p-valor, and, VIF  # R^2 of model
    R2 <- sum_lmmod$r.squared # R^2 of the modelo
    AdjR2 <- sum_lmmod$adj.r.squared # R^2 ajusted
    Est_F <- sum_lmmod$fstatistic # F statistic
    RMSE <- rmse(lmmod$fitted.values, Treino$PCE) # RMSE
    MAE <- mae(lmmod$fitted.values, Treino$PCE) # MAE
    VetorP <- sum_lmmod$coefficients[, 4] # p-value of variables
    VetorVIF <- vif(lmmod) # VIF of variables
    
    # Test Metrics
    ## - R2Pred, RMSEpred e MAEpred
    
    numtest <- sum((Teste$PCE - predict(lmmod, newdata = Teste))^2)
    dentest <- sum((Teste$PCE - mean(Treino$PCE))^2)
    R2test <- 1 - numtest/dentest
    RMSEpred <- rmse(predict(lmmod, newdata = Teste), Teste$PCE)
    MAEpred <- mae(predict(lmmod, newdata = Teste), Teste$PCE)
    
    ## - Filtering the models
    # Identifying VIF >= 5
    ValorVIFNA <- as.data.frame(which(VetorVIF >= 5))
    
    # Identifying p-value > 0.05
    ValorPNA <- as.data.frame(which(vetorP > 0.05))
    if(nrow(ValorPNA) == 0){
      if(nrow(ValorVIFNA) == 0){
        97
        if(length(which(colec_formulas == Matriz_Formulas[k, ])) == 0){
          Matriz_Metricas[k, ] <- c(R2, R2test, AdjR2, RMSE, RMSEpred, MAE, MAEpred,
                                    Est_F, randseed)
          colec_formulas <- rbind(colec_formulas, Matriz_Formulas[k, ])
          colec_metricas <- rbind(colec_metricas, Matriz_Metricas[k, ])
          Completo <- cbind(colec_metricas, colec_formulas)
          if(R2 >= 0.6){
            if(R2test >= 0.5) {
              colec_prov_model[(nrow(colec_prov_model) + 1), ] <- cbind(Matriz_Metricas[k,],
                                                                        Matriz_Formulas[k, ])
              write.csv(colec_prov_model, "Colecao_Provaveis_modelos.csv")
            }
          }
        }
      }
    }
    
    # - Printing progress
    
    n <- ncol(subsetsGA)
    progresso1 <- (k/n)*100
    progresso2 <- (ciclo/nciclo)*100
    print(str_c(k, "/", n, " (", progresso1, "%) - Ciclo: ", ciclo, "/", nciclo, "(", progresso2, "%) -
    nº de P.M.:", nrow(colec_prov_model)))
  }
  
  write.csv(colec_formulas, "Colecao_formulas.csv")
  write.csv(colec_metricas, "Colecao_metricas.csv")
  write.csv(Completo, "Conjunto_Completo.csv")
}