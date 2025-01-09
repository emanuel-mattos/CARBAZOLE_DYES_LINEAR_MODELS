# dados
Teste <- read.csv('path_to_test', row.names=1, sep=";") # put the path of test set in 'path_to_test'

Treino <- read.csv('path_to_train', row.names=1, sep=";") # put the path of test set in 'path_to_test'


Descri_est <- Treino[,1:529]
Descri_esp <- Treino[,530:length(Treino)]

require(faraway)
require(Metrics)
library(stringr)

## --- Iterations and Seed of the 'real' model
seed_real_mod <- as.integer(runif(1, min = 0, max = 100000)) # Set value
n_iterac <- 50 ## Defining number of iterations

## --- Searching for Models

Metricas_Y_rand <- data.frame(matrix(ncol = 4, nrow = 0))

colnames(Metricas_Y_rand) <-  c("R2", "Q2_LOO", "Cor_rand_Orig", "SEED")
Metricas_Y_rand[1,] <- c('r2', 'q2', 1, 0) ## Enter R2, Q2 of unrandomized model in 'r2' and 'q2'
Y_Original <- Treino$Delta_G_inj##

## --- Performing the iterations
for(ciclo in 1:n_iterac){
  
  ## --- Radomizing the Y vector  
  seed_rand_Y <- runif(1, min = 1, max = 100000) ## Seed for randomization
  
  Y_rand <- Treino$Delta_G_inj ## 
  set.seed(seed_rand_Y) ## 
  Y_rand <- sample(Y_rand) ## 
  Treino$Delta_G_inj <- sample(Y_rand)
  
  Corr_rand_Original <- cor(Y_Original, Y_rand)
  
  ## -- Seleção dos subset
  
  lmmod1 <- lm('model', Treino) #set the model in 'model'
  sum_lmmod1 <- summary(lmmod1)
  Matriz_Metricas_Y_rand <- data.frame(matrix(ncol = 4, nrow = 0))
  Matriz_Metricas_Y_rand[1,] <- c(rep(1, 4))
  colnames(Matriz_Metricas_Y_rand) <- c("R2", "Q2_LOO", "Cor_rand_Orig", "SEED")
  
  
  R2_Y_rand <- sum_lmmod1$r.squared  ## R^2 
  
  ### -----  get the Q^2_LOO
  
  Metricas_loo_y_rand <- as.data.frame(matrix(ncol = 3, nrow = 0))
  colnames(Metricas_loo_y_rand) <- c("Y_Pred", "Pred_Error", "Pred_Resid_Square")
  Pred_Error <- 0
  Pred_Resid_Square <- 0
  for(sample in 1:nrow(Treino)){
    
    vet_loo <- Treino[-sample,]
    mod_loo <- lm('model', vet_loo) #set the model in 'model'
    
    test_loo <- Treino[sample,]
    
    Y_Pred_y_rand <- predict(mod_loo, newdata = test_loo)
    Pred_Error_y_rand <- (test_loo$Delta_G_inj - Y_Pred_y_rand)
    Pred_Resid_Square_y_rand <- Pred_Error_y_rand^2
    Metricas_loo_y_rand[sample, ] <- c(Y_Pred_y_rand, Pred_Error_y_rand, Pred_Resid_Square_y_rand)
    
  }
  
  PRESS_y_rand <- sum(Metricas_loo_y_rand[,3])
  den_y_rand <- sum((Treino$Delta_G_inj - mean(Treino$Delta_G_inj))^2)
  q2_y_rand <- 1 - PRESS_y_rand/den_y_rand
  
  Metricas_Y_rand[nrow(Metricas_Y_rand) + 1,] <- c(R2_Y_rand, q2_y_rand, Corr_rand_Original, seed_rand_Y)
  
  write.csv(Metricas_Y_rand, "Resultado_Y_rand.csv")
  
  ## - Printing progress  
  progresso1 <- (ciclo/n_iterac)*100
  print(str_c(ciclo, "/", n_iterac, " (", progresso1, "%)"))
  
}
