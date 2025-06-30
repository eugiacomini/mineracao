str(dados)

matriz_x = dados[,-c(1, ncol(dados))]
names(matriz_x) = paste0("X", seq(1:length(names(matriz_x))))
dados_novo = cbind("y" = as.factor(dados$class), matriz_x)
View(dados_novo)

#Analise Descritiva
boxplot(dados_novo)

#Variavel 31 indica vazamento de dados
table(matriz_x$X31)
x31_nova = matrix(0, nrow = nrow(matriz_x), ncol = 1)
x31_nova[which(matriz_x[,31]!=0),] = 1
x31_nova
sum(x31_nova==y)==nrow(matriz_x)
matriz_x_nova = matriz_x[,-31]

boxplot(matriz_x_nova)
det(cor(matriz_x_nova))
round(eigen(cor(matriz_x_nova))$values/sum(eigen(cor(matriz_x_nova))$values), 4)*100

dados_novo_sem31 = cbind(y, matriz_x_nova)
View(dados_novo_sem31)

#Verificando normalidade das variaveis
p = vector()
for(i in 1:ncol(matriz_x_nova)){
  p[i] = shapiro.test(matriz_x_nova[,i])$p
}
round(p, 3)
which(p<0.05)

divide_treino_teste_valida_estratificado = function(vetor_y, pesos = c(60, 30, 10)) {
  indices_1 = sample(which(vetor_y==1))
  indices_0 = sample(which(vetor_y==0))
  
  indices_treino_0 = indices_0[1:round(pesos[1]*length(indices_0)/100)]
  indices_treino_1 = indices_1[]
  indices_treino = c(indices_treino_0, indices_treino_1)
  
  indices_teste_0 = indices_0[]
  indices_teste_1 = indices_1[]
  indices_teste = c(indices_teste_0, indices_teste_1)
  
  indices_valida_0 = indices_0[]
  indices_valida_1 = indices_1[]
  indices_valida = c(indices_valida_0, indices_valida_1)
    
  return(list(treino = indices_treino, teste = indices_teste, valida = indices_valida))
}

divide_treino_teste_valida_estratificado(y, c(60, 30, 10))


calcula_metricas_classificacao = function(vetor_real, vetor_estimado){
  if(!is.factor(vetor_real) | !is.factor(vetor_estimado)){return("Vectors must be factor class")}
  if(length(vetor_real)!=length(vetor_estimado)){
    return(paste0("Vectors must have same length/size, ", length(vetor_real), "!=", length(vetor_estimado)))}
  
  matriz_confusao = matrix(0, nrow = 2, ncol = 2)
  matriz_confusao[1,1] = length(intersect(which(vetor_real==1), which(vetor_estimado==1)))
  matriz_confusao[2,2] = length(intersect(which(vetor_real==0), which(vetor_estimado==0)))
  matriz_confusao[1,2] = length(intersect(which(vetor_real==0), which(vetor_estimado==1)))
  matriz_confusao[2,1] = length(intersect(which(vetor_real==1), which(vetor_estimado==0)))
  
  colnames(matriz_confusao) = c("Real1", "Real0")
  rownames(matriz_confusao) = c("Pred1", "Pred0")
  
  return(matriz_confusao)
}

real = as.factor(c(1, 1, 1, 1, 1, 0, 0, 0, 1, 1))
estimado = as.factor(c(0, 0, 1, 1, 1 , 1, 0, 0, 0, 1))
calcula_metricas_classificacao(real, estimado)


# 1. Regressão Logística
set.seed(123)

modelo_logit <- train(
  y ~ .,
  data = treino,
  method = "glm",
  family = "binomial",
  trControl = ctrl
)
acuracia_logit <- avaliar_modelo(modelo_logit, teste)

# 8. KNN
modelo_knn <- train(
  y ~ .,
  data = treino,
  method = "kknn",
  trControl = ctrl,
  preProcess = c("center", "scale")
)

#precisao
#recall
#sensibilidade
#especificidade
#percentual das classes
#auc
#acuracia
#kappa
#f1
#KNN com e sem variaveis
#LASSO na regressão logistica com todas
#Logistica com todas
#Logistica só com 3
#Selecao de variaveis pra verificar se aquelas 3 sao selecionadas.