rm(list=ls(all=TRUE))
set.seed(27112023)
library(dplyr)
library(ggplot2)
library(car)
library(dplyr)
library(tictoc)
library(cvms)
library(broom)    
library(tibble)   
library(ggimage)
library(rsvg)  

# Dados

dados <-read.table("acidentes2023_todas_causas_tipos.csv", header = T, sep=";", 
                   encoding = "latin1", dec = ".")#latin1 or UTF-8

length(dados$id)
dim(dados)
nomes_das_variaveis <- names(dados)
print(nomes_das_variaveis)

summary(dados)

dados_filtrados <- dados %>% 
  select(!c(regional, delegacia,fase_dia, latitude, longitude, 
            uop, id, pesid, causa_principal, ordem_tipo_acidente,
            uso_solo, id_veiculo, estado_fisico,ilesos,feridos_leves,
            feridos_graves,mortos, marca, classificacao_acidente)) %>%
filter(uf =="MG")

dim(dados_filtrados)

dados_filtrados <- dados_filtrados %>% 
  select(!c(uf))

dim(dados_filtrados)

summary(dados_filtrados)
colnames(dados_filtrados)

################################################# Tratamento dos dados #################################################

# Verificando valores ausentes

print(any(is.na(dados_filtrados)))

# Removendo linhas com valores ausentes

dados_filtrados <- na.omit(dados_filtrados)
print(any(is.na(dados_filtrados)))
dim(dados_filtrados)

glimpse(dados_filtrados) # tipos dos dados

# tratando a variável idade

boxplot(dados_filtrados$idade)
density(dados_filtrados$idade)
dados_filtrados$idade[dados_filtrados$idade>100] <- round(mean(dados_filtrados$idade[dados_filtrados$idade<100]))
ggplot(dados_filtrados, aes(x = idade)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#tratando a variável km

boxplot(dados_filtrados$km)
dados_filtrados$km[dados_filtrados$km==0] <- mean(dados_filtrados$km[dados_filtrados$km!=0])
boxplot(dados_filtrados$km)
hist(dados_filtrados$km)
max(dados_filtrados$km)

# tratando a variável horário

table(dados_filtrados$horario)
dados_filtrados$horario["00:00:00"<= dados_filtrados$horario &
                          dados_filtrados$horario <"06:00:00"] <- "madrugada"
dados_filtrados$horario["06:00:00"<= dados_filtrados$horario &
                          dados_filtrados$horario <"12:00:00"] <- "manhã"
dados_filtrados$horario["12:00:00"<= dados_filtrados$horario &
                          dados_filtrados$horario <"18:00:00"] <- "tarde"
dados_filtrados$horario["18:00:00"<= dados_filtrados$horario &
                          dados_filtrados$horario <"23:59:00"] <- "noite"
table(dados_filtrados$horario)
ggplot(dados_filtrados, aes(x = horario)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequência")
dados_filtrados$horario <- as.factor(dados_filtrados$horario)

# tratando a variável data_inversa

table(dados_filtrados$data_inversa)
dados_filtrados$data_inversa <- as.Date.character(dados_filtrados$data_inversa,tryFormats = "%d/%m/%Y")
table(dados_filtrados$data_inversa)
boxplot(dados_filtrados$data_inversa)

ggplot(dados_filtrados, aes(x = data_inversa)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  #geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$data_inversa["2023-01-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-01-31"] <- "1/1/1"
dados_filtrados$data_inversa["2023-02-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-02-28"] <- "2/2/2"
dados_filtrados$data_inversa["2023-03-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-03-31"] <- "3/3/3"
dados_filtrados$data_inversa["2023-04-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-04-30"] <- "4/4/4"
dados_filtrados$data_inversa["2023-05-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-05-31"] <- "5/5/5"
dados_filtrados$data_inversa["2023-06-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-06-30"] <- "6/6/6"
dados_filtrados$data_inversa["2023-07-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-07-31"] <- "7/7/7"
dados_filtrados$data_inversa["2023-08-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-08-31"] <- "8/8/8"
dados_filtrados$data_inversa["2023-09-01"<= dados_filtrados$data_inversa &
                               dados_filtrados$data_inversa <="2023-09-30"] <- "9/9/9"
table(dados_filtrados$data_inversa)
dados_filtrados$data_inversa <- as.character(dados_filtrados$data_inversa)
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0001-01-01"] <- "janeiro"
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0002-02-02"] <- "fevereiro"
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0003-03-03"] <- "março"
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0004-04-04"] <- "abril"
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0005-05-05"] <- "maio"
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0006-06-06"] <- "junho"
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0007-07-07"] <- "julho"
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0008-08-08"] <- "agosto"
dados_filtrados$data_inversa[dados_filtrados$data_inversa=="0009-09-09"] <- "setembro"
table(dados_filtrados$data_inversa)
ordem_desejada <- c("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro")

dados_filtrados$data_inversa <- as.factor(dados_filtrados$data_inversa)

ggplot(dados_filtrados, aes(x = data_inversa)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Frequência")
aux <- factor(dados_filtrados$data_inversa, levels = ordem_desejada)
plot(aux)

# tratamento da variável dia_semana

table(dados_filtrados$dia_semana)

ggplot(dados_filtrados, aes(x = dia_semana)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dados_filtrados$dia_semana <- as.factor(dados_filtrados$dia_semana)

# br

table(dados_filtrados$br)
boxplot(dados_filtrados$br)
dados_filtrados$br <- as.factor(dados_filtrados$br)

# município

table(dados_filtrados$municipio)

# causa_acidente

library(tidyverse)

### Condutor #####

causas_condutor <- c("Acessar a via sem observar a presença dos outros veículos",
                     "Ausência de reação do condutor",
                     "Condutor usando celular",
                     "Conversão proibida",
                     "Desrespeitar a preferência no cruzamento",
                     "Estacionar ou parar em local proibido",
                     "Frear bruscamente","Ingestão de álcool pelo condutor",
                     "Ingestão de substâncias psicoativas pelo condutor",
                     "Mal súbito do condutor","Manobra de mudança de faixa",
                     "Participar de racha","Reação tardia ou ineficiente do condutor",
                     "Retorno proibido", "Suicídio (presumido)",
                     "Trafegar com motocicleta (ou similar) entre as faixas",
                     "Transitar na contramão","Transitar no Acostamento",
                     "Ultrapassagem Indevida","Velocidade Incompatível",
                     "Acesso irregular", "Transitar na calçada",
                     "Condutor deixou de manter distância do veículo da frente",
                     "Condutor Dormindo", "Transtornos Mentais (exceto suicidio)",
                     "Condutor não acionou o farol baixo durante o dia em rodovias de pista simples",
                     "Deixar de acionar o farol da motocicleta (ou similar)")
dados_filtrados$causa_acidente[dados_filtrados$causa_acidente %in% causas_condutor] <- "Condutor"

### Pista #####

causas_pista <- c("Curva acentuada","Declive acentuado",
                  "Deficiência do Sistema de Iluminação/Sinalização",
                  "Demais falhas na via","Desvio temporário",
                  "Faixas de trânsito com largura insuficiente",
                  "Falta de acostamento",
                  "Falta de elemento de contenção que evite a saída do leito carroçável",
                  "Iluminação deficiente","Objeto estático sobre o leito carroçável",
                  "Obras na pista","Obstrução na via","Pista em desnível",
                  "Pista esburacada"," Pista Escorregadia",
                  "Restrição de visibilidade em curvas horizontais",
                  "Restrição de visibilidade em curvas verticais",
                  "Sinalização encoberta","Sinalização mal posicionada",
                  "Sistema de drenagem ineficiente","Acumulo de água sobre o pavimento",
                  "Acostamento em desnível","Redutor de velocidade em desacordo",
                  "Acumulo de areia ou detritos sobre o pavimento",
                  "Acumulo de óleo sobre o pavimento",
                  "Afundamento ou ondulação no pavimento",
                  "Área urbana sem a presença de local apropriado para a travessia de pedestres",
                  "Ausência de sinalização","Pista Escorregadia")
dados_filtrados$causa_acidente[dados_filtrados$causa_acidente %in% causas_pista] <- "Pista"

### Veículo #####

causas_veiculo <- c("Demais falhas mecânicas ou elétricas",
                    "Faróis desregulados", "Problema com o freio",
                    "Modificação proibida", "Problema na suspensão",
                    "Avarias e/ou desgaste excessivo no pneu",
                    "Carga excessiva e/ou mal acondicionada","Problema na suspensão")
dados_filtrados$causa_acidente[dados_filtrados$causa_acidente %in% causas_veiculo] <- "Veículo"

### Outros #####

causas_Outros <- c("Demais Fenômenos da natureza",
                   "Entrada inopinada do pedestre",
                   "Fumaça", "Chuva",
                   "Ingestão de álcool e/ou substâncias psicoativas pelo pedestre",
                   "Ingestão de álcool ou de substâncias psicoativas pelo pedestre",
                   "Neblina","Pedestre andava na pista",
                   "Pedestre cruzava a pista fora da faixa","Animais na Pista")
dados_filtrados$causa_acidente[dados_filtrados$causa_acidente %in% causas_Outros] <- "Outros"

causas_ac <- table(dados_filtrados$causa_acidente)

dados_filtrados$causa_acidente <- as.factor(dados_filtrados$causa_acidente)

ggplot(dados_filtrados, aes(x = causa_acidente)) + 
  geom_bar(stat = "count", fill = "lightblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Frequência")

# tipo_acidente

table(dados_filtrados$tipo_acidente)
ggplot(dados_filtrados, aes(x = tipo_acidente)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$tipo_acidente <- as.factor(dados_filtrados$tipo_acidente)

# sentido_via

class(dados_filtrados$sentido_via)
table(dados_filtrados$sentido_via)
ggplot(dados_filtrados, aes(x = sentido_via)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$sentido_via <- as.factor(dados_filtrados$sentido_via)

# condicao_metereologica

class(dados_filtrados$condicao_metereologica)
table(dados_filtrados$condicao_metereologica)
ggplot(dados_filtrados, aes(x = condicao_metereologica)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$condicao_metereologica <- as.factor(dados_filtrados$condicao_metereologica)

# tipo_pista

class(dados_filtrados$tipo_pista)
table(dados_filtrados$tipo_pista)
ggplot(dados_filtrados, aes(x = tipo_pista)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$tipo_pista <- as.factor(dados_filtrados$tipo_pista)

# tracado_via

class(dados_filtrados$tracado_via)
table(dados_filtrados$tracado_via)
ggplot(dados_filtrados, aes(x = tracado_via)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$tracado_via <- as.factor(dados_filtrados$tracado_via)

# tipo_veiculo

class(dados_filtrados$tipo_veiculo)
table(dados_filtrados$tipo_veiculo)
ggplot(dados_filtrados, aes(x = tipo_veiculo)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$tipo_veiculo <- as.factor(dados_filtrados$tipo_veiculo)

# ano_fabricacao_veiculo

class(dados_filtrados$ano_fabricacao_veiculo)
table(dados_filtrados$ano_fabricacao_veiculo)
ggplot(dados_filtrados, aes(x = ano_fabricacao_veiculo)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# tipo_envolvido

class(dados_filtrados$tipo_envolvido)
table(dados_filtrados$tipo_envolvido)
ggplot(dados_filtrados, aes(x = tipo_envolvido)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$tipo_envolvido <- as.factor(dados_filtrados$tipo_envolvido)

# sexo

class(dados_filtrados$sexo)
table(dados_filtrados$sexo)
ggplot(dados_filtrados, aes(x = sexo)) + 
  geom_bar(stat = "count", fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dados_filtrados$sexo <- as.factor(dados_filtrados$sexo)

############################# Preparação dos dados para teste e treino #################################################

library(caret)

train_index <- createDataPartition(y=dados_filtrados$causa_acidente, 
                                   p = 0.75, list=FALSE)

# Conjunto de treino e teste

treino <- dados_filtrados[train_index, ]
teste <- dados_filtrados[-train_index, ]

########################################## cross validation ############################################################

fitControl <- trainControl(method = "cv",
                           number = 10)

########################################### Random Forest ##############################################################

set.seed(1)

library(randomForest)
library(openxlsx)

{tic()
  fit1 <- randomForest(causa_acidente ~., 
                       data = treino, importance = T, trControl = fitControl)
  toc()}

fit1

plot(fit1)

# importância de variáveis

glimpse(dados_filtrados)
importance(fit1)
varImpPlot(fit1)

# para o teste

Test1 <- teste[,-7]
pred <- predict(fit1, Test1)

conf_matrix <- confusionMatrix(pred, teste$causa_acidente, mode="everything")
conf_matrix

# Calcular a matriz de confusão

conf_matrix <- confusionMatrix(data = factor(pred, levels = unique(pred)), 
                               reference = factor(teste$causa_acidente, levels = unique(teste$causa_acidente)))
conf_matrix

# Acurácia balanceada média

acuracia_balanceada <- mean(conf_matrix$byClass[,"Balanced Accuracy"])
print(paste("Acurácia Balanceada Total:", round(acuracia_balanceada, 4)))

# Especificidade média ponderada

if ("byClass" %in% names(conf_matrix)) {
  especificidade_media_ponderada <- sum(conf_matrix$byClass[,"Specificity"] * conf_matrix$byClass[,"Prevalence"])
  print(paste("Especificidade Média Ponderada:", round(especificidade_media_ponderada, 4)))
} else {
  print("A matriz de confusão não contém a coluna 'byClass'.")
}

conf_matrix <- confusion_matrix(targets = teste$causa_acidente,
                             predictions = pred)

plot_confusion_matrix(conf_matrix$`Confusion Matrix`[[1]])

plot_confusion_matrix(
  conf_matrix$`Confusion Matrix`[[1]],
  add_sums = TRUE,
  sums_settings = sum_tile_settings(
    palette = "Oranges",
    label = "Total",
    tc_tile_border_color = "black"
  )
)

# para o treino

pred <- predict(fit1, treino[-7])

conf_matrix <- confusionMatrix(pred,treino$causa_acidente, mode="everything")
conf_matrix

###################################################### KNN #############################################################

set.seed(2)

{tic()
  knnfit <- train(causa_acidente ~ . ,
                  method     = "knn",
                  tuneLength = 10,
                  trControl  = fitControl,
                  metric     = "Accuracy", 
                  data       = treino,
  )
  toc()}

knnfit

plot(knnfit) # gráfico do desempenho segundo os valores de k
knnfit$finalModel # melhor modelo

### Predições ###

predknn <- predict(knnfit, teste[-7])

conf_matrix_KNN <- confusionMatrix(predknn, teste$causa_acidente, mode="everything")
conf_matrix_KNN

+# Acurácia balanceada média

str(conf_matrix_KNN)

acuracia_balanceada <- mean(conf_matrix_KNN$byClass[, "Sensitivity"] + conf_matrix_KNN$byClass[, "Specificity"]) / 2
print(paste("Acurácia Balanceada Total:", round(acuracia_balanceada, 4)))

# Especificidade média ponderada

if ("byClass" %in% names(conf_matrix_KNN)) {
  especificidade_media_ponderada <- sum(conf_matrix_KNN$byClass[,"Specificity"] * conf_matrix_KNN$byClass[,"Prevalence"])
  print(paste("Especificidade Média Ponderada:", round(especificidade_media_ponderada, 4)))
} else {
  print("A matriz de confusão não contém a coluna 'byClass'.")
}

conf_matrix_KNN <- confusion_matrix(targets = teste$causa_acidente,
                                    predictions = predknn)

plot_confusion_matrix(conf_matrix_KNN$`Confusion Matrix`[[1]])

plot_confusion_matrix(
  conf_matrix_KNN$`Confusion Matrix`[[1]],
  add_sums = TRUE,
  sums_settings = sum_tile_settings(
    palette = "Oranges",
    label = "Total",
    tc_tile_border_color = "black"
  )
)

# para o treino

pred <- predict(knnfit, treino[-7])

conf_matrix_knn <- confusionMatrix(pred,treino$causa_acidente, mode="everything")
conf_matrix_knn

###################################################### SVM #############################################################

glimpse(dados_filtrados)
dados_filtrados$km <- as.factor(dados_filtrados$km)
dados_filtrados$municipio <- as.factor(dados_filtrados$municipio)
dados_filtrados$ano_fabricacao_veiculo <- as.factor(dados_filtrados$ano_fabricacao_veiculo)
dados_filtrados$tipo_envolvido <- as.factor(dados_filtrados$tipo_envolvido)
dados_filtrados$idade <- as.factor(dados_filtrados$idade)
glimpse(dados_filtrados)

train_index <- createDataPartition(y=dados_filtrados$causa_acidente, 
                                   p = 0.75, list=FALSE)
# Conjunto de treino e teste

treino <- dados_filtrados[train_index, ]
teste <- dados_filtrados[-train_index, ]

# igualando os níveis dos fatores: problemas em alguns modelos # teste #
levels(teste$data_inversa) <- levels(treino$data_inversa)
levels(teste$dia_semana) <- levels(treino$dia_semana)
levels(teste$horario) <- levels(treino$horario)
levels(teste$br) <- levels(treino$br)
levels(teste$km) <- levels(treino$km)
levels(teste$municipio) <- levels(treino$municipio)
levels(teste$causa_acidente) <- levels(treino$causa_acidente)
levels(teste$tipo_acidente) <- levels(treino$tipo_acidente)
levels(teste$sentido_via) <- levels(treino$sentido_via)
levels(teste$condicao_metereologica) <- levels(treino$condicao_metereologica)
levels(teste$tipo_pista) <- levels(treino$tipo_pista)
levels(teste$tracado_via) <- levels(treino$tracado_via)
levels(teste$tipo_veiculo) <- levels(treino$tipo_veiculo)
levels(teste$ano_fabricacao_veiculo) <- levels(treino$ano_fabricacao_veiculo)
levels(teste$tipo_envolvido) <- levels(treino$tipo_envolvido)
levels(teste$idade) <- levels(treino$idade)
levels(teste$sexo) <- levels(treino$sexo)

# igualando os níveis dos fatores: problemas em alguns modelos # treino #

levels(treino$data_inversa) <- levels(teste$data_inversa)
levels(treino$dia_semana) <- levels(teste$dia_semana)
levels(treino$horario) <- levels(teste$horario)
levels(treino$br) <- levels(teste$br)
levels(treino$km) <- levels(teste$km)
levels(treino$municipio) <- levels(teste$municipio)
levels(treino$causa_acidente) <- levels(teste$causa_acidente)
levels(treino$tipo_acidente) <- levels(teste$tipo_acidente)
levels(treino$sentido_via) <- levels(teste$sentido_via)
levels(treino$condicao_metereologica) <- levels(teste$condicao_metereologica)
levels(treino$tipo_pista) <- levels(teste$tipo_pista)
levels(treino$tracado_via) <- levels(teste$tracado_via)
levels(treino$tipo_veiculo) <- levels(teste$tipo_veiculo)
levels(treino$ano_fabricacao_veiculo) <- levels(teste$ano_fabricacao_veiculo)
levels(treino$tipo_envolvido) <- levels(teste$tipo_envolvido)
levels(treino$idade) <- levels(teste$idade)
levels(treino$sexo) <- levels(teste$sexo)

set.seed(3)

library(e1071)

{tic()
  svm_linear <- svm(causa_acidente ~ ., data = treino, importance = T, kernel = 'linear',  trControl = fitControl)
  toc()}

svm_linear

# para o teste

pred2 <- predict(svm_linear, teste[-7])

conf_matrix2 <- confusionMatrix(pred2,teste$causa_acidente, mode="everything")
conf_matrix2

# Acurácia balanceada média

acuracia_balanceada <- mean(conf_matrix2$byClass[,"Balanced Accuracy"])
print(paste("Acurácia Balanceada Total:", round(acuracia_balanceada, 4)))

# Especificidade média ponderada

if ("byClass" %in% names(conf_matrix2)) {
  especificidade_media_ponderada <- sum(conf_matrix2$byClass[,"Specificity"] * conf_matrix2$byClass[,"Prevalence"])
  print(paste("Especificidade Média Ponderada:", round(especificidade_media_ponderada, 4)))
} else {
  print("A matriz de confusão não contém a coluna 'byClass'.")
}

conf_mat <- confusion_matrix(targets = teste$causa_acidente,
                             predictions = pred2)

plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]])

plot_confusion_matrix(
  conf_mat$`Confusion Matrix`[[1]],
  add_sums = TRUE,
  sums_settings = sum_tile_settings(
    palette = "Oranges",
    label = "Total",
    tc_tile_border_color = "black"
  )
)

pred2 <- predict(svm_linear, treino[-7])

conf_matrix2 <- confusionMatrix(pred2,treino$causa_acidente, mode="everything")
conf_matrix2