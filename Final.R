# Instalar e carregar a biblioteca "trend" para realizar o teste de Mann-Kendall
if (!require("trend")) install.packages("trend")
library(trend)
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(car)) install.packages("car")   
library(car)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)                                
if(!require(DescTools)) install.packages("DescTools") 
library(DescTools)
if(!require(emmeans)) install.packages("emmeans")
library(emmeans)
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)
if(!require(easyanova)) install.packages("easyanova") 
library(easyanova)
if(!require(ExpDes.pt)) install.packages("ExpDes.pt")
library(ExpDes.pt)
if(!require(writexl)) install.packages("writexl")
library(writexl)
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
if (!require(zyp)) {
  install.packages("zyp")
  library(zyp)
}

# ---------------------------------------------------------------------------#

dados <- read.csv2('TodosOsDados_copiaseguranca.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
View(dados)
glimpse(dados)

# Remover colunas que começam com a letra "M"
dados <- dados %>% select(-starts_with("M"))

# Remover colunas que não contenham números (manter apenas as colunas numéricas)
dados <- dados %>% select_if(is.numeric)
View(dados)
glimpse(dados)


# ---------------------------------------------------------------------------#

# Encontrar e mostrar os valores negativos em cada coluna
for (coluna in names(dados)) {
  valores_negativos <- dados[dados[[coluna]] < 0, coluna]
  if (length(valores_negativos) > 0) {
    cat("Valores negativos na coluna", coluna, ":", toString(valores_negativos), "\n")
  }
}

# Verificar se há valores negativos na coluna "RE.N.19C.2208"
valores_negativos <- dados$`RE.N.19C.2208`[dados$`RE.N.19C.2208` < 0]
if (length(valores_negativos) > 0) {
  cat("Valores negativos na coluna RE.N.19C.2208:", toString(valores_negativos), "\n")
  
  # Removerndo valor negativo da coluna "RE.N.19C.2208"
  dados$`RE.N.19C.2208`[dados$`RE.N.19C.2208` < 0] <- NA
  cat("Valor negativo removido da coluna RE.N.19C.2208\n")
}

# Calcular média, mediana e desvio-padrão para a coluna "RE.N.19C.2208"
media_coluna <- mean(dados$`RE.N.19C.2208`, na.rm = TRUE)
mediana_coluna <- median(dados$`RE.N.19C.2208`, na.rm = TRUE)
desvio_padrao_coluna <- sd(dados$`RE.N.19C.2208`, na.rm = TRUE)

cat("Média da coluna RE.N.19C.2208:", media_coluna, "\n")
cat("Mediana da coluna RE.N.19C.2208:", mediana_coluna, "\n")
cat("Desvio-padrão da coluna RE.N.19C.2208:", desvio_padrao_coluna, "\n")


# Encontrar os menores valores em cada coluna
for (coluna in names(dados)) {
  menores <- head(sort(dados[[coluna]]), 1)
  cat("Dois menores valores na coluna", coluna, ":", toString(menores), "\n")
}

# ---------------------------------------------------------------------------#

# Calcular as estatísticas descritivas
desc_stats <- data.frame(
  Mean = sapply(dados, mean, na.rm = TRUE),
  Median = sapply(dados, median, na.rm = TRUE),
  SD = sapply(dados, sd, na.rm = TRUE)
)

# Criar um data frame com as estatísticas descritivas (sem a coluna "Range")
desc_stats_df <- data.frame(
  Variável = names(dados),
  Mean = desc_stats$Mean,
  Median = desc_stats$Median,
  SD = desc_stats$SD
)

# Exibir o data frame resultante
View(desc_stats_df)

# Salvando o banco de dados em um arquivo CSV 
### write_xlsx(desc_stats_df, path = "desc_statss.xlsx") #####

# ---------------------------------------------------------------------------#

# Remover colunas que começam com a letra "T"
dados <- dados %>% select(-starts_with("T"))

# Visualizar os dados após a remoção das colunas
View(dados)
glimpse(dados)

### TODOS OS HISTOGRAMAS A SEGUIR ##

##############
# Criando o histograma com limite no eixo y de 0 a 300 e ajustando o tamanho da fonte
hist_plot <- hist(dados$RE.N.22C.0910, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência", ylim = c(0, 200))

# Ajustando o tamanho da fonte do eixo x e y
par(cex.axis = 1.5, cex.lab = 1.5)

# Ajustando a margem para a palavra "Frequência" no eixo y
par(mgp = c(2.5, 1, 0))

# Criando o histograma com limite no eixo y de 0 a 300 e ajustando o tamanho da fonte
hist_plot <- hist(dados$RE.F.22C.1609, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência", ylim = c(0, 200))
###############

# Criando o histograma
hist_plot <- hist(dados$C.F.19C.2509, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
# hist_plot <- hist(dados$C.F.19C.2609, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.F.19C.2709, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.F.19C.0410, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")

hist_plot <- hist(dados$C.F.22C.1609, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.F.22C.1909, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.F.22C.3009, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.F.22C.0110, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")

hist_plot <- hist(dados$C.N.19C.2208, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.N.19C.2708, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.N.19C.1010, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")

hist_plot <- hist(dados$C.N.22C.1408, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.N.22C.0810, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$C.N.22C.0910, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")

hist_plot <- hist(dados$RE.F.19C.2509, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.F.19C.2609, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.F.19C.2709, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.F.19C.0410, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")

hist_plot <- hist(dados$RE.F.22C.1609, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.F.22C.1909, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.F.22C.3009, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.F.22C.0110, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")

hist_plot <- hist(dados$RE.N.19C.2208, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.N.19C.2708, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.N.19C.1010, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")

hist_plot <- hist(dados$RE.N.22C.1408, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.N.22C.0810, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")
hist_plot <- hist(dados$RE.N.22C.0910, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência")













# # Criando um histograma da primeira coluna
# hist_plot <- ggplot(dados, aes(x = C.N.22C.0910)) +
#   geom_histogram(fill = "gray", color = "black") +
#   labs(title = "09/10/2019 - convencional à 22°C", x = "COP", y = "Frequência") +  # Defina o título como NULL
#   theme_minimal() +  # Define um tema minimalista para remover o fundo cinza
#   theme(
#     panel.grid.major = element_blank(),  # Remove as linhas de grade do fundo
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white"),  # Define o fundo do gráfico como branco
#     axis.title.x = element_text(size = 12),  # Tamanho da fonte do título do eixo x
#     axis.title.y = element_text(size = 12),  # Tamanho da fonte do título do eixo y
#     axis.text.x = element_text(size = 12),   # Tamanho da fonte dos valores do eixo x
#     axis.text.y = element_text(size = 12)    # Tamanho da fonte dos valores do eixo y
#   )
# 
# # Exibindo o gráfico de histograma
# print(hist_plot)




























# # Criando um histograma da primeira coluna
# hist_plot <- ggplot(dados, aes(x = RE.N.22C.0910)) +
#   geom_histogram(binwidth = 1, fill = "red", color = "black") +
#   labs(title = "09/10/2019 - modificado com\n convecção natural à 22°C", x = "COP", y = "Frequência") +  # Defina o título como NULL
#   theme_minimal() +  # Define um tema minimalista para remover o fundo cinza
#   theme(
#     panel.grid.major = element_blank(),  # Remove as linhas de grade do fundo
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "white"),  # Define o fundo do gráfico como branco
#     axis.title.x = element_text(size = 12),  # Tamanho da fonte do título do eixo x
#     axis.title.y = element_text(size = 12),  # Tamanho da fonte do título do eixo y
#     axis.text.x = element_text(size = 12),   # Tamanho da fonte dos valores do eixo x
#     axis.text.y = element_text(size = 12)    # Tamanho da fonte dos valores do eixo y
#   )
# 
# # Exibindo o gráfico de histograma
# print(hist_plot)
# ---------------------------------------------------------------------------

# Criando os gráficos de dispersão com degrade da temperatura

dados <- read.csv2('TodosOsDados_copiaseguranca.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
# Remover colunas que não contenham números (manter apenas as colunas numéricas)
dados <- dados %>% select_if(is.numeric)
View(dados)

# Fazendo de 10 em 10 minutos 

# Criando uma nova coluna "Grupo" para dividir os dados em grupos de 10
dados$tempo <- 10*rep(1:(nrow(dados)/10), each = 10)

# Agora, agrupando os dados por "Grupo" e calculando as médias para as colunas "COP" e "T_Med_Reserv"
dados_agrupados <- dados %>%
  group_by(tempo) %>%
  summarize(COP_Media = mean(RE.F.19C.2609), T_Med_Reserv_Media = mean(T.RE.N.22C.0910))

# Visualizando os dados reorganizados
View(dados_agrupados)

ggplot(dados_agrupados, aes(x = tempo, y = COP_Media, color = T_Med_Reserv_Media)) +
  geom_point(size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Tempo", y = "COP") +
  ylim(3, 7) +  # Definindo o limite do eixo y para no máximo 7
  ggtitle(NULL) +  # Adicione o título aqui
  theme_minimal() + # Usando o tema minimal para evitar interferências com os eixos
  theme(
    # plot.background = element_rect(fill = "white"), # Define o fundo do painel principal como branco
    legend.text = element_text(size = 12), # Tamanho da fonte da legenda da barra de cores
    axis.text.x = element_text(size = 16), # Tamanho da fonte da legenda do eixo x
    axis.text.y = element_text(size = 16), # Tamanho da fonte da legenda do eixo y
    axis.title.x = element_text(size = 14), # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 14), # Tamanho da fonte do título do eixo y
    legend.title = element_text(size = 10) # Tamanho da fonte do título da barra de cores
  ) +
  guides(
    color = guide_colorbar(title = "Temperatura\nmédia do\nreservatório")
  )
# ---------------------------------------------------------------------------#

# Criando gráficos para o COP convencional ao longo do tempo. 

dados <- read.csv2('TodosOsDados_copiaseguranca.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
# Remover colunas que não contenham números (manter apenas as colunas numéricas)
dados <- dados %>% select_if(is.numeric)
View(dados)

# Fazendo de 10 em 10 minutos 

# Criando uma nova coluna "Grupo" para dividir os dados em grupos de 10
dados$tempo <- 10*rep(1:(nrow(dados)/10), each = 10)

# Agora, agrupando os dados por "Grupo" e calculando as médias para as colunas "COP" e "T_Med_Reserv"
dados_agrupados <- dados %>%
  group_by(tempo) %>%
  summarize(COP_Media = mean(C.N.22C.0910))

# Visualizando os dados reorganizados
View(dados_agrupados)

ggplot(dados_agrupados, aes(x = tempo, y = COP_Media)) +
  geom_point(color = "blue", size = 3) +  # Adicionando a cor azul aos pontos
  # geom_line(color = "blue") +   # Conectando os pontos com linhas azuis
  ylim(2.5, 4.5) +  # Definindo o limite do eixo y para no máximo 7
  theme_minimal() + # Usando o tema minimal para evitar interferências com os eixos
  theme(
    # plot.background = element_rect(fill = "white"), # Define o fundo do painel principal como branco
  
    axis.text.x = element_text(size = 16), # Tamanho da fonte da legenda do eixo x
    axis.text.y = element_text(size = 16), # Tamanho da fonte da legenda do eixo y
    axis.title.x = element_text(size = 14), # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 14), # Tamanho da fonte do título do eixo y

  ) +
  labs(title = NULL,
       x = "Tempo",
       y = "COP")


# ---------------------------------------------------------------------------#
dados <- read.csv2('TodosOsDados_copiaseguranca.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
# Remover colunas que não contenham números (manter apenas as colunas numéricas)
dados <- dados %>% select_if(is.numeric)

# Remover colunas que começam com a letra "M"
dados <- dados %>% select(-starts_with("M"))
# Remover colunas que começam com a letra "T"
dados <- dados %>% select(-starts_with("T"))

# Visualizar os dados após a remoção das colunas
View(dados)
glimpse(dados)

# Loop para aplicar o teste de Mann-Kendall a cada coluna de "dados"
for (coluna in names(dados)[-1]) {  # Começar da segunda coluna (ignorando a primeira com o nome das observações)
  result <- mk.test(dados[[coluna]])
  cat("Teste de Mann-Kendall para a coluna:", coluna, "\n")
  cat("Valor de Z:", (result$estimates[1] - 1) / sqrt(result$estimates[2]), "\n")
  cat("Valor p:", result$p.value, "\n")
  cat("\n")
}

# Criando uma lista para armazenar os resultados
resultados <- list()

# Loop para aplicar o teste de Mann-Kendall a cada coluna
for (coluna in names(dados)) {
  result <- mk.test(dados[[coluna]])
  resultado <- data.frame(
    Grupo = coluna,
    Valor_de_Z = (result$estimates[1] - 1) / sqrt(result$estimates[2]),
    P_valor = result$p.value
  )
  resultados[[coluna]] <- resultado
}

# Criar um quadro de dados a partir da lista de resultados
tabela_resultados <- do.call(rbind, resultados)

# Criar um novo quadro de dados
MK_minuto <- tabela_resultados
View(MK_minuto)


# Salvando o banco de dados em um arquivo CSV 
### write_xlsx(MK_minuto, path = "MK_minuto.xlsx") #####



# Função para aplicar o teste de Sen's Slope a uma coluna
apply_sens_slope <- function(column) {
  # Realize o teste de Sen's Slope
  sens_result <- sens.slope(column)
  
  # Exiba o resultado
  cat("\nColuna:", colnames(dados)[i], "\n")
  cat("Sen's Slope:", sens_result$est, "\n")
  cat("P-valor:", sens_result$p.value, "\n")
}

# Itere sobre cada coluna do seu conjunto de dados
for (i in 1:ncol(dados)) {
  apply_sens_slope(dados[, i])
}

# sens.slope(dados$C.F.19C.2509)
# View(dados)
# sens.slope(dados$RE.F.19C.2709)

#######################################################################################
########################## ANOVA - CASO 2 ######################################################
#######################################################################################

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('19natural.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
View(dados)                                    # Visualizacao dos dados em janela separada
glimpse(dados)                                 # Visualizacao de um resumo dos dados

# Passo 3: Verificacao dos pressupostos nos dados brutos

# dados$COP<-log10(dados$COP)
# View(dados) 


## Verificacao da presenca de outliers por grupo:
boxplot(dados$COP ~ dados$conveccao:dados$setup)


## Verificacao da homogeneidade de variancias - teste de Levene (pacote car)
leveneTest(COP ~ conveccao*setup, dados, center = mean)
leveneTest(sqrt(COP) ~ conveccao*setup, dados, center = mean)
leveneTest(log(COP) ~ conveccao*setup, dados, center = mean)
leveneTest(log10(COP) ~ conveccao*setup, dados, center = mean)
leveneTest((1/COP) ~ conveccao*setup, dados, center = mean)


# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana eh mais robusto
# Mudado para ser baseado na media 

# Passo 4: Verificacao dos pressupostos nos residuos

## Construcao do modelo:
modelo <- aov((1/COP) ~ conveccao*setup, dados)
summary(modelo)
plot(modelo, 1)

## Verificacao da normalidade - Shapiro por grupo:
dados %>% group_by(conveccao, setup) %>% 
  shapiro_test(COP)

## Teste de normalidade para os residuos:
shapiro.test(modelo$residuals)


## Verificacao da presenca de outliers entre os residuos:
boxplot(modelo$residuals)

dados$Residuos <- modelo$residuals


## Verificacao da homogeneidade de variancias - teste de Levene (pacote car)
leveneTest(Residuos ~ conveccao*setup, dados, center = mean)


# Passo 8: Grafico de interacao (Pacote ggplot2)

ggplot(dados, aes(x = conveccao, y = COP, group = setup)) +
  geom_line(stat = "summary", fun.data = "mean_se", linewidth = 0.6, aes(linetype = setup)) +
  geom_point(stat = "summary", fun = "mean", size = 2, aes(shape = setup)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  
  # Ajuste do tamanho dos rótulos do eixo x e y
  theme(axis.text.x = element_text(size = 12),   # Ajusta o tamanho do rótulo do eixo x
        axis.text.y = element_text(size = 12)) +  # Ajusta o tamanho do rótulo do eixo y
  
  # Modificação da legenda do eixo x
  labs(x = "Tipo de convecção")

# Carregue o pacote MASS
if(!require(MASS)) install.packages("MASS")
library(MASS)   


# Construa o modelo de Box-Cox
boxcox_model <- boxcox(COP ~ conveccao * setup, data = dados, lam=seq(-3,3, 2/10))

# Encontre o lambda ótimo
lambda_optimal <- boxcox_model$x[which.max(boxcox_model$y)]

# Imprima o lambda ótimo
cat("Lambda ótimo:", lambda_optimal, "\n")

# Transforme os dados com o lambda ótimo
dados$COP_transformed <- (dados$COP^lambda_optimal - 1) / lambda_optimal
View(dados)

# Visualize os dados transformados
hist(dados$COP_transformed, main = "Histograma dos dados transformados")

# Ajuste o modelo ANOVA com os dados transformados
modelo_transformado <- aov(COP_transformed ~ conveccao*setup, data = dados)
summary(modelo_transformado)

# Verificação da normalidade dos resíduos após a transformação
shapiro.test(modelo_transformado$residuals)

# Verificação da homogeneidade de variâncias dos resíduos após a transformação
leveneTest(modelo_transformado$residuals ~ conveccao*setup, data = dados, center = mean)



# Ajuste o modelo ANOVA
modelo <- aov(COP ~ conveccao*setup, data = dados)
summary(modelo)
# Extraia os resíduos do modelo
residuos <- residuals(modelo)

# Crie um gráfico de dispersão dos resíduos
plot(predict(modelo), residuos, 
     main = "Gráfico de Dispersão dos Resíduos",
     xlab = "Valores Previstos", 
     ylab = "Resíduos Padronizados")

# Adicione uma linha horizontal na média dos resíduos
abline(h = mean(residuos), col = "red", lty = 2)
mean(dados$Residuos)


#######################################################################################
########################## ANOVA - CASO 1 ######################################################
#######################################################################################
  
# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('CASO_2.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
View(dados)                                    # Visualizacao dos dados em janela separada
glimpse(dados)                                 # Visualizacao de um resumo dos dados



# Passo 3: Verificacao dos pressupostos nos dados brutos

## Verificacao da normalidade - Shapiro por grupo:
dados %>% group_by(aparelho, setup) %>% 
  shapiro_test(COP)


## Verificacao da presenca de outliers por grupo:
boxplot(dados$COP ~ dados$aparelho:dados$setup)


## Verificacao da homogeneidade de variancias - teste de Levene (pacote car)
leveneTest(COP ~ aparelho*setup, dados, center = mean)



# Por default, o teste realizado pelo pacote car tem como base a mediana (median)
# O teste baseado na mediana eh mais robusto
# Mudado para ser baseado na media 

# Passo 4: Verificacao dos pressupostos nos residuos

## Construcao do modelo:
modelo <- aov(COP ~ aparelho*setup, dados)
plot(modelo, 1)
summary(modelo)
## Teste de normalidade para os residuos:
shapiro.test(modelo$residuals)

## Verificacao da presenca de outliers entre os residuos:
boxplot(modelo$residuals)

dados$Residuos <- modelo$residuals


## Verificacao da homogeneidade de variancias - teste de Levene (pacote car)
leveneTest(Residuos ~ aparelho*setup, dados, center = mean)


# Passo 8: Grafico de interacao (Pacote ggplot2)

ggplot(dados, aes(x = aparelho, y = COP, group = setup)) +
  geom_line(stat = "summary", fun.data = "mean_se", linewidth = 0.6, aes(linetype = setup)) +
  geom_point(stat = "summary", fun = "mean", size = 2, aes(shape = setup)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  
  # Ajuste do tamanho dos rótulos do eixo x e y
  theme(axis.text.x = element_text(size = 12),   # Ajusta o tamanho do rótulo do eixo x
        axis.text.y = element_text(size = 12)) +  # Ajusta o tamanho do rótulo do eixo y
  
  # Modificação da legenda do eixo x
  labs(x = "Tipo de aparelho")

if(!require(easyanova)) install.packages("easyanova") 
library(easyanova)


fat2.dic(dados$aparelho, dados$setup, dados$COP, quali = c(TRUE, TRUE),
         fac.names = c("aparelho", "setup"), sigT = 0.05,
         sigF = 0.05, unfold = NULL)


