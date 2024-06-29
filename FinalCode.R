if (!require("trend")) install.packages("trend") #
library(trend)
if(!require(dplyr)) install.packages("dplyr") #
library(dplyr)                                
if(!require(car)) install.packages("car") #
library(car)                                
if(!require(ggplot2)) install.packages("ggplot2") #
library(ggplot2)
if(!require(writexl)) install.packages("writexl") #
library(writexl)


# ---------------------------------------------------------------------------#
# Carregando o banco de dados

dados <- read.csv2('TodosOsDados.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
View(dados)
glimpse(dados)



# ---------------------------------------------------------------------------#

# Encontrar e mostrar os valores negativos em cada coluna (Fazendo um "check" nos dados)
for (coluna in names(dados)) {
  valores_negativos <- dados[dados[[coluna]] < 0, coluna]
  if (length(valores_negativos) > 0) {
    cat("Valores negativos na coluna", coluna, ":", toString(valores_negativos), "\n")
  }
}

# ---------------------------------------------------------------------------#

# Calcular as estatísticas descritivas
desc_stats <- data.frame(
  Mean = sapply(dados, mean, na.rm = TRUE),
  Median = sapply(dados, median, na.rm = TRUE),
  SD = sapply(dados, sd, na.rm = TRUE)
)

# Criar um data frame com as estatísticas descritivas
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

# ---------------------------------------------------------------------------#
### TODOS OS HISTOGRAMAS SAO FEITOS A SEGUIR ###
### ALTERACAO DE COLUNA MANUALMENTE ###

##############
# Ajustando o tamanho dos números nos eixos x e y
par(cex.axis = 0.7)

# Criando o histograma com limite no eixo y de 0 a 200 e ajustando o tamanho da fonte
hist_plot <- hist(dados$RE.N.19C.2208, col = "gray", border = "black", main = NULL, xlab = "COP", ylab = "Frequência", xlim = c(0, 10), ylim = c(0, 300))

# Ajustando o tamanho da fonte do eixo x e y
par(cex.axis = 1, cex.lab = 1)

# Ajustando a margem para a palavra "Frequência" no eixo y
par(mgp = c(2.5, 1, 0))

################

# ---------------------------------------------------------------------------

# Criando os gráficos de dispersão com degrade da temperatura

dados <- read.csv2('TodosOsDados.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv

# Fazendo de 10 em 10 minutos 

# Criando uma nova coluna "Grupo" para dividir os dados em grupos de 10
dados$tempo <- 10*rep(1:(nrow(dados)/10), each = 10)

# Agora, agrupando os dados por "Grupo" e calculando as médias para as colunas "COP" e "T_Med_Reserv"
dados_agrupados <- dados %>%
  group_by(tempo) %>%
  summarize(COP_Media = mean(RE.N.22C.0910), T_Med_Reserv_Media = mean(T.RE.N.22C.0910))

# Visualizando os dados reorganizados
# View(dados_agrupados)

ggplot(dados_agrupados, aes(x = tempo, y = COP_Media, color = T_Med_Reserv_Media)) +
  geom_point(size = 2) +  # Reduzindo o tamanho dos pontos para 2
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(x = "Tempo (minutos)", y = "COP") +
  ylim(3, 7) +  # Definindo o limite do eixo y para no máximo 7
  ggtitle(NULL) +  # Adicione o título aqui
  theme_minimal() + # Usando o tema minimal para evitar interferências com os eixos
  theme(
    # plot.background = element_rect(fill = "white"), # Define o fundo do painel principal como branco
    legend.text = element_text(size = 11), # Tamanho da fonte da legenda da barra de cores
    axis.text.x = element_text(size = 13), # Tamanho da fonte da legenda do eixo x
    axis.text.y = element_text(size = 13), # Tamanho da fonte da legenda do eixo y
    axis.title.x = element_text(size = 13), # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 13), # Tamanho da fonte do título do eixo y
    legend.title = element_text(size = 13) # Tamanho da fonte do título da barra de cores
  ) +
  guides(
    color = guide_colorbar(
      title = expression(bar(T) * " "~reserv.~(degree*C)), 
      title.position = "top", 
      title.theme = element_text(size = 8)
    )
  )


# ---------------------------------------------------------------------------#

# Criando gráficos para o COP convencional ao longo do tempo. 

dados <- read.csv2('TodosOsDados.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv

# Fazendo de 10 em 10 minutos 

# Criando uma nova coluna "Grupo" para dividir os dados em grupos de 10
dados$tempo <- 10*rep(1:(nrow(dados)/10), each = 10)

# Agora, agrupando os dados por "Grupo" e calculando as médias para as colunas "COP" e "T_Med_Reserv"
dados_agrupados <- dados %>%
  group_by(tempo) %>%
  summarize(COP_Media = mean(C.N.22C.0910))

# Visualizando os dados reorganizados
# View(dados_agrupados)

ggplot(dados_agrupados, aes(x = tempo, y = COP_Media)) +
  geom_point(color = "blue", size = 1.5) +  # Adicionando a cor azul aos pontos
  # geom_line(color = "blue") +   # Conectando os pontos com linhas azuis
  ylim(2.5, 4.5) +  # Definindo o limite do eixo y para no máximo 7
  theme_minimal() + # Usando o tema minimal para evitar interferências com os eixos
  theme(
    # plot.background = element_rect(fill = "white"), # Define o fundo do painel principal como branco
    
    axis.text.x = element_text(size = 13), # Tamanho da fonte da legenda do eixo x
    axis.text.y = element_text(size = 13), # Tamanho da fonte da legenda do eixo y
    axis.title.x = element_text(size = 13), # Tamanho da fonte do título do eixo x
    axis.title.y = element_text(size = 13), # Tamanho da fonte do título do eixo y
    
  ) +
  labs(title = NULL,
       x = "Tempo (minutos)",
       y = "COP")


# ---------------------------------------------------------------------------#
### Teste de Mann-Kendall ###

dados <- read.csv2('TodosOsDados.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv

# Remover colunas que não contenham números (manter apenas as colunas numéricas)
dados <- dados %>% select_if(is.numeric)
# Remover colunas que começam com a letra "M"
dados <- dados %>% select(-starts_with("M"))
# Remover colunas que começam com a letra "T"
dados <- dados %>% select(-starts_with("T"))

# Visualizar os dados após a remoção das colunas nao necessarias
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
MK_ <- tabela_resultados
View(MK_)

mk.test(dados$C.F.19C.2509)

# Salvando o banco de dados em um arquivo CSV 
### write_xlsx(MK_minuto, path = "MK_.xlsx") #####

# ---------------------------------------------------------------------------#
### Sen's Slope ###

# Função para aplicar o teste de Sen's Slope a uma coluna
apply_sens_slope <- function(column) {
  # Realize o teste de Sen's Slope
  sens_result <- sens.slope(column)
  
  # Exiba o resultado
  cat("\nColuna:", colnames(dados)[i], "\n")
  cat("Sen's Slope:", sens_result$est, "\n")
  cat("P-valor:", sens_result$p.value, "\n")
}

# Iterando sobre cada coluna do conjunto de dados
for (i in 1:ncol(dados)) {
  apply_sens_slope(dados[, i])
}


#######################################################################################
########################## ANOVA - CASO 1 ######################################################
#######################################################################################
#Fatores: Aparelho e setup 

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

#AVALIANDO COM A BOMBA LIGADA
dados <- read.csv2('Caso1-1.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
View(dados)                                    # Visualizacao dos dados em janela separada
glimpse(dados)                                 # Visualizacao de um resumo dos dados

# Verificacao dos pressupostos nos residuos

## Construcao do modelo:
modelo <- aov(COP ~ aparelho*setup, dados)
summary(modelo)

## Teste de normalidade para os residuos:
shapiro.test(modelo$residuals)
## Verificacao da homogeneidade de variancias - teste de Levene (pacote car)
leveneTest(COP ~ aparelho*setup, dados)


# Grafico de interacao (Pacote ggplot2)

ggplot(dados, aes(x = aparelho, y = COP, group = setup)) +
  geom_line(stat = "summary", fun.data = "mean_se", linewidth = 0.6, aes(linetype = setup)) +
  geom_point(stat = "summary", fun = "mean", size = 2, aes(shape = setup)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  
  # Ajuste do tamanho dos rótulos do eixo x e y
  theme(axis.text.x = element_text(size = 12),   # Ajusta o tamanho do rótulo do eixo x
        axis.text.y = element_text(size = 12)) +  # Ajusta o tamanho do rótulo do eixo y
  
  # Modificação da legenda do eixo x
  labs(x = "Tipo de aparelho")

#----------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------#

#AVALIANDO COM A BOMBA DESLIGADA
dados <- read.csv2('Caso1-2.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
View(dados)                                    # Visualizacao dos dados em janela separada
glimpse(dados)                                 # Visualizacao de um resumo dos dados


## Construcao do modelo:
modelo <- aov(COP ~ aparelho*setup, dados)
summary(modelo)

## Teste de normalidade para os residuos:
shapiro.test(modelo$residuals)
## Verificacao da homogeneidade de variancias - teste de Levene (pacote car)
leveneTest(COP ~ aparelho*setup, dados)


# Grafico de interacao (Pacote ggplot2)

ggplot(dados, aes(x = aparelho, y = COP, group = setup)) +
  geom_line(stat = "summary", fun.data = "mean_se", linewidth = 0.6, aes(linetype = setup)) +
  geom_point(stat = "summary", fun = "mean", size = 2, aes(shape = setup)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  
  # Ajuste do tamanho dos rótulos do eixo x e y
  theme(axis.text.x = element_text(size = 12),   # Ajusta o tamanho do rótulo do eixo x
        axis.text.y = element_text(size = 12)) +  # Ajusta o tamanho do rótulo do eixo y
  
  # Modificação da legenda do eixo x
  labs(x = "Tipo de aparelho")



#####################################################################################
########################## ANOVA - CASO 2 ######################################################
#######################################################################################
#Fatores: tipo de conveção e temepatura de setup

dados <- read.csv2('Caso2.csv', stringsAsFactors = T,
                   fileEncoding = "latin1")    # Carregamento do arquivo csv
View(dados)                                    # Visualizacao dos dados em janela separada
glimpse(dados)                                 # Visualizacao de um resumo dos dados

# Verificacao dos pressupostos nos residuos

## Construcao do modelo:
modelo <- aov(COP ~ conveccao*setup, dados)
summary(modelo)

# Calculando as médias dos grupos
medias <- aggregate(COP ~ conveccao, data = dados, FUN = mean)
# Mostra as médias dos grupos
print(medias)


## Teste de normalidade para os residuos:
shapiro.test(modelo$residuals)

## Verificacao da homogeneidade de variancias - teste de Levene (pacote car)
leveneTest(COP ~ conveccao*setup, dados)


# Grafico de interacao (Pacote ggplot2)

ggplot(dados, aes(x = conveccao, y = COP, group = setup)) +
  geom_line(stat = "summary", fun.data = "mean_se", linewidth = 0.6, aes(linetype = setup)) +
  geom_point(stat = "summary", fun = "mean", size = 2, aes(shape = setup)) +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2) +
  
  # Ajuste do tamanho dos rótulos do eixo x e y
  theme(axis.text.x = element_text(size = 12),   # Ajusta o tamanho do rótulo do eixo x
        axis.text.y = element_text(size = 12)) +  # Ajusta o tamanho do rótulo do eixo y
  
  # Modificação da legenda do eixo x
  labs(x = "Tipo de convecção")

####################################### THE END ###############################################
