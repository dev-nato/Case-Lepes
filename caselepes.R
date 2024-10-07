install.packages("data.table")
install.packages("readxl")
install.packages("ggplot2")
install.packages("googlesheets4")
install.packages("googledrive")
library(googlesheets4)
library(googledrive)
library(data.table)
library(readxl)
library(ggplot2)

gs4_auth()
drive_auth()
url <- "https://docs.google.com/spreadsheets/d/1uX4jipdyuYQSOWQkAPIFf177uL6XPRqt/edit?gid=778707309#gid=778707309"

#Criando um novo diretório para baixar a tabela de dados
novo_diretorio <- "C:/caseLepesR"
# Verificar se a pasta já existe e, se não, criá-la
if (!dir.exists(novo_diretorio)) {
  dir.create(novo_diretorio)
  print("Pasta criada com sucesso!")
} else {
  print("A pasta já existe.")
}

#Baixando a planilha no diretorio que acabou de ser criado
drive_download(as_id(url), path = "C:/caseLepesR/saeb.xlsx", type = "xlsx", overwrite = TRUE)

#Importando dado Local
dados <- read_excel("C:/caseLepesR/saeb.xlsx")

#Transformando dados em data table
dt_saeb <- as.data.table(dados)

#Realizando a limpeza de dados ausentes
for (col in names(dt_saeb)) {
  if (is.numeric(dt_saeb[[col]])) {
    if(any(is.na(dt_saeb[[col]]))){
      dt_saeb[[col]][is.na(dt_saeb[[col]])] <- mean(dt_saeb[[col]], na.rm = TRUE)
    }
  }
}

#Função que calcula as médias das notas(Português, Matemática e Padronizadas)
notas_media <- function(dt, materia){
  i <- 1
  while(i <= length(dt)){
    if(i == 1){
      if(materia == 'matematica'){
        nome_intervalo <- strsplit(colnames(dt)[i], split="_")
        nome_intervalo <- unlist(nome_intervalo)
        dt_aux <- data.table(ano = nome_intervalo[3], valores = format(round(mean(dt[[i]]), 2), nsmall = 2))
      }else{
        nome_intervalo <- strsplit(colnames(dt)[i], split="_")
        nome_intervalo <- unlist(nome_intervalo)
        dt_aux <- data.table(ano = nome_intervalo[4], valores = format(round(mean(dt[[i]]), 2), nsmall = 2))
      }
    }else{
      if(materia == 'matematica'){
        nome_intervalo <- strsplit(colnames(dt)[i], split="_")
        nome_intervalo <- unlist(nome_intervalo)
        novo_valor <- data.table(ano = nome_intervalo[3], valores = format(round(mean(dt[[i]]), 2), nsmall = 2))
        dt_aux <- rbind(dt_aux, novo_valor)
      }else{
        nome_intervalo <- strsplit(colnames(dt)[i], split="_")
        nome_intervalo <- unlist(nome_intervalo)
        novo_valor <- data.table(ano = nome_intervalo[4], valores = format(round(mean(dt[[i]]), 2), nsmall = 2))
        dt_aux <- rbind(dt_aux, novo_valor)
      }
    }
    i <- i + 1
  }
  return(dt_aux)
}

#Função que calcula a taxa de crescimento médio das notas(matemática, português ou médias padronizadas)
#((nota_autal - nota_anterior)/nota_anterior) * 100
taxa_crescimento <- function(dt, materia){
  i <- 1
  j <- 2
  while(j <= length(dt)){
    nome_intervalo = paste(colnames(dt)[j], colnames(dt)[i], sep = "_")
    
    nome_intervalo <- strsplit(nome_intervalo, split="_")
    
    nome_intervalo <- unlist(nome_intervalo)
    
    if(materia == 'matematica'){
      
      nome_intervalo = paste(nome_intervalo[6], nome_intervalo[3], sep = "-")
      
    }else if(materia == 'lingua'){
      
      nome_intervalo = paste(nome_intervalo[8], nome_intervalo[4], sep = "-")
      
    }else{
      nome_intervalo = paste(nome_intervalo[8], nome_intervalo[4], sep = "-")
      
    }
    if(i == 1){
      crescimento <- data.table(intervalos = nome_intervalo, valores = format(
        round(mean(((dt[[j]] - dt[[i]])/dt[[i]])*100), 2), nsmall = 2))
      
    }else{
      novos_valores <- data.table(intervalos = nome_intervalo, valores = format(
        round(mean(((dt[[j]] - dt[[i]])/dt[[i]])*100), 2), nsmall = 2))
      
      crescimento <- rbind(crescimento, novos_valores)
    }
    i <- i + 1
    j <- j + 1
  }
  rm(i)
  rm(j)
  return(crescimento)
}

#Função que faz um data table notas(Português, Matemática ou Média padronizada)
separa_palavra <- function(dat, palavra){
  colpalavra <- grep(palavra, names(dat), value = TRUE)
  return(dat[, ..colpalavra])
}

#Função que separa o data table por estado
separa_estado <- function(dt, uf){
  return(dt_estado <- dt[sigla_uf == uf])
}

#Função que retorna a cidade da maior nota de cada estado
maior_nota <- function(tab, uf){
  indice_maior <- which.max(as.matrix(tab))
  posicao <- arrayInd(indice_maior, dim(tab))
  linha <- posicao[1]
  coluna <- posicao[2]
  if(uf == 'CE'){
    return(dt_saeb[sigla_uf=='CE'][linha,3])
  }else{
    return(dt_saeb[sigla_uf=='PB'][linha,3])
  }
  rm(list = indice_maior, posicao, linha, coluna)
}

#Criando data tables dividos por estados
dt_ce <- separa_estado(dt_saeb, "CE")
dt_PB <- separa_estado(dt_saeb, "PB")

#Criando data table para notas
notas_mat_ce <- separa_palavra(dt_ce, "matematica")
notas_port_ce <- separa_palavra(dt_ce, "lingua")
notas_mat_pb <- separa_palavra(dt_PB, "matematica")
notas_port_pb <- separa_palavra(dt_PB, "lingua")
notas_padronizadas_ce <- separa_palavra(dt_ce, 'padronizada')
notas_padronizadas_pb <- separa_palavra(dt_pb, 'padronizada')

#Notas medias
mat_med_ce <- notas_media(notas_mat_ce, 'matematica')
mat_med_pb <- notas_media(notas_mat_pb, 'matematica')
port_med_ce <- notas_media(notas_port_ce, 'lingua')
port_med_pb <- notas_media(notas_port_pb, 'lingua')
padrao_med_ce <- notas_media(notas_padronizadas_ce, 'padroniza')
padrao_med_pb <- notas_media(notas_padronizadas_pb, 'padroniza')


#Criando data tables com crescimento médio de cada matéria por estado
crescimento_mat_ce <- taxa_crescimento(notas_mat_ce, "matematica")
crescimento_port_ce <- taxa_crescimento(notas_port_ce, "lingua")
crescimento_mat_pb <- taxa_crescimento(notas_mat_pb, "matematica")
crescimento_port_pb <- taxa_crescimento(notas_port_pb, "lingua")

#maiores notas de cada estado por materia
cidade_ce_mat <- format(round(max(notas_mat_ce), 2), nsmall = 2)
cidade_ce_port <- format(round(max(notas_port_ce), 2), nsmall = 2)
cidade_pb_mat <- format(round(max(notas_mat_pb), 2), nsmall = 2)
cidade_pb_port <- format(round(max(notas_port_pb), 2), nsmall = 2)

#gráfico

#Adicionando coluna do estado para diferenciar no gráfico
crescimento_mat_ce$estado <- 'Ceará'
crescimento_port_ce$estado <- 'Ceará'
crescimento_mat_pb$estado <- 'Pernambuco'
crescimento_port_pb$estado <- 'Pernambuco'

#Cruzando os dados de data tables de estados diferente de mesmas materias
dados_cruzados_mat <- rbind(crescimento_mat_ce, crescimento_mat_pb)
dados_cruzados_port <- rbind(crescimento_port_ce, crescimento_port_pb)

#Maiores notas
cidade_ce <- data.table(materias = c('Matemática', 'Português'), 
                        valores = c(cidade_ce_mat, cidade_ce_port))
cidade_ce$estado <- 'Ceará'

#Maiores notas de Pernambuco
cidade_pb <- data.table(materias = c('Matemática', 'Português'), 
                        valores = c(cidade_pb_mat, cidade_pb_port))
cidade_pb$estado <- 'Pernambuco'

#Cruzando os dados da notas
cidades_cruzadas <- rbind(cidade_ce, cidade_pb)

#Cruzando dados dos crescimento médio de mesma matérias e diferente estados
crescimento_mat <- rbind(crescimento_mat_ce, crescimento_mat_pb)
crescimento_port <- rbind(crescimento_port_ce, crescimento_port_pb)

#Cruzando dados das medias de matematica
mat_med_ce$estado <- 'Ceará'
mat_med_pb$estado <- 'Ceará'
mat_med_cruzada <- rbind(mat_med_ce, mat_med_pb)

#Cruzando dados das medias de portugues
port_med_ce$estado <- 'Ceará'
port_med_pb$estado <- 'Pernambuco'
port_med_cruzada <- rbind(port_med_ce, port_med_pb)

#Cruzando dados das medias Padronizadas
padrao_med_ce$estado <- 'Ceará'
padrao_med_pb$estado <- 'Pernambuco'
padrao_med_cruzada <- rbind(padrao_med_ce, padrao_med_pb)

cm <- ggplot(cidades_cruzadas, aes(x = materias, y = valores, fill = estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = valores), vjust = -0.5)+
  labs(title = "Maiores Notas",
       x = "materias",
       y = "valores")
ggsave('C:/caseLepesR/cm.png', cm)

cresc_mat <- ggplot(crescimento_mat, aes(x = intervalos, y = valores, fill = estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = valores), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Evolução média de matemática",
       x = "Ano",
       y = "Notas")
ggsave('C:/caseLepesR/cresc_mat.png', cresc_mat)

cresc_port <- ggplot(crescimento_port, aes(x = intervalos, y = valores, fill = estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = valores), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Taxa de crescimento médio de Português",
       x = "Ano",
       y = "Notas")
ggsave('C:/caseLepesR/cresc_mat.png', cresc_port)

med_mat <- ggplot(mat_med_cruzada, aes(x = ano, y = valores, fill = estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = valores), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Notas Médias de Matemática",
       x = "Ano",
       y = "Notas")
ggsave('mediamat.png', med_mat)

port_med <- ggplot(port_med_cruzada, aes(x = ano, y = valores, fill = estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = valores), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Notas Médias de Portugues",
       x = "Ano",
       y = "Notas")
ggsave('C:/caseLepesR/port_med.png', port_med)

medpadrao <- ggplot(padrao_med_cruzada, aes(x = ano, y = valores, fill = estado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = valores), vjust = -0.5, hjust = 0.5, size = 3) +
  labs(title = "Notas Médias Padronizadas",
       x = "Ano",
       y = "Notas")
ggsave('C:/caseLepesR/medpadrao.png', medpadrao)