#Importação de dados
getwd()
setwd("C:/Users/Dante Ribeiro/Documents/PS ESTAT")
dados <- read.csv("athlete_events.csv", sep = ",")
View(dados)

## Preparação dos dados para a primeira análise ##
# Instalando o pacote dplyr e tidyverse
install.packages("tidyverse")
install.packages("dplyr")
library("tidyverse")
library("dplyr")


# Separando o ano do tipo de olimpíada da variável Games
A <- dados %>% separate(Games, into = c("Year","Game"), sep = c(" ")) 

# Filtrando os tipos de jogo (para jogos de verão), agrupando por Sexo e Ano,
# retirando nomes repetidos por ano e contando o número de pessoas de cada sexo
A %>% filter(Game == "Summer") %>% group_by(Sex, Year) %>% 
  distinct(Name) %>% count(Sex) -> finalmente

# Transformando a variável Ano em numérica
finalmente$Year <- as.numeric(finalmente$Year)
class(finalmente$Year)

# Primeira análise: Série histórica do nº de participantes 
# nas olimpíadas de verão para cada sexo

# Padronização ESTAT

cores_estat <-  c('#A11D21','#663333','#FF6600','#CC9900','#CC9966',
     '#999966','#006606','#008091','#003366','#041835','#666666')


theme_estat <-  function(...) {
theme<-  ggplot2::theme_bw() +
ggplot2::theme(axis.title.y = ggplot2::element_text(colour ="black",size =12),
axis.title.x = ggplot2::element_text(colour ="black",size =12),
axis.text= ggplot2::element_text(colour ="black", size= 9.5),
panel.border = ggplot2::element_blank(),
axis.line = ggplot2::element_line(colour ="black"),
legend.position ="top",
...
)

return(
  list(
    theme
    ,scale_fill_manual(values = cores_estat),
    scale_colour_manual(values = cores_estat)
  )
)
} 

# Instalar o pacote ggplot2
install.packages("ggplot2")
library("ggplot2")

# Construção do gráfico de linhas
ggplot(finalmente, aes(x=Year,y=n,group=Sex,colour=Sex)) +
  geom_line(size=1) + geom_point(size=2) +
  scale_colour_manual(name="Sexo", values = c("#A11D21", "#003366"), labels = c("Feminino", "Masculino"))+
  labs(x= "Anos",y="Quantidade de atletas") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  theme(legend.position="top")

# Salvando o gráfico
ggsave("seriespadrao.pdf", width =158, height =93, units ="mm")

## Preparação dos dados para a segunda análise ##
# Retirando os missing values das variáveis que serão analisadas (idade e medalha)
dados <- dados[!is.na(dados$Medal),]
dados <- dados[!is.na(dados$Age),]

# Fazendo um dataframe com as variáveis idade e tipo de medalha
grupo <- dados %>% select(Age, Medal) 

# Renomeando as observações da variável medalha
grupo$Medal <- recode(grupo$Medal,"Gold"="Ouro", "Silver" = "Prata")
  
# Ordenando a variável qualitativa ordinal (categorias das medalhas)
grupo$Medal <- factor(grupo$Medal,levels = c('Ouro',"Prata",'Bronze'),ordered = TRUE)

## Segunda análise: Correlação entre número de medalhas e idade ##

# Construção do gráfico (boxplot)
ggplot(grupo) +
  aes(x= Medal, y=Age) +
  geom_boxplot(fill =c("#A11D21"), width =0.5) +
  stat_summary(
  fun ="mean", geom ="point", shape =23, size =3, fill ="white" ) +
  labs(x = "Categorias das medalhas", y ="Idade dos atletas") +
  theme_estat() +
  theme(plot.title = element_text(hjust = 0.5))

# Salvando o gráfico
ggsave("box_uni.pdf", width = 158, height = 93, units = "mm")
          
# Separando os dados para o quadro de medidas resumo
ouro <- grupo %>% filter(Medal == "Ouro")
summary(ouro$Age)

prata <- grupo %>% filter(Medal == "Prata")
summary(prata$Age)

bronze <- grupo %>% filter(Medal == "Bronze")
summary(bronze$Age)


## Importando arquivo com os continentes relacionados aos códigos dos países ##
# Usado como auxílio para essa terceira análise
install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

json_file <- 'https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# Obtendo uma lista de todos os recursos
print(json_data$resources$name)

# Imprimindo todos os dados tabulares 
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
}

## Preparação dos dados para a terceira análise ##
# Fazendo um data.frame apenas com os códigos dos países e seu respectivo continente
continente <- data %>% select(Three_Letter_Country_Code,Continent_Name) 

# Mudando o nome da variável para ficar igual à da base de dados
"NOC" -> names(continente)[1]

# Selecionando apenas os códigos dos países e suas respectivas medalhas do banco de dados
M <- A %>% select(NOC, Medal) 

# Juntando os continentes com suas respectivas medalhas de acordo com o código do país
final <- merge(continente, M, by="NOC")

# Retirando os NAs da variável medalhas
final <- final[!is.na(final$Medal),]

# Alterando os nomes dos continentes para português e agrupando as Américas
final$Continent_Name <- recode(final$Continent_Name,"North America" = "América", "South America" = "América")
final$Continent_Name <- recode(final$Continent_Name,"Asia" = 'Ásia', "Europe" = "Europa", "Africa" = "África")

# Contabilizando o total de pódios por continente
total <- final %>% group_by(Continent_Name) %>% count()

## Terceira análise: Quantidade de pódios por continente ##

# Transformando o banco de dados em um dataframe
total <- as.data.frame(total)

# Construção do gráfico de colunas
ggplot(total) +
  aes(x = fct_reorder(Continent_Name,n, .desc = T), y= n,label = n)+
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -.5,
    size = 3
  ) + 
  labs(x = "Continentes", y = "Quantidade de pódios") +
  theme_estat()


# Salvando o gráfico
ggsave("colunas_padrao.pdf",width =158, height =93, units ="mm")

## Preparação dos dados para a quarta análise ##
# Utilizando o arquivo com os tipos de jogos separados
view(A)

# Retirando os missing values das variáveis que serão estudadas (altura e peso)
A <- A[!is.na(A$Height),]
A <- A[!is.na(A$Weight),] 

# Criando um arquivo apenas com as variáveis que serão utilizadas para a fórmula
calculo <- A %>% select("Weight","Height","Game")

# Colocando as alturas em metros ( estavam em centímetros) 
calculo["Altura"] <- (calculo$Height/100)  

# Adicionando uma coluna com o valor do IMC
calculo["IMC"] <- round((calculo$Weight/(calculo$Altura^2)),2)

# Selecionando as variáveis para o gráfico
fim <- calculo %>% select("IMC","Game")

# Mudando o nome dos dados para português
fim$Game <- recode(fim$Game, "Summer"="Verão", "Winter"="Inverno")

## Quarta análise: Distribuição dos IMCs por jogos de verão e inverno ##

# Construção do gráfico (boxplot)
ggplot(fim) +
aes(x= Game, y= IMC)+
geom_boxplot(fill =c("#A11D21"), width =0.5) +
stat_summary(
fun ="mean", geom ="point", shape =23, size =3, fill ="white" ) +
labs(x = "Tipos de jogos por estação", y ="IMCs dos atletas")+
theme_estat() +
theme(plot.title = element_text(hjust = 0.5))

# Salvando o gráfico
ggsave("box2.pdf",width=158, height = 93, units = "mm")

# Dados para a construção do quadro com as medidas-resumo
verao <- fim %>% filter(Game == "Verão")
summary(verao$IMC)

inverno <- fim %>% filter(Game == "Inverno")
summary(inverno$IMC)

## Preparação dos dados para a quinta análise ## 

# Utilizando o banco de dados original
dados <- read.csv("athlete_events.csv", sep = ",")

# Colocando peso 3 na medalha de ouro, 2 na de prata e 1 na de bronze
dados$Medal[dados$Medal == "Gold"] <- 3
dados$Medal[dados$Medal == "Silver"] <- 2
dados$Medal[dados$Medal == "Bronze"] <- 1

# A variável medalha está como character, irei transformá-la em numérica
dados$Medal <- as.numeric(as.character(dados$Medal))

# Verificando o tipo de variável
glimpse(dados$Medal)

# Selecionando as variáveis Nome e peso das medalhas
pesos2 <- dados %>% select(Name,Medal)

# Retirando os missing values das medalhas
pesos2 <- pesos2[!is.na(pesos2$Medal),]

# Contabilizando os "pontos" por jogador e somando o valor dos pesos
quinta <- pesos2 %>% 
  group_by(Name) %>% summarise(Valor = sum(Medal))

## Quinta análise: Top 5 de participantes, considerando o ganho das medalhas
## com pesos diferentes

# Colocando em ordem decrescente para verificar os cinco primeiros
quinta <- quinta %>% arrange(desc(Valor))

# Selecionando as cinco primeiras linhas
quinta <-quinta[1:5,]

# Construção do gráfico de barras
ggplot(quinta) +
  aes(x = Valor, y = fct_reorder(Name,Valor, .desc=F),label = Valor)+
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    hjust = -.2,
    size = 3
  ) + 
  labs(x = "Contagem das medalhas com seus pesos", y = "Nome dos atletas") +
  theme_estat()

# Salvando o gráfico
ggsave("barras_padrao.pdf",width =158, height =93, units ="mm")

## Separando dados sobre cada participante para colocar na tabela e na análise
# Dados sobre Michael Fred Phelps, II
phelps <- dados %>% filter(Name == "Michael Fred Phelps, II")

# Dados sobre Larysa Semenivna Latynina (Diriy-)
laryssa <- dados %>% filter(Name == "Larysa Semenivna Latynina (Diriy-)")

# Dados sobre Nikolay Yefimovich Andrianov
nikolay <- dados %>% filter(Name == "Nikolay Yefimovich Andrianov")

# Dados sobre Ole Einar Bjrndalen
ole <- dados %>% filter(Name == "Ole Einar Bjrndalen")

# Dados sobre Paavo Johannes Nurmi
paavo <- dados %>% filter(Name == "Paavo Johannes Nurmi")

## Termo aditivo ##
## Sexta análise: Países com menor número de participações entre as edições ##

# Contando quantas vezes cada país participou dos jogos Olímpicos
paises <- dados %>% count(NOC)

# Colocando em ordem crescente
paises <- paises %>% arrange(n)

# Selecionando os sete primeiros países
paises <- paises[1:7,]

# Substituindo os códigos dos países por seus nomes
paises$NOC[paises$NOC == "NFL"] <- "Terra Nova"
paises$NOC[paises$NOC == "NBO"] <- "Sabá"
paises$NOC[paises$NOC == "SSD"] <- "Sudão do Sul"
paises$NOC[paises$NOC == "YMD"] <- "Iémen do Sul"
paises$NOC[paises$NOC == "TUV"] <- "Tuvalu"
paises$NOC[paises$NOC == "KOS"] <- "Kosovo"

# Excluindo a linha que o nome do país é desconhecido 
paisesf <- paises[-3,]

# Construção do gráfico de colunas
ggplot(paisesf) +
  aes(x = fct_reorder(NOC,n, .desc=T), y=n,label = n)+
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -.5,
    size = 3
  ) + 
  labs(x = "Países", y = "Quantidade de participações") +
  theme_estat()

# Salvando o gráfico
ggsave("paises_padrao.pdf",width =158, height =93, units ="mm")

### FIM! ###