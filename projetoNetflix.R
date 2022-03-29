##############################################################################
# 1. CARREGAMENTO DOS PACOTES
##############################################################################

library(dplyr)
library(tidyr)
library(readxl)
library(readr)


##############################################################################
# 2. CARREGAMENTOS DOS DATASETS
##############################################################################

# Dados NETFLIX
df_netflix <- read.csv('datasets_originais/dados_netflix_Dec_2021.csv')
View(df_netflix)

# Dados do World Bank (Para verificar o PIB dos países)
df_pib <- read.csv('datasets_originais/dados_world_bank.csv', header=F)
View(df_pib)

# Dados Desigualdade Salarial
df_salario <- read.csv('datasets_originais/dados_desigualdade_salarial_harvard.csv')
View(df_salario)

# Dados do IMDB
df_IMDB <- read_tsv('datasets_originais/dados_IMDB.tsv')
View(df_IMDB)

# Carregando dados dos TOP 10 shows da NETFLIX por país
df_top10 <- read_excel('datasets_originais/top_10_shows_netflix.xlsx')
View(df_top10)

# Carregando dados de assinantes da NETFLIX em Julho/2021
df_sub <- read.csv('datasets_originais/assinantes_netflix_jul_2021.csv')
View(df_sub)

# Carregando dados de códigos ISO dos países
df_countryCode <- read.csv('datasets_originais/wikipedia-iso-country-codes.csv')
View(df_countryCode)

##############################################################################
# 3. LIMPEZA E PREPARAÇÃO DOS DADOS
##############################################################################

################## 3.1. PRIMEIRO DATASET ##################
# Criação da coluna com a diferença de dados (plano standard - plano básico)
df_netflix$basic_standard_diff <- (df_netflix$Cost.Per.Month...Standard.... - 
                                     df_netflix$Cost.Per.Month...Basic....)

# Criação da coluna com a diferença de dados (plano premium - plano standard)
df_netflix$standard_premium_diff <- (df_netflix$Cost.Per.Month...Premium.... -
                                       df_netflix$Cost.Per.Month...Standard....)

# Combinação dos dados anteriores com o PIB
names(df_pib)[names(df_pib) == 'V1'] <- 'Country'
df_netflix_pib <- merge(df_netflix, df_pib, by='Country')
View(df_netflix_pib)

# Extrair o PIB de 2020
df_netflix_pib2020 <- df_netflix_pib[-c(11:72, 74, 75)]
names(df_netflix_pib2020)[names(df_netflix_pib2020) == 'V64'] <- '2020 GDP (World Bank)'
View(df_netflix_pib2020)

# Limpeza do Dataframe de Desigualdade salarial
df_salario <- df_salario[, c(1:3)]
df_salario_ano <- df_salario %>% group_by(country) %>% summarise(max=max(year, na.rm=T))

# Combinação dos Dataframes
df_salario <- merge(df_salario, df_salario_ano, by.x = c('Country', 'year'), by.y = c('country', 'max'))
df_netflix_pib_salario2020 <- merge(df_netflix_pib2020, df_salario, by.x=c('Country'), by.y = c('country'))
View(df_netflix_pib_salario2020)

# Limpeza do Dataframe de Faturamento e Subscrição
df_sub <- df_sub[,c(1,23,24)]

# Combinação dos Dataframes
complete <- merge(df_netflix_pib_salario2020, df_sub, by=c('Country'))

# Realização do merge do Countrycode (utilizado no Choropleth Map)
df_countryCode <- df_countryCode[,c(1, 3)]
complete <- merge(complete, df_countryCode, by.x=c('Country'), by.y = c('English.short.name.lower.case'))
View(complete)


################## 3.2. SEGUNDO DATASET ##################
# Limpeza e filtro Dataframe IMDB
genero <- df_IMDB[,-c(1, 4:8)]
View(genero)
names(genero)[names(genero) == 'primaryTitle'] <- 'show_title'

# Associação do gênero com os TOP 10 shows
topgenero <- merge(df_top10, genero, by='show_title')

# Limpeza do Dataframe anterior para manter apenas 1 entrada para cada TOP 10
topgenero <- topgenero[(topgenero$category == 'Films' & topgenero$titleType == 'movie') |
                         (topgenero$category == 'TV' & topgenero$titleType == 'tvSeries'),]
topgenero <- distinct(topgenero, show_title, week, country_name, titleType, cumulative_weeks_in_top_10, .keep_all = T)
View(topgenero)

topgeneropaises <- topgenero[,-c(1, 3:9)]
View(topgeneropaises)

# Realização de um PIVOT no Dataframe
topgeneropaises <- separate(topgeneropaises, c('genres'), c('genero1', 'genero2', 'genero3'), sep=',')
topgeneropaises <- pivot_longer(topgeneropaises, c('genero1', 'genero2', 'genero3'), 
                                names_to = 'genero123', values_to = 'genres')
View(topgeneropaises)

# Contagem do número de gêneros
generoCount <- count(topgeneropaises, country_name, genres)
generoCount <- na.omit(generoCount)
generoCount <- subset(generoCount, genres!='\\N')
generoCount$n <- as.numeric(generoCount$n)
View(generoCount)

################## 3.3. TERCEIRO DATASET ##################
# Renomeação do DataFrame anterior
sunburst <- rename(generoCount, label=country_name)

# Remoção dos traços (SCI-FI, por exemplo)
sunburst$genres <- sub('-', ' ', sunburst$genres)

# Ajuste do nome
sunburst$parent = c('total - ')
sunburst$parent <- paste(sunburst$parent, sunburst$genres)
sunburst$id = c(' - ')
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)

# Agregação
added <- aggregate(sunburst$n, list(sunburst$genres), FUN=sum)
added <- rename(added, label=Group.1)
added <- rename(added, n=x)
added$n <- as.numeric(added$n)
added$genres <- c(NA)
added$parent <- c('total')
added$id <- c(' - ')
added$id <- paste(added$parent, added$id)
added$id <- paste(added$id, added$label)
View(added)

# Cálculo da Soma
total <- sum(added$n)
total

# Combina tudo para o Dataframe Final
sunburst <- rbind(added, sunburst)
sunburst <- rbind(c('total', total, NA, NA, 'total'), sunburst)
sunburst <- sunburst[,-c(3)]
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)


################## 3.4. QUARTO DATASET ##################
# Trabalhando apenas com os TOP 10
top10sunburst <- sunburst[-c(1:28), ]
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)

# TOP 10 gêneros por país
top10sunburst <- top10sunburst %>% group_by(label) %>% top_n(10, n)
View(top10sunburst)

# Recalculo dos totais, ajuste e combinação do Dataframe
top10add <- aggregate(top10sunburst$n, list(top10sunburst$parent), FUN=sum)
top10add <- rename(top10add, id=Group.1)
top10add <- rename(top10add, n=x)
top10add$label <- sub('total - ', '', top10add$id)
top10add$parent <- c('total')
top10add$n <- as.numeric(top10add$n)
total <- sum(top10add$n)
top10sunburst <- rbind(top10add, top10sunburst)
top10sunburst <- rbind(c('total', total, NA, NA, 'total'), top10sunburst)
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)


################## 3.5. QUINTO DATASET ##################
# Filtro do Dataframe anterior e criação de um novo
nototal <- sunburst[-c(1), ]
nototal$parent <- sub('total - ', '', nototal$parent)
nototal$parent <- sub('total', NA, nototal$parent)
nototal$id <- sub('total - ', '', nototal$id)
View(nototal)


################## 3.6. SEXTO DATASET ##################
# Filtro do Dataframe anterior e criação de um novo
countrytree <- nototal[-c(1:28), ]
countrytree <- rename(countrytree, parents=label)
countrytree <- rename(countrytree, labels=parent)
countrytree$id <- c(' - ')
countrytree$id <- paste(countrytree$parent, countrytree$id)
countrytree$id <- paste(countrytree$id, countrytree$label)
countries <- aggregate(countrytree$n, list(countrytree$parents), FUN=sum)
countries <- rename(countries, labels=Group.1)
countries <- rename(countries, n=x)
countries$n <- as.numeric(countries$n)
countries$id <- countries$label
countries$parents <- c(NA)
countrytree <- rbind(countrytree, countries)
View(countrytree)
