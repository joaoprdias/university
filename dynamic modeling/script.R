###### SETUP INICIAL
# Import das bibliotecas a utilizar
library(dplyr)
library(stringr)
library(geosphere) # para calcular as distâncias ao centro da cidade e das atrações
library(lubridate) # para manipular datas
library(mice) # prever missings
library(car)
library(ggplot2)
library(psych)
library(caret)
library(glmnet)
library(lmtest)  # para testes de hipótese sobre os pressupostos dos resíduos
library(olsrr)  # para regressão (e testes de hipótese)
library(tseries)  # teste de Jarque-Bera
library(gvlma)
library(Metrics)


# Leitura do dataset
ds <- read.csv("listings.csv")

set.seed(1235)

###### EDA e pequenas transformações
# Visualizar a estrutura dos dados
str(ds)

# Verificar bairros e tipos de quartos
table(ds$neighbourhood)
table(ds$room_type)

# Converter a coluna 'room_type' em um factor por ser categórica
ds$room_type <- factor(ds$room_type)

# Transformar os bairros em factor
ds$neighbourhood <- factor(ds$neighbourhood)

# Converter a coluna last_review para o tipo de data
ds$last_review <- as.Date(ds$last_review, format = "%Y-%m-%d")



###### FEATURE ENGINEERING
## A partir da feature "name" vamos criar novas features com as substrings de rating; número de quartos; de camas; de casas de banho; WC partilhada/privada (Uniformizar nomes); estudio (?)
# Verificar os diferentes nomes que podem aparecer especificados na coluna name
textos_sem_numeros <- gsub("\\b\\d+\\.?\\d*\\b", "", substring(ds$name, first = regexpr("·", ds$name) + 2))
unique(textos_sem_numeros)

## ★New considera-se NA porque não tem review associada
## Ranking aparece sempre após a ★
## O número de camas pode aparecer como bed ou beds
## O número de quartos pode aparecer como bedroom ou bedrooms 
## Quando é estudio aparece como Studio 
## O número de casas de banho aparece como bath; baths; shared bath; shared baths; private bath; Half-bath; Shared half-bath; Private half-bath

# Extrair ranking
ds <- ds %>%
  mutate(rating = ifelse(grepl("★\\d+\\.\\d+", name),
                         as.numeric(gsub(".*★(\\d+\\.\\d+).*", "\\1", name)),
                         as.numeric(gsub(".*★(\\d+).*", "\\1", name))))

# Extrair número de quartos
ds <- ds %>%
  mutate(bedrooms = as.integer(gsub(".*\\b(\\d+)\\s*(bedroom|bedrooms).*", "\\1", name)))

# Extrair número de camas
ds <- ds %>%
  mutate(beds = as.integer(gsub(".*\\b(\\d+)\\s*(bed|beds).*", "\\1", name)))

# Variavel binaria para saber se é estudio (1: sim - 0: não)
ds$studio <- as.integer(grepl("Studio", ds$name))
ds$studio <- as.numeric(ds$studio)

# Extrair número de WCs 
ds <- ds %>%
  mutate(bathrooms = ifelse(grepl(".*\\b(\\d+\\.\\d*)\\s*(shared|private)?\\s*(bath|baths|half-bath|half-baths)\\b.*", name),
                            as.numeric(gsub(".*\\b(\\d+\\.\\d*)\\s*(shared|private)?\\s*(bath|baths|half-bath|half-baths)\\b.*", "\\1", name)),
                            as.numeric(gsub(".*\\b(\\d+(?:\\.\\d+)?)\\s*(shared|private)?\\s*(bath|baths|half-bath|half-baths)?\\b", "\\1", name))))

# Criar uma variável binária para quem tem licenciamento (com license = 1 / sem license = 0)
ds <- ds %>%
  mutate(has_license = ifelse(grepl(".", license), 1, 0))

## Ver distância ao centro da cidade (usar latitude e longitude) https://pt.db-city.com/Canad%C3%A1--Ont%C3%A1rio--Ottawa--Ottawa
# Coordenadas do centro de Ottawa
ottawa_latitude <- 45.4208
ottawa_longitude <- -75.69
# Criar distancia ao centro de ottawa em km (distVincentyEllipsoid dá em metros e dividimos por 1000 para ter em km)
ds <- ds %>% 
  rowwise() %>%
  mutate(distance_ottawa_km = distVincentyEllipsoid(c(longitude, latitude), c(ottawa_longitude, ottawa_latitude)) / 1000)


## Ver distância para as principais atrações (https://www.tripadvisor.pt/Attractions-g155004-Activities-oa0-Ottawa_Ontario.html) - considerar o top3
# Coordenadas de Parliament Hill
parliament_hill_latitude <- 45.424807
parliament_hill_longitude <- -75.699234
# Criar distancia ao até ao parliament hill em kms (https://latitude.to/articles-by-country/ca/canada/4574/parliament-hill)
ds <- ds %>% 
  rowwise() %>%
  mutate(distance_parliament_hill_km = distVincentyEllipsoid(c(longitude, latitude), c(parliament_hill_longitude, parliament_hill_latitude)) / 1000)

# Coordenadas de Canadian War Museum
canadian_museum_latitude <- 45.417156
canadian_museum_longitude <- -75.716829
# Criar distancia ao até ao Canadian War Museum em kms (https://latitude.to/articles-by-country/ca/canada/15394/canadian-war-museum)
ds <- ds %>% 
  rowwise() %>%
  mutate(distance_canadian_museum_km = distVincentyEllipsoid(c(longitude, latitude), c(canadian_museum_longitude, canadian_museum_latitude)) / 1000)


# Coordenadas de Rideau Canal National Historic Site
rideau_canal_latitude <- 45.422164978
rideau_canal_longitude <- -75.6916639
# Criar distancia ao até ao Rideau Canal National Historic Site em kms (https://latitude.to/articles-by-country/ca/canada/5441/rideau-canal)
ds <- ds %>% 
  rowwise() %>%
  mutate(distance_rideau_canal_km = distVincentyEllipsoid(c(longitude, latitude), c(rideau_canal_longitude, rideau_canal_latitude)) / 1000)


## Separar host_name por listagens por pessoas individuais e por organizações


# Calcular a diferença em dias entre a última review e a data atual (review age em weeks)
ds$review_age_weeks <- ifelse(!is.na(ds$last_review), 
                        as.numeric(difftime(Sys.Date(), ds$last_review, units = "weeks")),
                        NA)

###### DROP DE COLUNAS
# Drop column neighbourhood_group, porque tem tudo empty e já temos essa info em neighbourhood;
ds <- dplyr::select(ds, -neighbourhood_group)

## id; host_id; host_name são para remover (IDs)
ds <- dplyr::select(ds, -id, -host_id)

## retirar variaveis utilizadas em feature engineering
ds <- dplyr::select(ds, -license, -latitude, -longitude, -last_review)


###### EDA
## Target Variable
ggplot(ds)+
  geom_histogram(aes(ds$price), fill = "black")+
  xlab("Airbnb pricing")
### temos outliers na variável de pricing

# Ver preço mediano (para não ser afetado por outliers) em cada bairro
aggregate(price ~ neighbourhood, data = ds, FUN = median)

## Identificar os outliers no target
# Calcular o intervalo interquartil (IQR)
Q1 <- quantile(ds$price, 0.25, na.rm=TRUE)
Q3 <- quantile(ds$price, 0.75, na.rm=TRUE)
IQR <- Q3 - Q1
# Definir os limites para identificar outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
# Identificar outliers
outliers <- na.omit(ds[ds$price < limite_inferior | ds$price > limite_superior, ])

## a maior parte dos outliers são de entidades com licença, mas apresentam valores MUITO extremos, pelo que vamos optar por removê-los através do critério do IQR
ds <- ds[is.na(ds$price) | ds$price < 299, ]

# para corrigir a assimetria, logaritmizamos
ds$log_price <- log(ds$price)

str(ds)

## Diagramas de dispersão entre todas as variáveis
#pairs(ds[, c("price", "review_age_weeks", "distance_ottawa_km", "distance_rideau_canal_km", "distance_canadian_museum_km", "distance_parliament_hill_km", "bathrooms", "beds", "bedrooms", "rating", "number_of_reviews_ltm", "availability_365", "calculated_host_listings_count", "reviews_per_month", "number_of_reviews", "minimum_nights")])
## INSIGHTS dos pairs
### multicolinearidade forte na distância ao centro de ottawa e a distância até às atrações (vamos remover as atrações para modelar)
### outliers extremos em number_of_reviews; reviews_per_month; bedrooms


# remover preço nao log
ds <- dplyr::select(ds, -price)

## ver o target depois de logaritmizar
ggplot(ds)+
  geom_histogram(aes(log_price), fill = "black")+
  xlab("Airbnb pricing")

# Histograma number_of_reviews
ggplot(ds)+
  geom_histogram(aes(number_of_reviews), fill = "black")+
  xlab("number of reviews")

# Histograma number_of_reviews
ggplot(ds)+
  geom_histogram(aes(reviews_per_month), fill = "black")+
  xlab("reviews per month")

boxplot(ds$number_of_reviews)

### remover os outliers de number_of_reviews; reviews_per_month
## Identificar os outliers no number_of_reviews
# Calcular o intervalo interquartil (IQR)
Q1 <- quantile(ds$number_of_reviews, 0.25, na.rm=TRUE)
Q3 <- quantile(ds$number_of_reviews, 0.75, na.rm=TRUE)
IQR <- Q3 - Q1
# Definir os limites para identificar outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
# Identificar outliers
outliers_rv <- na.omit(ds[ds$number_of_reviews < limite_inferior | ds$number_of_reviews > limite_superior, ])

## a maior parte dos outliers são de entidades com licença, mas apresentam valores MUITO extremos, pelo que vamos optar por removê-los através do critério do IQR
ds <- ds[is.na(ds$number_of_reviews) | ds$number_of_reviews < 116, ]


## Identificar os outliers no reviews_per_month
# Calcular o intervalo interquartil (IQR)
Q1 <- quantile(ds$reviews_per_month, 0.25, na.rm=TRUE)
Q3 <- quantile(ds$reviews_per_month, 0.75, na.rm=TRUE)
IQR <- Q3 - Q1
# Definir os limites para identificar outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
# Identificar outliers
outliers_rv_m <- na.omit(ds[ds$reviews_per_month < limite_inferior | ds$reviews_per_month > limite_superior, ])

ds <- ds[is.na(ds$reviews_per_month) | ds$reviews_per_month < 4.82, ]


## Identificar os outliers no number_of_reviews_ltm
# Calcular o intervalo interquartil (IQR)
Q1 <- quantile(ds$number_of_reviews_ltm, 0.25, na.rm=TRUE)
Q3 <- quantile(ds$number_of_reviews_ltm, 0.75, na.rm=TRUE)
IQR <- Q3 - Q1
# Definir os limites para identificar outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
# Identificar outliers
outliers_rv_ltm <- na.omit(ds[ds$number_of_reviews_ltm < limite_inferior | ds$number_of_reviews_ltm > limite_superior, ])

ds <- ds[is.na(ds$number_of_reviews_ltm) | ds$number_of_reviews_ltm < 17.5, ]


# criar uma nova variável de impacto das reviews através da multiplicação do numero de reviews pelas reviews por mes e logaritmizando para corrigir a assimetria (acrescenta-se 0.1 para a multiplicação não ser por 0, mas considerarmos esse fator)
ds$log_reviews_impact <- log(ds$number_of_reviews * ds$reviews_per_month * (ds$number_of_reviews_ltm + 0.1))
ds <- dplyr::select(ds, -number_of_reviews, -reviews_per_month, -number_of_reviews_ltm)

# Histograma da nova variavel
ggplot(ds)+
  geom_histogram(aes(log_reviews_impact), fill = "black")+
  xlab("log reviews impact")

# Histograma do rating 
ggplot(ds)+
  geom_histogram(aes(rating), fill = "black")+
  xlab("rating")

## remover outliers distance_ottawa_kme e review_age_weeks

# Calcular o intervalo interquartil (IQR)
Q1 <- quantile(ds$distance_ottawa_km, 0.25, na.rm=TRUE)
Q3 <- quantile(ds$distance_ottawa_km, 0.75, na.rm=TRUE)
IQR <- Q3 - Q1
# Definir os limites para identificar outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
# Identificar outliers
outliers_ottawa <- na.omit(ds[ds$distance_ottawa_km < limite_inferior | ds$distance_ottawa_km > limite_superior, ])

ds <- ds[is.na(ds$distance_ottawa_km) | ds$distance_ottawa_km < 39.3, ]


# Histograma distance ottawa
ggplot(ds)+
  geom_histogram(aes(distance_ottawa_km), fill = "black")+
  xlab("distance_ottawa_km")


# Calcular o intervalo interquartil (IQR)
Q1 <- quantile(ds$review_age_weeks, 0.25, na.rm=TRUE)
Q3 <- quantile(ds$review_age_weeks, 0.75, na.rm=TRUE)
IQR <- Q3 - Q1
# Definir os limites para identificar outliers
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
# Identificar outliers
outliers_review_age <- na.omit(ds[ds$review_age_weeks < limite_inferior | ds$review_age_weeks > limite_superior, ])

ds <- ds[is.na(ds$review_age_weeks) | ds$review_age_weeks < 206, ]


# Histograma review_age_weeks
ggplot(ds)+
  geom_histogram(aes(review_age_weeks), fill = "black")+
  xlab("review_age_weeks")

# Logaritmizar distance_ottawa_km e review_age_weeks
ds$log_distance_ottawa_km <- log(ds$distance_ottawa_km)
ds$log_review_age_weeks <- log(ds$review_age_weeks)

ds <- dplyr::select(ds, -distance_ottawa_km, -review_age_weeks)

# Histograma nova variavel distancia ao centro
ggplot(ds)+
  geom_histogram(aes(log_distance_ottawa_km), fill = "black")+
  xlab("log_distance_ottawa_km")

# Histograma nova variavel log_review_age_weeks
ggplot(ds)+
  geom_histogram(aes(log_review_age_weeks), fill = "black")+
  xlab("log_review_age_weeks")


# Barplot número de quartos
ggplot(ds, aes(x = bedrooms)) +
  geom_bar(fill = "black") +  # Estilo de barras com contagem automática
  labs(x = "Número de Quartos", y = "Contagem") +    # Rótulos dos eixos x e y
  ggtitle("Contagem de Quartos") +                   # Título do gráfico
  theme_minimal()

# Barplot número de camas
ggplot(ds, aes(x = beds)) +
  geom_bar(fill = "black") +  # Estilo de barras com contagem automática
  labs(x = "Número de Camas", y = "Contagem") +    # Rótulos dos eixos x e y
  ggtitle("Contagem de Camas") +                   # Título do gráfico
  theme_minimal()
## outlier em camas e quartos

# Barplot número de casas de banho
ggplot(ds, aes(x = bathrooms)) +
  geom_bar(fill = "black") +  # Estilo de barras com contagem automática
  labs(x = "Número de WCs", y = "Contagem") +    # Rótulos dos eixos x e y
  ggtitle("Contagem de WCs") +                   # Título do gráfico
  theme_minimal()

# Barplot licenciamentos
ggplot(ds, aes(x = has_license)) +
  geom_bar(fill = "black") +  # Estilo de barras com contagem automática
  labs(x = "License", y = "Contagem") +    # Rótulos dos eixos x e y
  ggtitle("Tem licensa?") +                   # Título do gráfico
  theme_minimal()

# Barplot estudio
ggplot(ds, aes(x = studio)) +
  geom_bar(fill = "black") +  # Estilo de barras com contagem automática
  labs(x = "Studio", y = "Contagem") +    # Rótulos dos eixos x e y
  ggtitle("É um estúdio?") +                   # Título do gráfico
  theme_minimal()

# Histograma availability
ggplot(ds)+
  geom_histogram(aes(availability_365), fill = "black")+
  xlab("availability")

# Definindo os pontos de corte para transformar em categorias
cut_points <- c(-1, 90, 180, 270, 366)  # dividindo o ano em trimestres
# Transformando 'availability_365' em uma variável categórica
ds$availability_category <- cut(ds$availability_365, cut_points, labels = c("Baixa", "Média-Baixa", "Média-Alta", "Alta"))

# Barplot categorias de disponibilidade
ggplot(ds, aes(x = availability_category)) +
  geom_bar(fill = "black") +  # Estilo de barras com contagem automática
  labs(x = "Availability", y = "Contagem") +    # Rótulos dos eixos x e y              
  theme_minimal()

ds <- dplyr::select(ds, -availability_365)

# Histograma
ggplot(ds)+
  geom_histogram(aes(minimum_nights), bins = 20, fill = "black")+
  xlab("minimum_nights")

table(ds$minimum_nights)

# Definindo os pontos de corte para transformar em categorias
cut_points <- c(-1, 1, 7, 30, 366)
# Transformando 'minimum_nights' em uma variável categórica
ds$minimum_stay <- cut(ds$minimum_nights, cut_points, labels = c("Uma noite", "Uma semana", "Um mês", "mais de um mês"))

# Barplot estadia minima
ggplot(ds, aes(x = minimum_stay)) +
  geom_bar(fill = "black") +  # Estilo de barras com contagem automática
  labs(x = "Estadia minima", y = "Contagem") +    # Rótulos dos eixos x e y              
  theme_minimal()

ds <- dplyr::select(ds, -minimum_nights)

# Histograma
ggplot(ds)+
  geom_histogram(aes(calculated_host_listings_count), fill = "black")+
  xlab("calculated_host_listings_count")

table(ds$calculated_host_listings_count)

# aparecem as mesmas empresas (corporate stays por ex. várias x no dataset)

# Definindo os pontos de corte para transformar em categorias
cut_points <- c(-1, 1, Inf)  # primeira listagem vs mais do que uma
# Transformando 'host_listings_category' em uma variável categórica
ds$host_listings_category <- cut(ds$calculated_host_listings_count, cut_points, labels = c("Primeira listagem", "Mais do que uma listagem"))

# Barplot host_listings_category
ggplot(ds, aes(x = host_listings_category)) +
  geom_bar(fill = "black") +  # Estilo de barras com contagem automática
  labs(x = "host_listings_category", y = "Contagem") +    # Rótulos dos eixos x e y              
  theme_minimal()

ds <- dplyr::select(ds, -calculated_host_listings_count)

###### TRATAMENTO DE MISSINGS
# Verificar missings por coluna
colSums(is.na(ds))
# Verificar missings por linha
missing_info <- ds[rowSums(is.na(ds)) > 1, ]

# Filtrar as linhas onde a coluna 'bathrooms' contém NA
ds_missings_bathrooms <- subset(ds, is.na(bathrooms))
## Todas as WCs que têm Half não têm número atribuído e como tal aparecem com NA. Nestes 8 casos teremos de imputar o número 1. Contudo, há dois casos sem WC especificada, pelo que imputaremos o número 1 (pelo número de camas que têm) 
ds$bathrooms[is.na(ds$bathrooms)] <- 1
#retiramos os outliers de WCs (identificado anteriormente)
ds <- ds[ds$bathrooms < 7.5, ]

# Filtrar as linhas onde a coluna 'bedrooms' contém NA
ds_missings_bedrooms <- subset(ds, is.na(bedrooms))
## A maior parte dos missings regista-se quando se trata de um Estúdio. Como um estúdio implica a existência de um quarto imputamos 1
ds$bedrooms[is.na(ds$bedrooms)] <- 1
ds <- ds[ds$bedrooms < 9, ]

# Filtrar as linhas onde a coluna 'beds' contém NA
ds_missings_beds <- subset(ds, is.na(beds))
## Os missings são em estudio. Em alguns casos, após alguma pesquisa vemos que em estudio é comum ter-se sofá-cama. Como tal, e por se tratar de um estúdio, vamos imputar 1 nestes casos, por ser o número mais comum de camas num estúdio. 
ds$beds[is.na(ds$beds)] <- 1
#retiramos os outliers de camas (identificado anteriormente)
boxplot(ds$beds)
ds <- ds[ds$beds <= 6, ]

# Filtrar as linhas onde a coluna 'rating' contém NA
ds_missings_ratings <- subset(ds, is.na(rating))
## Listagens pela primeira vez ou informação não disponibilizada no name. 


# de acordo com o outlierTest no final
#indices_a_remover <- c(513, 506, 812, 1171, 530, 2218, 529, 528)
#ds <- ds[-indices_a_remover, ]


# drop de colunas nao utilizadas
ds <- dplyr::select(ds, -name, -host_name)

## Modelação preditiva com MICE para as restantes variáveis com missings
md.pattern(ds)
ds <- complete(mice(ds, method = "pmm", m = 5, maxit=100, seed=500))

###### AJUSTAR DATA TYPES
# eliminar o atributo que se criou com o dplyr para pre processar os dados
attr(ds, "groups") <- NULL
str(ds)

corr_mat <- cor(ds[, 3:15])

# Encoding categorical variables
ds <- ds %>% mutate_if(is.factor, as.numeric)


###### MODELING
# Identificar as variáveis numéricas
fit <- lm(log_price ~., data=ds)
summary(fit)

# Multicolinearidade (foi necessário remover -distance_parliament_hill_km, -distance_canadian_museum_km, -distance_rideau_canal_km, -neighbourhood)
vif(fit)

ds <- dplyr::select(ds, -distance_parliament_hill_km, -distance_canadian_museum_km, -distance_rideau_canal_km, -neighbourhood)

fit2 <- lm(log_price ~., data=ds)
summary(fit2)

step(fit2, direction="backward")

model2 <- lm(formula = log_price ~ room_type + rating + bedrooms + beds + 
               studio + bathrooms + has_license + log_reviews_impact + log_distance_ottawa_km + 
               log_review_age_weeks + minimum_stay + host_listings_category, 
             data = ds)
summary(model2)

# Não há multicolinearidade
vif(model2)

# Verificar os pressupostos dos residuos
par(mfrow=c(2,2)) 
plot(model2)

# residuos com média nula - ok
mean(model2$residuals)

# teste de Breusch-Pagan (H0: erros homocedásticos) - como o p-value é menor para o nivel de significancia (0.05 por ex.) então há evidência estatística para rejeitar a hipótese nula de homocedasticidade
## pressuposto não verificado - possiveis outliers (?)
bptest(model2)

# teste de Breusch-Godfrey (H0:resíduos independentes)
bgtest(model2)
# p-value < 0.05, rejeitamos a HO, logo o pressuposto não é verificado (resíduos nao sao independentes)

# Teste de Jarque-Bera (H0: distribuição normal)
jarque.bera.test(model2$residuals)
# p-value < 0.05, rejeitamos a HO, logo o pressuposto não é verificado (resíduos nao estao normalm. distribuídos)

# Testes de uma so vez
gvmodel <- gvlma(model2)
summary(gvmodel)

# teste de outliers
outlierTest(model2)

# Lasso regression (from caret, alpha=1)
my_control = trainControl(method="cv", number=5)
Grid = expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.02))
lasso_lr <-train(log_price ~., data = ds, method='glmnet', trControl= my_control, tuneGrid = Grid)
lasso_lr

# Ridge regression (from caret, alpha=0)
Grid = expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.02))
ridge_linear_reg_mod = train(log_price ~., data = ds, method='glmnet', trControl= my_control, tuneGrid = Grid)
ridge_linear_reg_mod


# Dividir os dados em conjunto de treinamento e teste
set.seed(123)  # Para garantir a reprodutibilidade
indice_treino <- sample(1:nrow(ds), 0.8 * nrow(ds))
dados_treino <- ds[indice_treino, ]
dados_teste <- ds[-indice_treino, ]

# Ajustar o modelo nos dados de treinamento
model23 <- lm(log_price ~., data=dados_treino)

# Avaliar o desempenho do modelo nos dados de teste
previsoes <- predict(model23, newdata = dados_teste)


my_pred <-exp(previsoes)
actual <- exp(dados_teste$log_price)
plot(my_pred,actual)



result11 = rmse(actual,my_pred)
result21 = mape(actual,my_pred)
result11
result21


plot(exp(ds$log_price), col="red", type="o", pch="o", lty=1, xlim=c(1,80))
lines(exp(model23$fitted.values), type="o", col="blue", pch="o")


