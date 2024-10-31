# Bibliotecas necessarias para o projeto
library(readxl)
library(tidyr)
library(mice)
library(dplyr) 
library(corrplot)
library(cluster)
library(psych)
library(purrr)
library(car)
library(mclust)


# Importar o dataset e guardar na variavel ds
ds <- read_excel('data_wdi2021.xlsx')

# Contagem dos países com indicadores disponíveis
available_countries <- as.data.frame(table(ds$`Country Name`))
available_countries <- available_countries[order(-available_countries$Freq), ]

# Contagem de indicadores
available_indicators <- as.data.frame(table(ds$`Series Name`))
available_indicators <- available_indicators[order(-available_indicators$Freq), ]


#################################################################################################################


# Df filtrado com primeira versão das variáveis a utilizar
lista_titulos <- c("Access to electricity (% of population)", "Adequacy of social protection and labor programs (% of total welfare of beneficiary households)", "Adequacy of unemployment benefits and ALMP (% of total welfare of beneficiary households)", "Adjusted net national income per capita (current US$)", "Adolescents out of school (% of lower secondary school age)", "Child employment in agriculture (% of economically active children ages 7-14)", "Child employment in manufacturing (% of economically active children ages 7-14)", "Child employment in services (% of economically active children ages 7-14)", "Children in employment, study and work (% of children in employment, ages 7-14)", "Children out of school, primary", "Contributing family workers, total (% of total employment) (modeled ILO estimate)", "Coverage of unemployment benefits and ALMP (% of population)", "Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)", "Educational attainment, at least completed post-secondary, population 25+, total (%) (cumulative)", "Educational attainment, at least completed primary, population 25+ years, total (%) (cumulative)", "Educational attainment, at least Master's or equivalent, population 25+, total (%) (cumulative)", "Educational attainment, Doctoral or equivalent, population 25+, total (%) (cumulative)", "Employers, total (% of total employment) (modeled ILO estimate)", "Employment in agriculture (% of total employment) (modeled ILO estimate)", "Employment in industry (% of total employment) (modeled ILO estimate)", "Employment in services (% of total employment) (modeled ILO estimate)", "Employment to population ratio, 15+, total (%) (modeled ILO estimate)", "Expenditure on primary education (% of government expenditure on education)", "Expenditure on secondary education (% of government expenditure on education)", "Expenditure on tertiary education (% of government expenditure on education)", "Human capital index (HCI) (scale 0-1)", "Inflation, consumer prices (annual %)", "Interest payments (% of expense)", "Labor force participation rate for ages 15-24, total (%) (modeled ILO estimate)", "Labor force with basic education (% of total working-age population with basic education)", "Literacy rate, adult total (% of people ages 15 and above)", "Lower secondary completion rate, total (% of relevant age group)", "Net primary income (BoP, current US$)", "Net secondary income (BoP, current US$)", "Over-age students, primary (% of enrollment)", "Part time employment, total (% of total employment)", "Population density (people per sq. km of land area)", "Population growth (annual %)", "Primary completion rate, total (% of relevant age group)", "Primary education, teachers", "Proportion of people living below 50 percent of median income (%)", "School enrollment, primary (% gross)", "School enrollment, secondary (% gross)", "Self-employed, total (% of total employment) (modeled ILO estimate)", "Unemployment with advanced education (% of total labor force with advanced education)", "Unemployment with basic education (% of total labor force with basic education)", "Unemployment, total (% of total labor force) (modeled ILO estimate)", "Vulnerable employment, total (% of total employment) (modeled ILO estimate)", "Wage and salaried workers, total (% of total employment) (modeled ILO estimate)", "Personal remittances, paid (current US$)", "Ratio of female to male labor force participation rate (%) (modeled ILO estimate)", "Preprimary education, duration (years)", "Secondary education, duration (years)", "Primary education, duration (years)", "Age dependency ratio, old (% of working-age population)", "Age dependency ratio, young (% of working-age population)", "Age dependency ratio, young (% of working-age population)", "GDP growth (annual %)", "GDP per capita (current US$)", "Compulsory education, duration (years)", "Foreign direct investment, net inflows (BoP, current US$)", "Individuals using the Internet (% of population)")
df_final <- subset(ds, ds$`Series Name` %in% lista_titulos)
df_final <- subset(df_final, select = -`Series Code`)

# Número de valores preenchidos (sem missings)
sum(df_final$`2021 [YR2021]` != '..')

# Transpose 
df_final <- spread(df_final, key = "Series Name", value = "2021 [YR2021]")

# Contagem do número de missings em cada coluna
missing_values_count <- colSums(df_final == "..")

result_df <- data.frame(
  Column = names(missing_values_count),
  Missing_Values_Count = as.numeric(missing_values_count)
)


# Teste numa só variável para verificar os valores da tabela
sum(df_final[["Access to electricity (% of population)"]] == "..")


# Contagem do número de missings em cada linha
missing_values_count_row <- rowSums(df_final == "..")
# Criar um novo df com row indices e missing values count
result_df_row <- data.frame(
  Country = df_final$`Country Name`,
  Missing_Values_Count = as.numeric(missing_values_count_row)
)


# Teste numa só observação para verificar os valores da tabela
sum(sapply(df_final, function(x) any(x == ".." & df_final$`Country Name` == "Afghanistan")))


# Colunas a considerar (max de 20% de missings do total de paises)
columns_to_consider <- subset(result_df, result_df$Missing_Values_Count < 53)


# Df filtrado com variáveis a utilizar
lista_titulos_updated <- c("Country Name", "Country Code", "Access to electricity (% of population)", "Age dependency ratio, old (% of working-age population)", "Age dependency ratio, young (% of working-age population)", "Compulsory education, duration (years)", "Contributing family workers, total (% of total employment) (modeled ILO estimate)", "Employers, total (% of total employment) (modeled ILO estimate)", "Employment in agriculture (% of total employment) (modeled ILO estimate)", "Employment in industry (% of total employment) (modeled ILO estimate)", "Employment in services (% of total employment) (modeled ILO estimate)", "Employment to population ratio, 15+, total (%) (modeled ILO estimate)", "Foreign direct investment, net inflows (BoP, current US$)", "GDP growth (annual %)", "GDP per capita (current US$)", "Individuals using the Internet (% of population)", "Inflation, consumer prices (annual %)", "Labor force participation rate for ages 15-24, total (%) (modeled ILO estimate)", "Population density (people per sq. km of land area)", "Population growth (annual %)", "Preprimary education, duration (years)", "Primary education, duration (years)", "Ratio of female to male labor force participation rate (%) (modeled ILO estimate)", "Secondary education, duration (years)", "Self-employed, total (% of total employment) (modeled ILO estimate)", "Unemployment, total (% of total labor force) (modeled ILO estimate)", "Vulnerable employment, total (% of total employment) (modeled ILO estimate)")
df_final <- df_final[, lista_titulos_updated]
#retirei "Wage and salaried workers, total (% of total employment) (modeled ILO estimate)" e "Personal remittances, paid (current US$)"

# Contagem do número de missings em cada linha depois do subset 
missing_values_count_row <- rowSums(df_final == "..")

result_df_row <- data.frame(
  Country = df_final$`Country Name`,
  Missing_Values_Count = as.numeric(missing_values_count_row)
)

# Países a considerar (max de 20% de missings do total de colunas)
countries_to_consider <- subset(result_df_row, result_df_row$Missing_Values_Count < 6)


# Excluir países com + de 20% missings
countries_include <- result_df_row$Country[result_df_row$Missing_Values_Count < 6]
df_final <- df_final[df_final$`Country Name` %in% countries_include, ]


#################################################################################################

# Ver estrutura do dataset
str(df_final)
# n = 231
# p = 27

# Descriptive statistics
summary(df_final)

# retirar caracteres especiais do nome das variáveis
colnames(df_final) <- gsub(" ", "_", colnames(df_final))
colnames(df_final) <- gsub("%", "percentage", colnames(df_final))
colnames(df_final) <- gsub(",", "", colnames(df_final))
colnames(df_final) <- gsub(")", "", colnames(df_final))
colnames(df_final) <- gsub("\\(", "", colnames(df_final))
colnames(df_final) <- gsub("-", "_", colnames(df_final))
colnames(df_final) <- gsub("\\$", "", colnames(df_final))
colnames(df_final) <- gsub("\\+", "more", colnames(df_final))


# Lista de países a serem removidos
paises_para_remover <- c("Africa Eastern and Southern", "Africa Western and Central", "Arab World", "Central African Republic", "Central Europe and the Baltics", "Early-demographic dividend", "East Asia & Pacific", "East Asia & Pacific (excluding high income)", "East Asia & Pacific (IDA & IBRD countries)", "Euro area", "Europe & Central Asia", "Europe & Central Asia (excluding high income)", "Europe & Central Asia (IDA & IBRD countries)", "European Union", "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)", "High income", "IBRD only", "IDA & IBRD total", "IDA blend", "IDA only", "IDA total", "Late-demographic dividend", "Latin America & Caribbean", "Latin America & Caribbean (excluding high income)", "Latin America & the Caribbean (IDA & IBRD countries)", "Least developed countries: UN classification", "Low & middle income", "Low income", "Lower middle income", "Middle East & North Africa", "Middle East & North Africa (excluding high income)", "Middle East & North Africa (IDA & IBRD countries)", "Middle income", "North America", "OECD members", "Other small states", "Pacific island small states", "Post-demographic dividend", "Pre-demographic dividend", "Small states", "South Africa", "South Asia", "South Asia (IDA & IBRD)", "Sub-Saharan Africa", "Sub-Saharan Africa (excluding high income)", "Sub-Saharan Africa (IDA & IBRD countries)", "Upper middle income", "World")

# Remover observações com Country_Name na lista
df_final <- df_final[!(df_final$Country_Name %in% paises_para_remover), ]

# Depois de excluir os países com muitos missings, contagem do número de missings em cada coluna
missing_values_count <- colSums(df_final == "..")
# Criar uma n
# Create a new data frame with column names and missing values count
result_df <- data.frame(
  Column = names(missing_values_count),
  Missing_Values_Count = as.numeric(missing_values_count)
)

# Substituir marcadores por NA
df_final[df_final == '..'] <- NA

# Ver os missings que existem já
sapply(df_final, function(x) sum(is.na(x)))

# alterar data types dos indicadores para numéricos
df_final <- df_final %>%
  mutate_at(vars(3:27), as.double)
# mudar data types
df_final$Compulsory_education_duration_years <- as.integer(df_final$Compulsory_education_duration_years)
df_final$Preprimary_education_duration_years <- as.integer(df_final$Preprimary_education_duration_years)
df_final$Primary_education_duration_years <- as.integer(df_final$Primary_education_duration_years)
df_final$Secondary_education_duration_years <- as.integer(df_final$Secondary_education_duration_years)

#################### TRATAMENTO DE OUTLIERS
#check outliers 
summary(df_final)

# 13 outliers (países pouco desenvolvidos de África)
boxplot(df_final$Access_to_electricity_percentage_of_population)
elect_outliers <- df_final[df_final$Access_to_electricity_percentage_of_population < 40,]
## Winsorizing
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers ex.: 13)
lower_percentile <- 13/180
upper_percentile <- 1-13/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Access_to_electricity_percentage_of_population, lower_percentile)
upper_limit <- quantile(df_final$Access_to_electricity_percentage_of_population, upper_percentile)
# Apply winsorizing
df_final$Access_to_electricity_percentage_of_population <- pmin(pmax(df_final$Access_to_electricity_percentage_of_population, lower_limit), upper_limit)
boxplot(df_final$Access_to_electricity_percentage_of_population)


# 1 outlier (Japan)
boxplot(df_final$Age_dependency_ratio_old_percentage_of_working_age_population)
## Winsorizing
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers ex.: 13)
lower_percentile <- 1/180
upper_percentile <- 1-1/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Age_dependency_ratio_old_percentage_of_working_age_population, lower_percentile)
upper_limit <- quantile(df_final$Age_dependency_ratio_old_percentage_of_working_age_population, upper_percentile)
# Apply winsorizing
df_final$Age_dependency_ratio_old_percentage_of_working_age_population <- pmin(pmax(df_final$Age_dependency_ratio_old_percentage_of_working_age_population, lower_limit), upper_limit)
boxplot(df_final$Age_dependency_ratio_old_percentage_of_working_age_population)


# Already ok
boxplot(df_final$Age_dependency_ratio_young_percentage_of_working_age_population)


# 1 outlier (New Caledonia) // 0 não faz sentido, a menos que não tenha 
boxplot(df_final$Compulsory_education_duration_years)
# justificação de 9 - https://www.scholaro.com/db/Countries/New-Caledonia/Education-System
df_final$Compulsory_education_duration_years[df_final$Compulsory_education_duration_years == 0] <- 9


# 11 outliers (Azerbeijão, Coreia e países da África como a Etiópia, etc.)
boxplot(df_final$Contributing_family_workers_total_percentage_of_total_employment_modeled_ILO_estimate)
fam_workers_outliers <- df_final[df_final$Contributing_family_workers_total_percentage_of_total_employment_modeled_ILO_estimate >= 30,]
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 13/180
upper_percentile <- 1-13/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Contributing_family_workers_total_percentage_of_total_employment_modeled_ILO_estimate, lower_percentile)
upper_limit <- quantile(df_final$Contributing_family_workers_total_percentage_of_total_employment_modeled_ILO_estimate, upper_percentile)
# Apply winsorizing
df_final$Contributing_family_workers_total_percentage_of_total_employment_modeled_ILO_estimate <- pmin(pmax(df_final$Contributing_family_workers_total_percentage_of_total_employment_modeled_ILO_estimate, lower_limit), upper_limit)
boxplot(df_final$Contributing_family_workers_total_percentage_of_total_employment_modeled_ILO_estimate)


# 6 outliers (países do médio oriente; américa central e áfrica)
boxplot(df_final$Employers_total_percentage_of_total_employment_modeled_ILO_estimate)
employers_outliers <- df_final[df_final$Employers_total_percentage_of_total_employment_modeled_ILO_estimate > 8,]
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 6/180
upper_percentile <- 1-6/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Employers_total_percentage_of_total_employment_modeled_ILO_estimate, lower_percentile)
upper_limit <- quantile(df_final$Employers_total_percentage_of_total_employment_modeled_ILO_estimate, upper_percentile)
# Apply winsorizing
df_final$Employers_total_percentage_of_total_employment_modeled_ILO_estimate <- pmin(pmax(df_final$Employers_total_percentage_of_total_employment_modeled_ILO_estimate, lower_limit), upper_limit)
boxplot(df_final$Employers_total_percentage_of_total_employment_modeled_ILO_estimate)


# 1 outlier (Burundi)
boxplot(df_final$Employment_in_agriculture_percentage_of_total_employment_modeled_ILO_estimate)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 1/180
upper_percentile <- 1-1/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Employment_in_agriculture_percentage_of_total_employment_modeled_ILO_estimate, lower_percentile)
upper_limit <- quantile(df_final$Employment_in_agriculture_percentage_of_total_employment_modeled_ILO_estimate, upper_percentile)
# Apply winsorizing
df_final$Employment_in_agriculture_percentage_of_total_employment_modeled_ILO_estimate <- pmin(pmax(df_final$Employment_in_agriculture_percentage_of_total_employment_modeled_ILO_estimate, lower_limit), upper_limit)
boxplot(df_final$Employment_in_agriculture_percentage_of_total_employment_modeled_ILO_estimate)


# 2 outliers (Qatar e Oman - médio oriente)
boxplot(df_final$Employment_in_industry_percentage_of_total_employment_modeled_ILO_estimate)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 2/180
upper_percentile <- 1-2/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Employment_in_industry_percentage_of_total_employment_modeled_ILO_estimate, lower_percentile)
upper_limit <- quantile(df_final$Employment_in_industry_percentage_of_total_employment_modeled_ILO_estimate, upper_percentile)
# Apply winsorizing
df_final$Employment_in_industry_percentage_of_total_employment_modeled_ILO_estimate <- pmin(pmax(df_final$Employment_in_industry_percentage_of_total_employment_modeled_ILO_estimate, lower_limit), upper_limit)
boxplot(df_final$Employment_in_industry_percentage_of_total_employment_modeled_ILO_estimate)


# Already ok
boxplot(df_final$Employment_in_services_percentage_of_total_employment_modeled_ILO_estimate)


# 5 outliers
boxplot(df_final$Employment_to_population_ratio_15more_total_percentage_modeled_ILO_estimate)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 5/180
upper_percentile <- 1-5/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Employment_to_population_ratio_15more_total_percentage_modeled_ILO_estimate, lower_percentile)
upper_limit <- quantile(df_final$Employment_to_population_ratio_15more_total_percentage_modeled_ILO_estimate, upper_percentile)
# Apply winsorizing
df_final$Employment_to_population_ratio_15more_total_percentage_modeled_ILO_estimate <- pmin(pmax(df_final$Employment_to_population_ratio_15more_total_percentage_modeled_ILO_estimate, lower_limit), upper_limit)
boxplot(df_final$Employment_to_population_ratio_15more_total_percentage_modeled_ILO_estimate)


# Demasiados outliers
boxplot(df_final$Foreign_direct_investment_net_inflows_BoP_current_US)


# 10 outliers
boxplot(df_final$GDP_growth_annual_percentage)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 10/180
upper_percentile <- 1-10/180
# Calculate winsorized values
lower_limit <- quantile(df_final$GDP_growth_annual_percentage, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$GDP_growth_annual_percentage, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$GDP_growth_annual_percentage <- pmin(pmax(df_final$GDP_growth_annual_percentage, lower_limit), upper_limit)
boxplot(df_final$GDP_growth_annual_percentage)


# muitos outliers
boxplot(df_final$GDP_per_capita_current_US)
gdp_p_capit <- df_final[df_final$GDP_per_capita_current_US >= 40000,]
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 24/180
upper_percentile <- 1-24/180
# Calculate winsorized values
lower_limit <- quantile(df_final$GDP_per_capita_current_US, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$GDP_per_capita_current_US, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$GDP_per_capita_current_US <- pmin(pmax(df_final$GDP_per_capita_current_US, lower_limit), upper_limit)
boxplot(df_final$GDP_per_capita_current_US)


# Already ok
boxplot(df_final$Individuals_using_the_Internet_percentage_of_population)


# 16 outliers (valores de inflação acima dos 10%)
boxplot(df_final$Inflation_consumer_prices_annual_percentage)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 16/180
upper_percentile <- 1-16/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Inflation_consumer_prices_annual_percentage, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$Inflation_consumer_prices_annual_percentage, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$Inflation_consumer_prices_annual_percentage <- pmin(pmax(df_final$Inflation_consumer_prices_annual_percentage, lower_limit), upper_limit)
boxplot(df_final$Inflation_consumer_prices_annual_percentage)


# Already ok
boxplot(df_final$Labor_force_participation_rate_for_ages_15_24_total_percentage_modeled_ILO_estimate)


# muitos outliers
# boxplot(df_final$Personal_remittances_paid_current_US)


# 16 outliers (territórios da China // max ref valores acima de 450)
boxplot(df_final$Population_density_people_per_sq._km_of_land_area)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 16/180
upper_percentile <- 1-16/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Population_density_people_per_sq._km_of_land_area, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$Population_density_people_per_sq._km_of_land_area, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$Population_density_people_per_sq._km_of_land_area <- pmin(pmax(df_final$Population_density_people_per_sq._km_of_land_area, lower_limit), upper_limit)
boxplot(df_final$Population_density_people_per_sq._km_of_land_area)


# 4 outliers (croácia, Singapura, Qatar e Kuwait / valores abaixo de -2)
boxplot(df_final$Population_growth_annual_percentage)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 4/180
upper_percentile <- 1-4/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Population_growth_annual_percentage, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$Population_growth_annual_percentage, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$Population_growth_annual_percentage <- pmin(pmax(df_final$Population_growth_annual_percentage, lower_limit), upper_limit)
boxplot(df_final$Population_growth_annual_percentage)


# Already ok
boxplot(df_final$Preprimary_education_duration_years)


# 1 outlier (irlanda)
boxplot(df_final$Primary_education_duration_years)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 1/180
upper_percentile <- 1-1/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Primary_education_duration_years, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$Primary_education_duration_years, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$Primary_education_duration_years <- pmin(pmax(df_final$Primary_education_duration_years, lower_limit), upper_limit)
boxplot(df_final$Primary_education_duration_years)


# 7 outliers (abaixo de 29)
boxplot(df_final$Ratio_of_female_to_male_labor_force_participation_rate_percentage_modeled_ILO_estimate)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 7/180
upper_percentile <- 1-7/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Ratio_of_female_to_male_labor_force_participation_rate_percentage_modeled_ILO_estimate, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$Ratio_of_female_to_male_labor_force_participation_rate_percentage_modeled_ILO_estimate, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$Ratio_of_female_to_male_labor_force_participation_rate_percentage_modeled_ILO_estimate <- pmin(pmax(df_final$Ratio_of_female_to_male_labor_force_participation_rate_percentage_modeled_ILO_estimate, lower_limit), upper_limit)
boxplot(df_final$Ratio_of_female_to_male_labor_force_participation_rate_percentage_modeled_ILO_estimate)


# 2 outliers
boxplot(df_final$Secondary_education_duration_years)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 2/180
upper_percentile <- 1-2/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Secondary_education_duration_years, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$Secondary_education_duration_years, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$Secondary_education_duration_years <- pmin(pmax(df_final$Secondary_education_duration_years, lower_limit), upper_limit)
boxplot(df_final$Secondary_education_duration_years)


# Already ok
boxplot(df_final$Self_employed_total_percentage_of_total_employment_modeled_ILO_estimate)


# 10 outliers (acima de 20)
boxplot(df_final$Unemployment_total_percentage_of_total_labor_force_modeled_ILO_estimate)
# Set the percentage cutoff for winsorizing (cutoff based on the number of outliers)
lower_percentile <- 10/180
upper_percentile <- 1-10/180
# Calculate winsorized values
lower_limit <- quantile(df_final$Unemployment_total_percentage_of_total_labor_force_modeled_ILO_estimate, lower_percentile, na.rm = TRUE)
upper_limit <- quantile(df_final$Unemployment_total_percentage_of_total_labor_force_modeled_ILO_estimate, upper_percentile, na.rm = TRUE)
# Apply winsorizing
df_final$Unemployment_total_percentage_of_total_labor_force_modeled_ILO_estimate <- pmin(pmax(df_final$Unemployment_total_percentage_of_total_labor_force_modeled_ILO_estimate, lower_limit), upper_limit)
boxplot(df_final$Unemployment_total_percentage_of_total_labor_force_modeled_ILO_estimate)


# Already ok
boxplot(df_final$Vulnerable_employment_total_percentage_of_total_employment_modeled_ILO_estimate)


###################### TRATAMENTO DE MISSINGS E STANDARDIZATION 

# embora já tenhamos uma ideia das variáveis com mais missings (ver result_df), com um pattern é possível ter uma ideia mais clara do padrão de missing values
md.pattern(df_final)

dados_padronizados <- scale(df_final[,3:27])
imputacao <- mice(dados_padronizados, method = "pmm", m = 5, maxit=100, seed=500)
dados_imputados <- complete(imputacao)


# verificar nulos apos imputaçao
sapply(dados_imputados, function(x) sum(is.na(x)))

# correlation matrix
corr <- round(cor(dados_imputados[,]),3)
#plot.new()
#png("correlation.png", width = 1800, height = 1800,res = 60)
#par(oma=c(0,3,3,0))
#corrplot.mixed(corr, 
#               order = "hclust", #order of variables
#               tl.pos = "lt", #text left + top
#               upper = "ellipse",
#               tl.cex = 0.7)
#dev.off()

# determinante
det(corr)

# Ver adequabilidade - KMO bom 
cortest.bartlett(corr)
KMO(corr)


# PC extraction
pc25 <- principal(dados_imputados, nfactors = 25, rotate = "none", scores = TRUE)

# Kaiser criterion
round(pc25$values,3)
# 6 PCs

# Screeplot
plot(pc25$values, type = "b", main = "Scree plot for WDI dataset", 
     xlab = "Number of PC", ylab = "Eigenvalue")
# 3 PCs

#Tests
pc5 <- principal(dados_imputados, nfactors = 5, rotate = "none")
pc6 <- principal(dados_imputados, nfactors = 6, rotate = "none")

# Loadings and explained variance
pc25$loadings
# We need at least 4 PCs

# Loadings for 4 PCs
pc4 <- principal(dados_imputados, nfactors = 4, rotate = "none")

# Rotated 4 PCs
pc4r <- principal(dados_imputados, nfactors = 4, rotate = "varimax")
pc4r$loadings

# Initial Interpretation
# RC1 - Força de trabalho
# RC2 - Condições laborais precárias
# RC3 - Formação e qualificação profissional
# RC4 - Impacto da natalidade no mercado

# Communalities
round(pc4$communality,2)
round(pc5$communality,2)
round(pc6$communality,2)

# Calculo dos "scores"
pc4sc <- principal(dados_imputados, nfactors = 4, rotate = "none", scores = TRUE)
round(pc4sc$scores,3)

mean(pc4sc$scores[,1])
sd(pc4sc$scores[,1])
# valores 0 e 1 são normais porque temos tudo estandardizado

# Rotated 6 PCs
pc6r <- principal(dados_imputados, nfactors = 6, rotate = "varimax")
pc6r$loadings

# Interpretation
# RC1 - Acesso a infraestruturas e dinâmicas de trabalho
# RC2 - Precariedade nas condições laborais
# RC3 - Formação e qualificação profissional
# RC4 - Desenvolvimento económico
# RC5 - Evolução demográfica
# RC6 - Investimento estrangeiro

# Calculo dos "scores"
pc6sc <- principal(dados_imputados, nfactors = 6, rotate = "none", scores = TRUE)
round(pc6sc$scores,3)

mean(pc6sc$scores[,1])
sd(pc6sc$scores[,1])
# valores 0 e 1 são normais porque temos tudo estandardizado


# 6 Pcs based on the interpretation
# Adicionar os scores ao dataset
df_final$infrastructures <- pc6sc$scores[,1]
df_final$precarity <- pc6sc$scores[,2]
df_final$education <- pc6sc$scores[,3]
df_final$economic_development <- pc6sc$scores[,4]
df_final$demographics <- pc6sc$scores[,5]
df_final$foreign_investment <- pc6sc$scores[,6]

head(df_final)


# Depict the scatterplot of PC2 vs PC4
plot(df_final$precarity, df_final$economic_development, pch = 19, xlim = c(-5,5),
     ylim = c(-5,5), xlab = "Precarity", ylab = "Economic development", 
     main = "Scores: Precarity vs Economic development")

# Compute correlations: Precarity vs Economic_development
cor(df_final$precarity,df_final$economic_development)


## Clustering
#Hierarchical cluster
pc_dist <- dist(df_final[,28:33])
hclust  <- hclust(pc_dist, method='ward.D2')
plot(hclust, hang=-1, labels=FALSE)

# Cut the dendrogram
groups.k6 <- cutree(hclust, k=6) # cut tree into 6 clusters
rect.hclust(hclust, k=6, border="red") 

groups.k5 <- cutree(hclust, k=5) # cut tree into 5 clusters
#rect.hclust(hclust, k=5, border="red") 


#Silhouette
plot(silhouette(groups.k6, pc_dist))
plot(silhouette(groups.k5, pc_dist))
# Based on the dendrogram select 6 clusters
# Silhouette shows no substantial structure


#Hierarchical cluster method complete
pc_dist <- dist(df_final[,28:33]) # compute distance (no need of scaling)
hclust  <- hclust(pc_dist,method='complete')
plot(hclust, hang=-1, labels=FALSE)

# Cut the dendrogram
groups.k3_c <- cutree(hclust, k=3) # cut tree into 3 clusters
rect.hclust(hclust, k=3, border="red")

#Silhouette
plot(silhouette(groups.k3_c, pc_dist))

#no substantial structure


# K-Means: number of clusters
wssplot <- function(xx, nc=15, seed=1234){
  wss <- (nrow(xx)-1)*sum(apply(xx,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(xx, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(df_final[,28:33], nc=15)

#K-means cluster com K=6
kmeans.k6 <- kmeans(df_final[,28:33], 6, nstart=100) 
df_final = df_final %>% mutate(cluster = kmeans.k6$cluster)


#crosstab
table(groups.k6, df_final$cluster)


#Barplot of average score in each principal component within each cluster
barplot(colMeans(subset(df_final,cluster==1)[,28:33]),main= "Cluster 1 - Average score in each principal component")
barplot(colMeans(subset(df_final,cluster==2)[,28:33]),main= "Cluster 2 - Average score in each principal component")
barplot(colMeans(subset(df_final,cluster==3)[,28:33]),main= "Cluster 3 - Average score in each principal component")
barplot(colMeans(subset(df_final,cluster==4)[,28:33]),main= "Cluster 4 - Average score in each principal component")
barplot(colMeans(subset(df_final,cluster==5)[,28:33]),main= "Cluster 5 - Average score in each principal component")
barplot(colMeans(subset(df_final,cluster==6)[,28:33]),main= "Cluster 6 - Average score in each principal component")


#### CARACTERIZAÇÃO DOS CLUSTERS (ACRESCENTAR VARIAVEIS DE PROFILING)


## PAM clustering
std_data <- scale(df_final[,28:33])
pam.k6 <- pam(std_data, 6)

#pam.k6
table(groups.k6,pam.k6$clustering)

#PCA and Clustering
clusplot(pam.k6, labels = 6, col.p = pam.k6$clustering)


# Probabilistic clustering
# Dataset
data <- df_final[,28:33]

# Model selection
BIC <- mclustBIC(data)
plot(BIC)

### GMM
set.seed(1233)

# Apply GMM with 6 components
results.G6 <- Mclust(data, G = 6)
summary(results.G6, parameters = TRUE)

# Some results
results.G6$modelName          # Optimal selected model
results.G6$G                  # Optimal number of cluster
head(results.G6$z, 5)         # Probability to belong to a given cluster
head(results.G6$classification, 5) # Cluster assignment of each observation

plot(results.G6, what = "classification")
plot(results.G6, what = "uncertainty")
