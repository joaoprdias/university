library(igraph)

rede <- read_graph("ligacoes_t1.txt", format = "edgelist", directed = F)

plot(
  rede,
  vertex.size = 12,           # Tamanho dos nodos
  edge.color = "gray",       # Cor das arestas
  vertex.color = "lightblue",# Cor dos nodos
)


############### Q1
# 1. Dimensão (número de nodos) e número de ligações
dimensao <- vcount(rede)
num_ligacoes <- ecount(rede)
cat("Dimensão da rede (número de nodos):", dimensao, "\n")
cat("Número de ligações:", num_ligacoes, "\n")

# 2. Densidade da rede
densidade <- edge_density(rede)
cat("Densidade da rede:", densidade, "\n")

# Classificação da rede
if (densidade > 0.5) {
  cat("Classificação: Rede Densa\n")
} else {
  cat("Classificação: Rede Esparsa\n")
}

# 3. Grau médio
graus <- degree(rede, mode = "total")
grau_medio <- mean(graus)
cat("Grau médio:", grau_medio, "\n")

# 4. Distribuição do grau
cat("Resumo estatístico do grau:\n")
summary(graus)
quartis <- quantile(graus)
cat("Quartis do grau:\n")
print(quartis)

# Plot da distribuição de graus
hist(graus, breaks = 0:vcount(rede), col = "lightblue", border = "black",
     main = "Distribuição de Grau", xlab = "Grau", ylab = "Frequência")

# 5. Parâmetro de heterogeneidade
heterogeneidade <- mean(graus^2) / (grau_medio^2)
cat("Parâmetro de heterogeneidade:", heterogeneidade, "\n")

# Identificar a existência de hubs
if (heterogeneidade > 1.5) {
  cat("A rede possui hubs (nodos altamente conectados).\n")
} else {
  cat("A rede não apresenta hubs significativos.\n")
}



############### Q2
# Calcular o degree assortativity da rede
assortatividade <- assortativity_degree(rede)
cat("Degree Assortativity:", assortatividade, "\n")



############### Q3
# Calcular a média dos comprimentos dos caminhos mais curtos (distância média)
distancia_media <- mean_distance(rede)
cat("Média dos comprimentos dos caminhos mais curtos (distância média):", distancia_media, "\n")

# Calcular o logaritmo de base 10 do número de nodos
log_num_nodos <- log10(dimensao)
cat("Logaritmo de base 10 do número de nodos:", log_num_nodos, "\n")



############### Q4
# Determinar os coeficientes de clustering dos nodos e da rede
# Coeficiente de clustering dos nodos
coef_clustering_nodos <- transitivity(rede, type = "local")
cat("Coeficientes de clustering dos nodos:\n", coef_clustering_nodos)

# Média dos coeficientes de clustering dos nodos
media_clustering_nodos <- mean(coef_clustering_nodos, na.rm = TRUE)
cat("\nMédia dos coeficientes de clustering dos nodos:", media_clustering_nodos, "\n")

# Coeficiente de clustering da rede (global)
coef_clustering_rede <- transitivity(rede, type = "global")
cat("Coeficiente de clustering da rede (global):", coef_clustering_rede, "\n")

# Número total de triângulos na rede
num_triangulos <- count_triangles(rede)
cat("\nNúmero total de triângulos na rede:", sum(num_triangulos) / 3, "\n")  # Dividimos por 3 porque cada triângulo é contado três vezes.


############### Q5
# Calcular os índices de coreness para cada nodo
coreness_values <- coreness(rede)

# Determinar o número total de conchas (cores)
num_conchas <- max(coreness_values)
cat("Número total de conchas (cores):", num_conchas, "\n")

# Determinar a dimensão de cada concha
for (i in 1:num_conchas) {
  dimensao_concha <- sum(coreness_values == i)
  cat(paste("Número de nodos na concha", i, ":", dimensao_concha, "\n"))
}

#Definir cores únicas para cada concha (k-core)
core_colors <- rainbow(length(unique(coreness_values)))

#Mapear cada valor de coreness para a cor correspondente
color_mapping <- setNames(core_colors, unique(coreness_values))

#Gráfico circular da percentagem dos nodos por concha
percentages <- table(coreness_values) / length(coreness_values) * 100
pie(percentages, 
    main = "Distribuição Percentual de Nodos por Concha", 
    col = color_mapping[as.character(names(percentages))],
    labels = paste0("Concha ", names(percentages), ": ", round(percentages, 1), "%"), 
    cex = 0.7)

#Visualização do grafo com k-core onde cada concha tem uma cor diferente
V(rede)$color <- color_mapping[as.character(coreness_values)]
plot(rede, 
     vertex.size = 12, 
     vertex.color = V(rede)$color, 
     main = "Visualização da Rede com K-Core Decomposition", 
     vertex.label = NA)