library(igraph)

# Seed para reproducibilidade dos resultados
set.seed(123)


rede <- read_graph("ligacoes_t2.txt", format = "edgelist", directed = F)
# Definir o layout fixo das redes
layout <- layout_with_fr(rede)

plot(
  rede,
  vertex.size = 12,           # Tamanho dos nodos
  edge.color = "gray",        # Cor das arestas
  vertex.color = "lightblue", # Cor dos nodos
  layout = layout
)

###############################
# MÉTODO 1: REMOÇÃO DE PONTES #
###############################

comunidades_remocao_pontes <- cluster_edge_betweenness(rede)
plot(comunidades_remocao_pontes, rede, vertex.size = 7, vertex.label.cex = .6, layout = layout)

# Número de comunidades
length(comunidades_remocao_pontes)

# Dimensão das comunidades
sizes(comunidades_remocao_pontes)

print(paste("Tamanho mínimo da comunidade:", min(sizes(comunidades_remocao_pontes))))
print(paste("Tamanho máximo da comunidade:", max(sizes(comunidades_remocao_pontes))))

# Frequência de tamanhos
table(sizes(comunidades_remocao_pontes))

# Número de ligações internas e densidade interna + ligações externas
for (i in 1:length(comunidades_remocao_pontes)) {
  # Subgrafo da comunidade
  comunidade_nodes <- which(membership(comunidades_remocao_pontes) == i)
  subgrafo <- induced_subgraph(rede, comunidade_nodes)
  
  # Número de arestas internas (dentro da comunidade)
  num_arestas_internas <- ecount(subgrafo)
  densidade_interna <- edge_density(subgrafo)
  
  # Número de ligações externas (arestas para fora da comunidade)
  arestas_externas <- E(rede)[which(
    (ends(rede, E(rede))[, 1] %in% comunidade_nodes & !(ends(rede, E(rede))[, 2] %in% comunidade_nodes)) | 
      (ends(rede, E(rede))[, 2] %in% comunidade_nodes & !(ends(rede, E(rede))[, 1] %in% comunidade_nodes))
  )]
  
  num_ligacoes_externas <- length(arestas_externas)
  
  # Exibição dos resultados
  print(paste("Comunidade", i, ": Ligações internas =", num_arestas_internas, 
              ", Densidade interna =", densidade_interna, 
              ", Ligações externas =", num_ligacoes_externas))
  cat("Nodos :", paste(comunidade_nodes, collapse = " "), "\n")
}

# Modularidade
modularity(comunidades_remocao_pontes)


######################################################
# MÉTODO 2: OTIMIZAÇÃO DE MODULARIDADE (FAST GREEDY) #
######################################################

set.seed(2024)
comunidades_fast_greedy <- cluster_fast_greedy(rede)
plot(comunidades_fast_greedy, rede, vertex.size = 7, vertex.label.cex = .6, layout = layout)

# Número de comunidades
length(comunidades_fast_greedy)

# Dimensão das comunidades
sizes(comunidades_fast_greedy)

print(paste("Tamanho mínimo da comunidade:", min(sizes(comunidades_fast_greedy))))
print(paste("Tamanho máximo da comunidade:", max(sizes(comunidades_fast_greedy))))

# Frequência de tamanhos
table(sizes(comunidades_fast_greedy))

# Número de ligações internas e densidade interna + ligações externas
for (i in 1:length(comunidades_fast_greedy)) {
  # Subgrafo da comunidade
  comunidade_nodes <- which(membership(comunidades_fast_greedy) == i)
  subgrafo <- induced_subgraph(rede, comunidade_nodes)
  
  # Número de arestas internas (dentro da comunidade)
  num_arestas_internas <- ecount(subgrafo)
  densidade_interna <- edge_density(subgrafo)
  
  # Número de ligações externas (arestas para fora da comunidade)
  arestas_externas <- E(rede)[which(
    (ends(rede, E(rede))[, 1] %in% comunidade_nodes & !(ends(rede, E(rede))[, 2] %in% comunidade_nodes)) | 
      (ends(rede, E(rede))[, 2] %in% comunidade_nodes & !(ends(rede, E(rede))[, 1] %in% comunidade_nodes))
  )]
  
  num_ligacoes_externas <- length(arestas_externas)
  
  # Exibição dos resultados
  print(paste("Comunidade", i, ": Ligações internas =", num_arestas_internas, 
              ", Densidade interna =", densidade_interna, 
              ", Ligações externas =", num_ligacoes_externas))
  cat("Nodos :", paste(comunidade_nodes, collapse = " "), "\n")
}

# Modularidade
modularity(comunidades_fast_greedy)


###################################
# MÉTODO 3: OTIMIZAÇÃO DE LOUVAIN #
###################################

set.seed(123)
comunidades_louvain <- cluster_louvain(rede)
plot(comunidades_louvain, rede, vertex.size = 7, vertex.label.cex = .6, layout = layout)

# Número de comunidades
length(comunidades_louvain)

# Dimensão das comunidades
sizes(comunidades_louvain)

print(paste("Tamanho mínimo da comunidade:", min(sizes(comunidades_louvain))))
print(paste("Tamanho máximo da comunidade:", max(sizes(comunidades_louvain))))

# Frequência de tamanhos
table(sizes(comunidades_louvain))

# Número de ligações internas e densidade interna + ligações externas
for (i in 1:length(comunidades_louvain)) {
  # Subgrafo da comunidade
  comunidade_nodes <- which(membership(comunidades_louvain) == i)
  subgrafo <- induced_subgraph(rede, comunidade_nodes)
  
  # Número de arestas internas (dentro da comunidade)
  num_arestas_internas <- ecount(subgrafo)
  densidade_interna <- edge_density(subgrafo)
  
  # Número de ligações externas (arestas para fora da comunidade)
  arestas_externas <- E(rede)[which(
    (ends(rede, E(rede))[, 1] %in% comunidade_nodes & !(ends(rede, E(rede))[, 2] %in% comunidade_nodes)) | 
      (ends(rede, E(rede))[, 2] %in% comunidade_nodes & !(ends(rede, E(rede))[, 1] %in% comunidade_nodes))
  )]
  
  num_ligacoes_externas <- length(arestas_externas)
  
  # Exibição dos resultados
  print(paste("Comunidade", i, ": Ligações internas =", num_arestas_internas, 
              ", Densidade interna =", densidade_interna, 
              ", Ligações externas =", num_ligacoes_externas))
  cat("Nodos :", paste(comunidade_nodes, collapse = " "), "\n")
}

# Modularidade
modularity(comunidades_louvain)


#####################################
# MÈTODO 4: PROPAGAÇÃO DE ETIQUETAS #
#####################################

set.seed(2024)
comunidades_label_propagation <- cluster_label_prop(rede)
plot(comunidades_label_propagation, rede, vertex.size = 7, vertex.label.cex = .6, layout = layout)

# Número de comunidades
length(comunidades_label_propagation)

# Dimensão das comunidades
sizes(comunidades_label_propagation)

print(paste("Tamanho mínimo da comunidade:", min(sizes(comunidades_label_propagation))))
print(paste("Tamanho máximo da comunidade:", max(sizes(comunidades_label_propagation))))

# Frequência de tamanhos
table(sizes(comunidades_label_propagation))

# Número de ligações internas e densidade interna + ligações externas
for (i in 1:length(comunidades_label_propagation)) {
  # Subgrafo da comunidade
  comunidade_nodes <- which(membership(comunidades_label_propagation) == i)
  subgrafo <- induced_subgraph(rede, comunidade_nodes)
  
  # Número de arestas internas (dentro da comunidade)
  num_arestas_internas <- ecount(subgrafo)
  densidade_interna <- edge_density(subgrafo)
  
  # Número de ligações externas (arestas para fora da comunidade)
  arestas_externas <- E(rede)[which(
    (ends(rede, E(rede))[, 1] %in% comunidade_nodes & !(ends(rede, E(rede))[, 2] %in% comunidade_nodes)) | 
      (ends(rede, E(rede))[, 2] %in% comunidade_nodes & !(ends(rede, E(rede))[, 1] %in% comunidade_nodes))
  )]
  
  num_ligacoes_externas <- length(arestas_externas)
  
  # Exibição dos resultados
  print(paste("Comunidade", i, ": Ligações internas =", num_arestas_internas, 
              ", Densidade interna =", densidade_interna, 
              ", Ligações externas =", num_ligacoes_externas))
  cat("Nodos :", paste(comunidade_nodes, collapse = " "), "\n")
}

# Modularidade
modularity(comunidades_label_propagation)
