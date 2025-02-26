


processar_aquisicao <- function(tb_aux, tabela_estratos, base_pessoas, padrao_filtro,agregar_por_estrato) {
  
  # Filtra os estratos conforme o padrão informado
  estratos_filtrados <- unique(tabela_estratos$descricao_estrato[grep(padrao_filtro, tabela_estratos$descricao_estrato)])
  
  # Filtra os dados corretamente
  tab_aquisicao_aux <- tb_aux %>%
    right_join(tabela_estratos %>% filter(descricao_estrato %in% estratos_filtrados), by = c("UF", "ESTRATO_POF"))
  
  tab_pessoas_aux <- base_pessoas %>%
    right_join(tabela_estratos %>% filter(descricao_estrato %in% estratos_filtrados), by = c("UF", "ESTRATO_POF"))
  
  # Função auxiliar para calcular soma dos níveis
  calcular_soma_nivel <- function(df, coluna, indicador,agregar_por_estrato) {
    
    if (agregar_por_estrato){
      df %>%
        group_by(descricao_estrato, !!sym(coluna)) %>%
        summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE), .groups = "drop") %>%
        rename(descricao = !!sym(coluna)) %>%
        mutate(descricao = as.character(descricao),
               indicador_nivel = indicador)
    } else{   
      df %>%
        group_by(!!sym(coluna)) %>%
        summarise(qtidade_total = sum(qtidade_anual, na.rm = TRUE), .groups = "drop") %>%
        rename(descricao = !!sym(coluna)) %>%
        mutate(descricao = as.character(descricao),
               indicador_nivel = indicador) } 
    
  }
  
  # Aplicando a função para cada nível
  soma_nivel_1 <- calcular_soma_nivel(tab_aquisicao_aux, "class_final", "classes_nova",agregar_por_estrato)
  soma_nivel_2 <- calcular_soma_nivel(tab_aquisicao_aux, "class_analisegeral_final", "alimentos_decreto",agregar_por_estrato)
  soma_nivel_3 <- calcular_soma_nivel(tab_aquisicao_aux, "item_regional", "alimentos_regionais",agregar_por_estrato)
  soma_nivel_4 <- calcular_soma_nivel(tab_aquisicao_aux, "class_analisegeral_final_bebidas", "alimentos_decreto_bebidas",agregar_por_estrato)
  
  # Unindo todas as tabelas
  soma_niveis <- bind_rows(soma_nivel_1, soma_nivel_2, soma_nivel_3, soma_nivel_4)
  
  # Soma de indivíduos
  if (agregar_por_estrato){
    soma_individuo <- tab_pessoas_aux %>% 
      group_by(descricao_estrato)  %>% 
      summarise(soma_individuo = sum(PESO_FINAL, na.rm = TRUE))
    
    # Mesclando os resultados
    tab_aquisicao <- merge(soma_niveis, soma_individuo, by = "descricao_estrato", all.x = TRUE)
  } else {
    soma_individuo <- tab_pessoas_aux %>% 
      summarise(soma_individuo = sum(PESO_FINAL, na.rm = TRUE))
    
    # Mesclando os resultados
    tab_aquisicao <- merge(soma_niveis, soma_individuo, all.x = TRUE)
  }
  
  # Criando variável per capita
  tab_aquisicao <- tab_aquisicao %>%
    mutate(qtd_anual_percapita = round(qtidade_total / soma_individuo, 2))
  
  return(tab_aquisicao)
}
