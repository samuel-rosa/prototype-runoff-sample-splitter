# Função para organizar os dados para tabela 1
my_reshape <- function (x, y) {
  x <- t(x) %>% as.data.frame()
  x <- cbind(rownames(x), x)[-1, ]
  colnames(x) <- colnames(y)
  x
}
# Função para computar o teste t para a média de uma amostra
teste_t1 <- 
  function (d, alternative = "two.sided") {
    group_by(d, entrada, saida) %>% 
      summarise('*t*' = t.test(erro, mu = 0, alternative = alternative)$statistic %>% round(3),
                '*P*' = t.test(erro, mu = 0, alternative = alternative)$p.value %>% round(4))
  }
# Função para computar o teste t para a média de duas amostras pareadas
# teste_t2 <-
#   function (x = d, y = 300) {
#     tmp <- 
#       filter(x, entrada == y / 2) %>% 
#       tidyr::spread(entrada, erro) %>% 
#       select(repeticao, saida, as.character(y/2)) %>% 
#       tidyr::spread(saida, as.character(y/2))
#     names(tmp) <- gsub(" ", "", names(tmp))
#     summarise(tmp,
#               '*t*' = t.test(A, B, mu = 0, alternative = "two.sided", paired = TRUE)$statistic,
#               '*P*' = t.test(A, B, mu = 0, alternative = "two.sided", paired = TRUE)$p.value)
#   }
teste_t2 <-
  function (x = d, y = 2) {
    tmp <- 
      filter(x, entrada == y) %>% 
      tidyr::spread(entrada, erro) %>% 
      select(repeticao, saida, as.character(y)) %>% 
      tidyr::spread(saida, as.character(y))
    names(tmp) <- gsub(" ", "", names(tmp))
    summarise(tmp,
              '*t*' = t.test(A, B, mu = 0, alternative = "two.sided", paired = TRUE)$statistic %>% round(3),
              '*P*' = t.test(A, B, mu = 0, alternative = "two.sided", paired = TRUE)$p.value %>% round(4))
  }
# Função auxiliar para preparar dados para análise
preparar_dados <- 
  function (x, unidade = 'g/L', fracionador = TRUE) {
    if (fracionador) {
      x %>% 
        mutate(entrada = stringr::str_split_fixed(amostra_codigo, ";", Inf)[, 1]) %>%
        mutate(entrada = gsub("Entrada: ", "", entrada)) %>% 
        mutate(entrada = gsub(unidade, "", entrada)) %>% 
        mutate(saida = stringr::str_split_fixed(amostra_codigo, ";", Inf)[, 2]) %>%
        mutate(saida = gsub("Saída: ", "", saida)) %>% 
        mutate(repeticao = stringr::str_split_fixed(amostra_codigo, ";", Inf)[, 3]) %>%
        mutate(repeticao = gsub("Repetição: ", "", repeticao)) %>%
        select(-amostra_codigo) %>% 
        mutate(entrada = as.numeric(entrada))  
    } else {
      x %>%
        mutate(entrada = stringr::str_split_fixed(amostra_codigo, ";", Inf)[, 1]) %>%
        mutate(entrada = gsub("Entrada: ", "", entrada)) %>% 
        mutate(entrada = gsub(unidade, "", entrada)) %>%
        mutate(repeticao = stringr::str_split_fixed(amostra_codigo, ";", Inf)[, 2]) %>%
        mutate(repeticao = gsub("Repetição: ", "", repeticao)) %>%
        select(-amostra_codigo) %>% 
        mutate(entrada = as.numeric(entrada)) 
    }
  }
