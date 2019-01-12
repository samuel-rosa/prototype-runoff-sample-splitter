# Função para calcular a razão das dimensões em x e y de um mapa
bboxRatio <-
  function (obj) {
    dx <- sf::st_bbox(obj)[c(1, 3)] %>% diff()
    dy <- sf::st_bbox(obj)[c(2, 4)] %>% diff()
    res <- dx / dy
    return (unname(res))
  }
# Função para organizar os dados para tabela 1
my_reshape <- function (x, y) {
  x <- t(x) %>% as.data.frame()
  x <- cbind(rownames(x), x)[-1, ]
  colnames(x) <- colnames(y)
  x
}
# Função para computar o teste t para a média de uma amostra
teste_t1 <- 
  function (d, alternative = "two.sided", fracionador = TRUE) {
    if (fracionador) {
      group_by(d, entrada, saida) %>% 
        summarise('*t*' = t.test(erro, mu = 0, alternative = alternative)$statistic %>% round(3),
                  '*P*' = t.test(erro, mu = 0, alternative = alternative)$p.value %>% round(4))
    } else {
      group_by(d, entrada) %>% 
        summarise('*t*~1~' = t.test(erro, mu = 0, alternative = "two.sided")$statistic,
                  '*P*~1~' = t.test(erro, mu = 0, alternative = "two.sided")$p.value) 
    }
  }
# Função para computar o teste t para a média de duas amostras pareadas
.teste_t2 <-
  function (x = d, y = 2) {
    tmp <- 
      filter(x, entrada == y) %>% 
      tidyr::spread(entrada, erro) %>% 
      select(repeticao, saida, as.character(y)) %>% 
      tidyr::spread(saida, as.character(y))
    names(tmp) <- gsub(" ", "", names(tmp))
    summarise(tmp,
              '*t*~2~' = t.test(A, B, mu = 0, alternative = "two.sided", paired = TRUE)$statistic %>% round(3),
              '*P*~2~' = t.test(A, B, mu = 0, alternative = "two.sided", paired = TRUE)$p.value %>% round(4))
  }
teste_t2 <-
  function (x = d) {
    i <- x %>% select(entrada) %>% unique()
    t2 <- sapply(i[[1]], function (i) .teste_t2(x, i))
    t2 <- t2[, rep(1:nrow(i), each = 2)] %>% 
      apply(2, as.numeric) %>% 
      as.data.frame()
    cbind(r = c('*t*~2~', '*P*~2~'), t2)
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
