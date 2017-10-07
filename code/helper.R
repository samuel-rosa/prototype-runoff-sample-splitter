# Função para organizar os dados para tabela 1
my_reshape <- function (x, y) {
  x <- t(x) %>% as.data.frame()
  x <- cbind(rownames(x), x)[-1, ]
  colnames(x) <- colnames(y)
  x
}
# Função para computar o teste t para a média de uma amostra
teste_t1 <- 
  function (d) {
    group_by(d, entrada, saida) %>% 
      summarise('*t*' = t.test(erro, mu = 0, alternative = "two.sided")$statistic,
                '*P*' = t.test(erro, mu = 0, alternative = "two.sided")$p.value)
  }
# Função para computar o teste t para a média de duas amostras pareadas
teste_t2 <-
  function (x = d, y = 300) {
    tmp <- 
      filter(x, entrada == y / 2) %>% 
      tidyr::spread(entrada, erro) %>% 
      select(repeticao, saida, as.character(y/2)) %>% 
      tidyr::spread(saida, as.character(y/2))
    names(tmp) <- gsub(" ", "", names(tmp))
    summarise(tmp,
              '*t*' = t.test(A, B, mu = 0, alternative = "two.sided", paired = TRUE)$statistic,
              '*P*' = t.test(A, B, mu = 0, alternative = "two.sided", paired = TRUE)$p.value)
  }
