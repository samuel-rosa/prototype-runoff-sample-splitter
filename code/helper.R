# Função para organizar os dados para tabela 1
my_reshape <- function (x, y) {
  x <- t(x) %>% as.data.frame()
  x <- cbind(rownames(x), x)[-1, ]
  colnames(x) <- colnames(y)
  x
}
# Função para computar o teste t para a média de uma amostra
teste_t_uma <- 
  function (d) {
    group_by(d, entrada, saida) %>% 
      summarise('*t*' = t.test(erro, mu = 0, alternative = "two.sided")$statistic,
                '*P*' = t.test(erro, mu = 0, alternative = "two.sided")$p.value)
  }
