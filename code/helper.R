# Função para organizar os dados para tabela 1
my_reshape <- function (x, y) {
  x <- t(x) %>% as.data.frame()
  x <- cbind(rownames(x), x)[-1, ]
  colnames(x) <- colnames(y)
  x
}
