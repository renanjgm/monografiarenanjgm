library(mirt)
library(readxl)
library(tidyverse)

dados<- read_excel("dados_25_03.xlsx")

# como tivemos algumas alterações dos dados para ajuste essas sao as alterações. 

# dados_edit2 <- dados %>%
#   mutate(
#     s2 = case_when(
#       is.na(s2) ~ NA_real_,
#       s2 == 2 ~ 1,
#       TRUE ~ 0
#     ),
#     c3 = case_when(
#       is.na(c3) ~ NA_real_,
#       c3 == 2 ~ 1,
#       TRUE ~ 0
#     )
#   )%>%
#   select(-f1)

mod <- mirt(dados, 1, itemtype = "graded")
Theta <- fscores(mod,full.scores=TRUE)

coef2 <- coef(mod, IRTpars = TRUE, simplify = TRUE)[1]
coef2_vetor <-coef2[[1]]

coluna_a <- matrix(coef2_vetor[, 1])
colunas_d <- coef2_vetor[, c(2, 3)]
set.seed(912)

simulacaoreal <- simdata(coluna_a, colunas_d, 108, Theta = Theta, itemtype ='graded')

simulacao250 <- simdata(coluna_a, colunas_d, 250, itemtype = 'graded')

simulacao500 <- simdata(coluna_a, colunas_d, 500, itemtype = 'graded')

simulacao1000 <- simdata(coluna_a, colunas_d, 1000, itemtype = 'graded')

simulacao2000 <- simdata(coluna_a, colunas_d, 2000, itemtype = 'graded')


# função de Probalbilibade gradual 
#  -> theta a b1 b1 







