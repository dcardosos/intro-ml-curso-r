# Exercícios - Tidymodels

# pacotes -----------------------------------------------------------------

library(tidymodels)
library(rpart.plot)
library(ISLR)

# dados ------------------------------------------------------------------
View(Hitters)
help(Hitters)

# exercício 1 -------------------------------------------------------------
# Defina uma especificação de f que caracterize uma árvore de regressão 
# (mode 'regression') com o tree_depth = 3 e use "rpart" como 'engine'.
# Curiosidade (veremos depois): tree_depth é a profundidade máxima da árvore.
# Dicas: decision_tree(), set_engine(), set_mode().

especificacao_modelo <- decision_tree(tree_depth = 3) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")


# exercicio 2 -------------------------------------------------------------
# a) Usando a base Hitters, ajuste o modelo de árvore para os 'HmRun' (Home Runs em 1986) 
# de cada jogador usando como variável explicativa 'CHmRun' (o número de Home Runs que ele fez na vida).
# Dicas: fit(), uma fórmula e um data.frame.

modelo <- especificacao_modelo %>% 
  fit(CHmRun ~ HmRun, data = Hitters)

# b) use rpart.plot(ajuste$fit) para visualizar a sua árvore. O que a intensidade
# da cor azul informa?
# OBS: troque o nome do objeto 'ajuste' para o nome do objeto que você
# criou em (a). Por exemplo: rpart.plot(hitters_ajuste$fit)

rpart.plot(modelo$fit)


# exercicio 3 --------------------------------------------------------------
# Coloque uma coluna a mais no banco de dados com as predições.
# Dicas: predict()

hitters_com_pred <- Hitters %>% 
  add_column(predict(modelo, new_data = Hitters))


# exercicio 4 -------------------------------------------------------------
# Calcule RMSE, MAE e R-quadrado do modelo que você ajustou.
# Dicas: use as funções do yardstick rmse(), mae(), rsq().

metrics <- metric_set(rmse, mae, rsq)

hitters_com_pred %>% 
  metrics(truth = CHmRun, estimate = .pred)

# exercicio 5 [desafio] ---------------------------------------------------
# a) Faça um gráfico de dispersão entre HmRun e CHmRun e coloque a curva ajustada em cima.
# Dicas: use a tabela feita em (a). ggplot2() + geom_point() + geom_line()

hitters_com_pred %>% 
  ggplot() +
  geom_point(aes(HmRun, CHmRun), alpha = 0.3) + 
  geom_line(aes(HmRun, .pred), color = "red")


# b) Faça um gráfico de dispersão comparando as suas predições e o que realmente aconteceu.
# Dicas: use a tabela feita em (a). ggplot2() + geom_point()

hitters_com_pred %>% 
  ggplot() +
  geom_point(aes(.pred, CHmRun)) +
  geom_abline(slope = 1, intercept = 0, color = "red")