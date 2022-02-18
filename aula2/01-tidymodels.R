library(tidymodels)
library(ggplot2)
library(skimr)


# Dados -------------------------------------------------------------------

data("diamonds")

# EDA ---------------------------------------------------------------------
glimpse(diamonds)
skim(diamonds)
# pair_plot
# GGally::ggpairs(diamonds %>% sample_n(200))
qplot(x, price, data = diamonds)

# Precisamos passar pro R:
# 1. A f que queremos usar
# 2. Ajustar a f para um conjunto de dados


# -------------------------------------------------------------------------

# Passo 1: Especificações de
# a) a f (hipótese) com seus respectivos hiperparâmetros;
# b) o pacote 'motor' (engine);
# c) a tarefa/modo ("regression" ou "classification")

# tidymodels generaliza os diferentes tipos de engine
# qual usar? depende do que você quer, por exemplo, se você quiser ajustar o
# modelo com penalização, o 'lm' não vai te ajudar, você vai precisar usar o 'glmnet'
# tidymodels - problemas sempre supervisionados, reg, classif e analise de sobrevivencia

especificacao_modelo <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# Outros exemplos...

# especificacao_modelo <- decision_tree() %>% 
#   set_engine("rpart") %>% 
#   set_mode("regression")
# 
# especificacao_modelo <- rand_forest() %>% 
#   set_engine("rpart") %>% 
#   set_mode("regression")

# -------------------------------------------------------------------------
# Passo 2: Ajuste o modelo

modelo <- especificacao_modelo %>% 
  fit(price ~ x, data = diamonds)
    # variavel resposta ~ feature + f2 + f3
modelo

# intercept = beta0, x = beta1

# -------------------------------------------------------------------------
# Passo 3: Analisar previsões

predict(modelo, new_data = diamonds)

## outro jeito
only_x <- diamonds %>% select(x)
predict(modelo, new_data = only_x)

diamonds_com_previsao <- diamonds %>% 
  add_column(predict(modelo, new_data = diamonds))

# Pontos observados + curva da f
diamonds_com_previsao %>% 
  filter(x > 0) %>% 
  sample_n(1000) %>% 
  ggplot() + 
  geom_point(aes(x, price), alpha = 0.3) + 
  geom_point(aes(x, .pred), color = "red") +
  theme_classic()

# Observado vs Esperado
diamonds_com_previsao %>% 
  filter(x > 0) %>% 
  ggplot() +
  geom_point(aes(.pred, price)) +
  # reta identidade -> y = 0 + 1x
  geom_abline(slope = 1, intercept = 0, colour = "red", size = 1) +
  theme_classic()

# Onde meu modelo está errando?
diamonds_com_previsao %>% 
  filter(x > 0) %>% 
  ggplot() +
  geom_point(aes(price, price - .pred)) + 
  theme_classic()
  
# Como quantificar a qualidade de um modelo?
diamonds_com_previsao %>% rmse(truth = price, estimate = .pred)
diamonds_com_previsao %>% mae(truth = price, estimate = .pred)
diamonds_com_previsao %>% rsq(truth = price, estimate = .pred)

# -------------------------------------------------------------------------
# Passo 4: Melhorar o pré processamento

qplot(price, data = diamonds, geom = "histogram")

## fazer o log, por exemplo
qplot(log(price), data = diamonds, geom = "histogram")

diamonds_log <- diamonds %>% mutate(log_price = log(price))

modelo_log <- especificacao_modelo %>% 
  fit(log_price ~ x, data = diamonds_log)

# muda a interpretação, porqueo modelo agora é
# price = exp(2.8205 + 0.86 * x)
diamonds_com_previsao_log <- diamonds_log %>% 
  add_column(predict(modelo_log, new_data = diamonds_log)) %>% 
  mutate(.pred_price = exp(.pred))

# o problema vira exponencial
diamonds_com_previsao_log %>% 
  filter(x > 0) %>% 
  sample_n(1000) %>% 
  ggplot() + 
  geom_point(aes(x, price), alpha = 0.3) + 
  geom_point(aes(x, .pred_price), color = "red") +
  theme_classic()

diamonds_com_previsao_log %>% 
  filter(x > 0) %>% 
  ggplot() +
  geom_point(aes(.pred_price, price)) +
  # reta identidade -> y = 0 + 1x
  geom_abline(slope = 1, intercept = 0, colour = "red", size = 1) +
  theme_classic()

# erro pra mais e pra menos da mesma forma, mas
# quanto maior o preço, a variabilidade é maior
# heterocedasticidade, a variância não é constante

diamonds_com_previsao_log %>% 
  filter(x > 0) %>%
  sample_n(1000) %>% 
  ggplot() +
  geom_point(aes(price, price - .pred_price)) + 
  theme_classic()

diamonds_com_previsao_log %>% rmse(truth = price, estimate = .pred_price)
diamonds_com_previsao_log %>% mae(truth = price, estimate = .pred_price)
diamonds_com_previsao_log %>% rsq(truth = price, estimate = .pred_price)

# qual escolher? o segundo, por mais que as métricas sejam piores
# o problema? quando o tamanho do diamante é muito alto, o modelo não ta funcionando

# -------------------------------------------------------------------------
# Feature Engineering
diamonds_ft <- diamonds %>% mutate(log_price = log(price),
                                   flag_maior_oito = x > 8)

modelo_ft <- especificacao_modelo %>% 
  fit(log_price ~ x + flag_maior_oito, data = diamonds_ft)

diamonds_com_previsao_ft <- diamonds_ft %>% 
  add_column(predict(modelo_ft, new_data = diamonds_ft)) %>% 
  mutate(.pred_price = exp(.pred))

# enxerga melhor
diamonds_com_previsao_ft %>% 
  filter(x > 0) %>% 
  sample_n(1000) %>% 
  ggplot() + 
  geom_point(aes(x, price), alpha = 0.3) + 
  geom_point(aes(x, .pred_price), color = "red") +
  theme_classic()

diamonds_com_previsao_ft %>% 
  filter(x > 0) %>%
  sample_n()
  ggplot() +
  geom_point(aes(.pred_price, price)) +
  geom_abline(slope = 1, intercept = 0, colour = "red", size = 1) +
  theme_classic()

# erro pra mais e pra menos da mesma forma, mas
# quanto maior o preço, a variabilidade é maior
# heterocedasticidade, a variância não é constante

diamonds_com_previsao_ft %>% 
  filter(x > 0) %>%
  sample_n(1000) %>% 
  ggplot() +
  geom_point(aes(price, price - .pred_price)) + 
  theme_classic()

diamonds_com_previsao_ft %>% 
  group_by(flag_maior_oito) %>%
  rmse(truth = price, estimate = .pred_price)

diamonds_com_previsao_ft %>% 
  group_by(flag_maior_oito) %>%
  mae(truth = price, estimate = .pred_price)

diamonds_com_previsao_ft %>% 
  group_by(flag_maior_oito) %>% 
  rsq(truth = price, estimate = .pred_price)

# junta as métricas tudo numa só
metrics <- metric_set(rmse, mae, rsq)

diamonds_com_previsao_ft %>% 
  group_by(flag_maior_oito) %>% 
  metrics(truth = price, estimate = .pred_price)

# calcula as métricas par aum range
diamonds_com_previsao_ft %>%
  group_by(groups = cut(x, 3)) %>% 
  metrics(truth = price, estimate = .pred_price)
