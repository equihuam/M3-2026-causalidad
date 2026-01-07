library(ggplot2)
library(dplyr)

suppressPackageStartupMessages(library(tidyverse))


# Función que contiene los términos de la simulación del experimento
experimento <- function (n, m_real, sd_real, h1, p)
{
  # datos simulados
  datos <- data.frame(y = rnorm(n, m_real, sd_real))
 
  # estimadores estadísticos a partir de los datos simulados
  es_estimado <-  sd(datos$y) / sqrt(n)
  m_estimada <- mean(datos$y)
  m_h0 = 0
  q_h0_up = qnorm(p = p, mean = 0, sd = es_estimado,  lower.tail = F)
  beta_p = 1 - pnorm(q_h0_up, mean = h1, sd = es_estimado)
  
  # Valores teóricos de las distribuciones suponiendo H0 y H1
  #intervalo de valores de respuesta explorados
  x <- seq(from = -3 * es_estimado, 
           to = max(h1 + 3 * es_estimado, m_estimada), 
           length = 1500 )
  # valores de densidad de probabilidad asociado a cada valor x
  dist_hs <- data.frame(x = x, 
                        y_h0 = dnorm(x, 0,  sd = es_estimado), 
                        y_h1 = dnorm(x, h1, sd = es_estimado))
  
  # Graficación ilustrativa del experimento
  etiquetas <- data.frame(
                  x = c(0, h1),
                  y = c(0.8, 0.8),
                  texto = c("H0", "H1"))
  
  grafica <- dist_hs |> 
    ggplot(aes(x = x)) + 
    geom_line(aes(y = y_h0), color = "blue") +
    geom_ribbon(data = dist_hs[x >= q_h0_up,], xmin = q_h0_up, 
                aes(xmax = Inf, ymin = 0, ymax = y_h0), 
                fill = "blue", alpha = 0.3) +
    geom_line(aes(y = y_h1), color = "red") +
    geom_ribbon(data = dist_hs[x <= q_h0_up,], xmin = -Inf, 
                aes(xmax = q_h0_up, ymin = 0, ymax = y_h1), 
                fill = "red", alpha = 0.3) +
    geom_ribbon(data = dist_hs[x >= q_h0_up,], xmin = q_h0_up, 
                aes(xmax = q_h0_up, ymin = 0, ymax = y_h1), 
                fill = "yellow", alpha = 0.3) +
    geom_vline(xintercept = m_estimada, 
               color = "darkgreen",
               linetype = "dotted",
               size = 2) +
    geom_label(data = etiquetas, aes(x = x, y = y, label = texto),                 , 
               color="gray40", 
               size=6 , angle=45, fontface="bold" ) +
    labs(title = "Experimento simulado", subtitle = "analiza H0 vs H1") +
    ylab(label = "densidad de probabilidad") +
    xlab(label = "variable de respuesta")
  
  # Organización y entrega de resultados
  resultados <- list(grafica = grafica,
                     resultados = data.frame(h1 = h1,
                           n = n,
                           m_obs = m_estimada,
                           error_es = es_estimado,
                           p_v_obs = 1 - pnorm(q = m_estimada, mean = 0, sd = es_estimado),
                           p_H0 = p,
                           beta_p = beta_p,
                           potencia = 1 - beta_p))
  
  
  return(resultados)
}


# Realiza el experimento
experimento(n = 2, m_real = 1, sd_real = 1, h1 = 0.5, p = 0.05)


#----------------------

experimento_balanceado <- function (n_trat, 
                                    beta_real = c(0.5, 1, 2, 5), 
                                    sd_real = 1, 
                                    beta_h1 = c(1, 1, 1, 1), 
                                    p = 0.05)
{
  # modelo propuesto y = m + 0.5x1 + 1x2 + 2x3 + 5x3 + e
  # datos simulados
  x <- factor(rep(c("control", "trat_1", "trat_2", "trat_3"), each = n_trat))
  datos <- tibble(x, y =  beta_real[1] * (x == "control") + 
                              beta_real[2] * (x == "trat_1") +
                              beta_real[3] * (x == "trat_2") +
                              beta_real[4] * (x == "trat_3") +
                              rnorm(n_trat * length(beta_real), 0, sd_real))
  
  # estimadores estadísticos omnibus a partir de los datos simulados
  model_ajst <- lm(y ~ -1 + x, datos)
  F_estimada <- anova(model_ajst)$"F value"[1]
  gl <- anova(lm(y ~ x, datos))$Df
  
  # Prueba omnibus
  q_h0 <- qf(1 - p, gl[1], gl[2])
  cuantiles <-  seq(from = 0, to = max(q_h0, F_estimada) * 1.1, length = q_h0 * 100)
  dist_h0 <- tibble(q = cuantiles, y_h0 = df(cuantiles, gl[1], gl[2]))
  
  # Gráfica prueba omnibus
  graf_h0 <- ggplot(dist_h0, aes(x = q)) +     
    geom_line(aes(y = y_h0), color = "blue") +
    geom_ribbon(data = dist_h0[cuantiles >= q_h0,], 
                xmin = q_h0, 
                aes(xmax = Inf, ymin = 0, ymax = y_h0), 
                fill = "red", alpha = 0.3) +
    geom_hline(yintercept = 0, 
               color = "blue") + 
    geom_vline(xintercept = F_estimada, 
               color = "darkgreen",
               linetype = "dotted")
    
  # Pruebas ed medias
  betas_est <- summary(model_ajst)$coef[,1:2]
  cuant_betas <- seq(from = -2 * max(betas_est[,1]), 
                     to = 2 * max(betas_est[,1]),
                     length = 1500)
  dist_beta_h0 <- tibble(q = cuant_betas, 
                         yb_h0 = dt(cuant_betas, gl[2])) 
  
  qb_h0 <- qt(p/2, gl[2], lower.tail = F)
  
  # Gráfica valores de las betas
  graf_beta_h0 <- ggplot(dist_beta_h0, aes(x = q)) +     
    geom_line(aes(y = yb_h0), color = "blue") +
    geom_ribbon(data = dist_beta_h0[cuant_betas >= qb_h0, ], 
                xmin = qb_h0, 
                aes(xmax = Inf, ymin = 0, ymax = yb_h0), 
                fill = "red", alpha = 0.3) +
  geom_ribbon(data = dist_beta_h0[cuant_betas <= -qb_h0, ], 
              xmin = -Inf, 
              aes(xmax = -qb_h0, ymin = 0, ymax = yb_h0), 
              fill = "red", alpha = 0.3) +
    geom_hline(yintercept = 0, 
               color = "blue") +
  geom_vline(xintercept = betas_est[,1],
             color = 1:length(betas_est[,1]),
             linetype = "dotted",
             size = 1.2,
             show.legend = T)
  
  
  
  return(list(grafica_omnibus = graf_h0,
              grafica_medias = graf_beta_h0,
              datos = dist_h0,
              modelo = model_ajst))
}

dat_anova <- experimento_balanceado(5, sd_real = 2)

dat_anova

