library(tidyverse)
library(readr)
library(ggplot2)
library(qqplotr)
library(fpp3)
cerveza <- read_csv("monthly-beer-production-in-austr.csv")
cerveza_tsibble <- cerveza %>% 
  mutate(Month = yearmonth(Month),
         anio = str_sub(Month, start = 1, end = 4),
         mes = str_sub(Month, start = 6, end = 8), 
         fecha = Month,
         produccion = `Monthly beer production`) %>%
  select(-c(Month, `Monthly beer production`)) %>% 
  as_tsibble(index = fecha) # No pongo el argumento "key" porque cuando quiero hacer el gráfico de rezagos me dice que tengo muchas series de tiempo (vaya a saber Dios porqué)

# Filtro los años con los que voy a trabajar
cerveza1 <- cerveza_tsibble %>% 
  filter(anio >= "1988",
         anio < "1994") %>% 
  mutate(mes = rep(c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"), times = 6),
         mes = factor(mes,levels = c( "Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                      "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")))

# Fijo un tema para todos los gráficos
theme_set(theme_bw())
color <- c("red", "#FF7F24", "#FFB90F", "#548B54", "#9ACD32", "seagreen2",
           "#27408B", "#63B8FF", "#FF3E96", "#7D26CD", "#B03060", "#7FFF00")

# Gráfico temporal
ggplot(cerveza1) +
  aes(x = fecha, y = produccion) +
  geom_line(color = color[11]) + 
  geom_point(color = color[11]) +
  scale_x_yearmonth(breaks = c(cerveza1$fecha[1], cerveza1$fecha[13], 
                               cerveza1$fecha[25], cerveza1$fecha[37],
                               cerveza1$fecha[49], cerveza1$fecha[61],
                               cerveza1$fecha[72])) +
  theme(axis.text = element_text(color = "black")) +
  labs(title = "Gráfico 1: Producción mensual de cerveza",
       subtitle = "Australia",
       x = "Año", y = "Megalitros") 


# Gráfico por año para ver la estacionalidad -reacomodar lo del eje X-
ggplot(cerveza1) +
  aes(x = mes, y = produccion, color = anio, group = anio) +
  geom_line(linewidth = 0.9) +
  scale_color_manual("Año",values = color[6:12]) +
  theme(axis.text = element_text(color = "black"),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 12)) +
  labs(title = "Gráfico 2: Producción mensual de cerveza por año",
       x = "Mes", y = "Megalitros")


# Boxplot 
ggplot(cerveza1) +
  aes(x = anio, y = produccion) +
  geom_boxplot(color = color[7], fill = "white") +
  theme(axis.text = element_text(color = "black")) +
  labs(title = "Gráfico 3: Boxplot por año", x = "Año", y = "Megalitros")

# Veo que las variancias no son constantes a través del tiempo por lo que voy a 
# tener que aplicar una transformación.

# Transformación: tal vez no sea necesaria pq complica 

transform <- MASS::boxcox(lm(cerveza1$produccion~1), 
                          lambda = c(-5, -4, -3, -2, -1, -0.5, 0 , 0.5, 1, 2))

lambda <- data.frame(
  l_4 =((cerveza1$produccion^-4)-1)/-4,
  l_3 = ((cerveza1$produccion^-3)-1)/-3,
  l_2 = ((cerveza1$produccion^-2)-1)/-2,
  l_1 = 1/cerveza1$produccion,
  l_0 = log(cerveza1$produccion),
  l_1.1 = cerveza1$produccion, 
  l_2.2 = cerveza1$produccion^2
)

coef_var <- data.frame(
  lambda = c(-4, -3, -2, -1, 0, 1, 2),
  coef = round(c(sd(lambda$l_4)/mean(lambda$l_4), sd(lambda$l_3)/mean(lambda$l_3),
           sd(lambda$l_2)/mean(lambda$l_2), sd(lambda$l_1)/mean(lambda$l_1),
           sd(lambda$l_0)/mean(lambda$l_0), sd(lambda$l_1.1)/mean(lambda$l_1.1),
           sd(lambda$l_2.2)/mean(lambda$l_2.2)),4)
)
  

cerveza2 <- cerveza %>%
  mutate(Month = yearmonth(Month),
         anio = str_sub(Month, start = 1, end = 4),
         mes = str_sub(Month, start = 6, end = 8), 
         produccion = `Monthly beer production`) %>%
  select(-c(Month, `Monthly beer production`)) %>% 
  filter(anio >= "1988",
         anio < "1994")
  
medidas <-cerveza2 %>%
  group_by(anio) %>% 
  summarise(desvio = sd(produccion), var = desvio^2,
            media = mean(produccion),total = sum(produccion)) %>% 
  arrange(anio)
kableExtra::kable(medidas)

# Si bien los desvios para cada año son diferentes entre si, se mantienen todos 
# dentro del mismo rango de valores. No hay nada raro digamos; entonces decido no
# transformar para no complicar la interpretación de la variable respuesta.

# Test "uni-roots": variable NO tranformada 
cerveza1 %>% 
  features(produccion, unitroot_kpss)

cerveza1 %>% 
  features(produccion, unitroot_nsdiffs)
cerveza1 %>% 
  features(produccion, feat_stl)


# Sin embargo, si se aplica la función "unitroot_nsdiffs" para ver cuántas 
# diferenciaciones son necesarias en la parte estacional, este devuelve 1; además 
# el test de Fs da un valor mayor a 0.64 por lo que se debería hacer una diferenciacion
# en la parte estacional.

# Diferenciamos:
cerveza1 <- cerveza1 %>% 
  mutate(prod_diff = difference(produccion, 12))

cerveza1 %>% 
  features(prod_diff, feat_stl)

# El valor de la estadistica Fs es menor a 0.64, no se necesitan más diferenciaciones

# Gráfico de autocorrelacion
auto_corr <- cerveza1 %>% 
  ACF(prod_diff, lag_max = 48)

acf <- ggplot(auto_corr) +
  geom_segment(aes(xend = lag, yend = 0, y = acf, x = lag), color = "black") + 
  geom_hline(yintercept = 0, color = "black") +             
  geom_hline(yintercept = c(-0.23, 0.23), 
             color = color[2], linetype = "dashed") + 
  theme(axis.text = element_text(color = "black", size = 12)) +
  labs(title = "Gráfico 4: ACFM", x = "Lag", y = "ACF")

# Autocorrelacion parcial
part_acf <- cerveza1 %>% 
  PACF(prod_diff, lag_max = 48)

pacf <- ggplot(part_acf) +
  geom_segment(aes(xend = lag, yend = 0, y = pacf, x = lag), color = "black") + 
  geom_hline(yintercept = 0, color = "black") +             
  geom_hline(yintercept = c(-0.23, 0.23), 
             color = color[2], linetype = "dashed") +   
  theme(axis.text = element_text(color = "black", size = 12)) +
  labs(title = "Gráfico 5: PACFM", x = "Lag", y = "PACF")

gridExtra::grid.arrange(acf, pacf, ncol = 1, nrow = 2)

# ANÁLISIS: Teniendo en cuenta que en la parte estacional solo hay un solo pico
# cuasi-significativo (en el lag 12 en la ACF) y se observa un decaimiento lento 
# de un lag a otro (del 12 al 24, al 36, etc) podriamos plantear un proceso
# autorregresivo en esta parte; en particular de orden 1 (y probar de orden 0)
# Por otro lado, se tiene un colo pico significativo en la PACF (lag 10) que podria
# ser debido a la interacción de las partes regular y estacional, fuera de ese 
# ningún otro es significativo como para saber que tipo de proceso probar; aun así
# vemos que tanto la ACF como PACF muestran un comortamiento de decrecimiento lento.
# Se probarán combinaciones de órdenes de proces AR y MA. (orden 0 y 1)

# MODELOS CANDIDATOS: 
# 1) SARIMA(0, 0, 1)(0, 1, 0)
# 2) SARIMA(1, 0, 0)(0, 1, 0)
# 3) SARIMA(1, 0, 1)(0, 1, 0)
# 4) SARIMA(0, 0, 1)(1, 1, 0)
# 5) SARIMA(1, 0, 0)(1, 1, 0)
# 6) SARIMA(1, 0, 1)(1, 1, 0)


modelos <- cerveza1 %>% 
  model(mod_1 = ARIMA(produccion ~ 0 + pdq(0, 0, 1) + PDQ(0, 1, 0)),
        mod_2 = ARIMA(produccion ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 0)),
        mod_3 = ARIMA(produccion ~ 0 + pdq(1, 0, 1) + PDQ(0, 1, 0)),
        mod_4 = ARIMA(produccion ~ 0 + pdq(0, 0, 1) + PDQ(1, 1, 0)),
        mod_5 = ARIMA(produccion ~ 0 + pdq(1, 0, 0) + PDQ(1, 1, 0)),
        mod_6 = ARIMA(produccion ~ 0 + pdq(1, 0, 1) + PDQ(1, 1, 0)),
        automatico = ARIMA(produccion ~ 0, stepwise = F, approximation = F))


modelos %>% 
  pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")

a <- glance(modelos) %>% 
  arrange(AICc) %>% 
  select(.model:BIC)
# Para hacer un filtro y no hacer el análisis de residuos de los 6 modelos, me 
# quedo con los primeros 3 modelos con mejor AIC. (mod_1, mod_2 y mod_6)
mod_1 <- cerveza1 %>% 
  model(ARIMA(produccion ~ 0 + pdq(0, 0, 1) + PDQ(0, 1, 0))) 

mod_2 <- cerveza1 %>% 
  model(ARIMA(produccion ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 0)))

mod_6 <- cerveza1 %>% 
  model(ARIMA(produccion ~ 0 + pdq(1, 0, 1) + PDQ(1, 1, 0)))

# Veamos si los modelos cumplen los supuestos.

# SUPUESTO N°1: Esperanza igual a 0
res_1 <- cerveza1 %>% 
  model(ARIMA(produccion ~ 0 + pdq(0, 0, 1) + PDQ(0, 1, 0))) %>% 
  residuals()

res_2 <- cerveza1 %>% 
  model(ARIMA(produccion ~ 0 + pdq(1, 0, 0) + PDQ(0, 1, 0))) %>% 
  residuals()

# res_3 <- cerveza1 %>% 
#    model(ARIMA(produccion ~ 0 + pdq(1, 0, 1) + PDQ(0, 1, 0))) %>% 
#    residuals()
# 
# res_4 <- cerveza1 %>% 
#    model(ARIMA(produccion ~ 0 + pdq(0, 0, 1) + PDQ(1, 1, 0))) %>% 
#    residuals()
# 
# res_5 <- cerveza1 %>% 
#    model(ARIMA(produccion ~ 0 + pdq(1, 0, 0) + PDQ(1, 1, 0))) %>% 
#    residuals()

res_6 <- cerveza1 %>% 
  model(ARIMA(produccion ~ 0 + pdq(1, 0, 1) + PDQ(1, 1, 0))) %>% 
  residuals()


graf_1 <- ggplot(res_1) +
  aes(x = fecha, y = scale(.resid)) +
  geom_point(pch = 19, color =  color[2]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_yearmonth(breaks = c(cerveza1$fecha[1], cerveza1$fecha[13], 
                               cerveza1$fecha[25], cerveza1$fecha[37],
                               cerveza1$fecha[49], cerveza1$fecha[61],
                               cerveza1$fecha[72])) +
  theme(axis.text = element_text(color = "black")) +
  labs(x = "", y = "Residuos", title = "SARIMA(0, 0, 1)(0, 1, 0)")

graf_2 <- ggplot(res_2) +
  aes(x = fecha, y = scale(.resid)) +
  geom_point(pch = 19, color =  "chartreuse4") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_yearmonth(breaks = c(cerveza1$fecha[1], cerveza1$fecha[13], 
                               cerveza1$fecha[25], cerveza1$fecha[37],
                               cerveza1$fecha[49], cerveza1$fecha[61],
                               cerveza1$fecha[72])) +
  theme(axis.text = element_text(color = "black")) +
  labs(x = "", y = "Residuos", title = "SARIMA(1, 0, 0)(0, 1, 0)")

graf_3 <- ggplot(res_6) +
  aes(x = fecha, y = scale(.resid)) +
  geom_point(pch = 19, color =  color[7]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_yearmonth(breaks = c(cerveza1$fecha[1], cerveza1$fecha[13], 
                               cerveza1$fecha[25], cerveza1$fecha[37],
                               cerveza1$fecha[49], cerveza1$fecha[61],
                               cerveza1$fecha[72])) +
  theme(axis.text = element_text(color = "black")) +
  labs(x = "Fecha", y = "Residuos", title = "SARIMA(1, 0, 1)(1, 1, 0)")

gridExtra::grid.arrange(graf_1, graf_2, graf_3, nrow = 3, ncol = 1)
# Los residuos de los 3 modelos parecen estar alrededor de cero; por lo que podemos
# decir que este primero supuesto se cumple

# SUPUESTO N°2: Variancia constante
# En ninguno de los 3 gráficos se observa un patrón especifico (tunel abierto o 
# cerrado), parece ser que la variancia es constante


# SUPUESTO N°3: los residuos están incorrelados

# Modelo 1: SARIMA(0, 0, 1)(0, 1, 0)
res_1_acf <- res_1 %>% 
  ACF(.resid, lag_max = 48)
res_1_pacf <- res_1 %>% 
  PACF(.resid, lag_max = 48) 

graf_4 <- ggplot(res_1_acf) +
  geom_segment(aes(xend = lag, yend = 0, y = acf, x = lag), color = "black") + 
  geom_hline(yintercept = 0, color = "black") +             
  geom_hline(yintercept = c(-0.23, 0.23), 
             color = color[2], linetype = "dashed") + 
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(color = "black")) +
  labs(title = "SARIMA(0, 0, 1)(0, 1, 0)", x = " ", y = "ACF")

graf_5 <- ggplot(res_1_pacf) +
  geom_segment(aes(xend = lag, yend = 0, y = pacf, x = lag), color = "black") + 
  geom_hline(yintercept = 0, color = "black") +             
  geom_hline(yintercept = c(-0.23, 0.23), 
             color = color[2], linetype = "dashed") +  
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(color = "black"))  +
  labs(title = " ", x = "Lag", y = "PACF")

augment(mod_1) %>% 
  features(.innov, ljung_box, lag = 48, dof = 1)
# La probabilidad asociada a la estadística es > 0.05 ---> no rechazo H0) 

# Modelo 2: SARIMA(1, 0, 0)(0, 1, 0)
res_2_acf <- res_2 %>% 
  ACF(.resid, lag_max = 48) 
res_2_pacf <- res_2 %>% 
  PACF(.resid, lag_max = 48)

graf_6 <- ggplot(res_2_acf) +
  geom_segment(aes(xend = lag, yend = 0, y = acf, x = lag), color = "black") + 
  geom_hline(yintercept = 0, color = "black") +             
  geom_hline(yintercept = c(-0.23, 0.23), 
             color = "chartreuse4", linetype = "dashed") + 
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(color = "black")) +
  labs(title = "SARIMA(1, 0, 0)(0, 1, 0)", x = " ", y = " ")

graf_7 <- ggplot(res_2_pacf) +
  geom_segment(aes(xend = lag, yend = 0, y = pacf, x = lag), color = "black") + 
  geom_hline(yintercept = 0, color = "black") +             
  geom_hline(yintercept = c(-0.23, 0.23), 
             color = "chartreuse4", linetype = "dashed") +     
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(color = "black")) +
  labs(title = " ", x = "Lag", y = " ")

augment(mod_2) %>% 
  features(.innov, ljung_box, lag = 48, dof = 1)
# La probabilidad asociada a la estadística es > 0.05 ---> no rechazo H0) 


# Los gráficos de ambos modelos son practicamente iguales; en la ACF no se tienen
# rezagos significativos salvo por los rezagos 22 y 26 y en la PACF se tiene 
# significativos los rezago 10 y 19.
# En ambos casos no tienen valores mayores al 0.5 así que se podría decir que está
# todo en orden.

# Modelo 6: SARIMA(1, 0, 1)(1, 1, 0)
res_6_acf <- res_6 %>% 
  ACF(.resid, lag_max = 48) 
res_6_pacf <- res_6 %>% 
  PACF(.resid, lag_max = 48)

graf_8 <- ggplot(res_6_acf) +
  geom_segment(aes(xend = lag, yend = 0, y = acf, x = lag), color = "black") + 
  geom_hline(yintercept = 0, color = "black") +             
  geom_hline(yintercept = c(-0.23, 0.23), 
             color = color[7], linetype = "dashed") +  
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(color = "black")) +
  labs(title = "SARIMA(1, 0, 1)(1, 1, 0)", x = " ", y = " ")

graf_9 <- ggplot(res_6_pacf) +
  geom_segment(aes(xend = lag, yend = 0, y = pacf, x = lag), color = "black") + 
  geom_hline(yintercept = 0, color = "black") +             
  geom_hline(yintercept = c(-0.23, 0.23), 
             color = color[7], linetype = "dashed") +   
  theme(plot.title = element_text(size = 10),
        axis.text = element_text(color = "black")) +
  labs(title = " ", x = "Lag", y = " ")

bind_rows(
  augment(mod_2) %>% 
  features(.innov, box_pierce, lag = 6, dof = 1),
  augment(mod_2) %>% 
    features(.innov, box_pierce, lag = 12, dof = 1),
  augment(mod_2) %>% 
    features(.innov, box_pierce, lag = 18, dof = 1),
  augment(mod_2) %>% 
    features(.innov, box_pierce, lag = 24, dof = 1)
)

# La probabilidad asociada es mayor a 0.05 ---> No rechazo H0)

# Al igual que los modelos anteriores, la ACF tiene los rezagos 22 y 26 significativos
# pero con valores que apenas pasan el -0.2 y el 0.2 respectivamente; por otro lado
# la PACF no muestra rezagos significativos. La parte estacional está bien descripta
# ya que los rezagos 12, 24, 36, etc no son significativos; se cumple el supuesto.

gridExtra::grid.arrange(graf_4, graf_6, graf_8,
                        graf_5, graf_7, graf_9, ncol = 3, nrow = 2)

# SUPUESTO N°4: los residuos se distribuyen normales

graf_10 <- ggplot(data = res_1, aes(sample = scale(.resid))) +
  stat_qq_band(bandType = "boot", fill = "grey") +
  stat_qq_line() +
  stat_qq_point(color = color[2]) +
  labs(title = "SARIMA(0, 0, 1)(0, 1, 0)",
       caption = "Test de Lilliefors: Prob. Asoc. = 0.0065") +
  theme(plot.caption = element_text(hjust = 0.5, color = "black"),
        axis.text = element_text(color = "black"))


graf_11<- ggplot(data = res_2, aes(sample = scale(.resid))) +
  stat_qq_band(bandType = "boot", fill = "grey") +
  stat_qq_line() +
  stat_qq_point(color = "chartreuse4") +
  labs(title = "SARIMA(1, 0, 0)(0, 1, 0)",
       caption = "Test de Lilliefors: Prob. Asoc. = 0.0068") +
  theme(plot.caption = element_text(hjust = 0.5, color = "black"),
        axis.text = element_text(color = "black"))

graf_12 <- ggplot(data = res_6, aes(sample = scale(.resid))) +
  stat_qq_band(bandType = "boot", fill = "grey") +
  stat_qq_line() +
  stat_qq_point(color = color[7]) +
  labs(title = "SARIMA(1, 0, 1)(1, 1, 0)", 
       caption = "Test de Lilliefors: Prob. Asoc. = 0.001") +
  theme(plot.caption = element_text(hjust = 0.5, color = "black"),
        axis.text = element_text(color = "black"))

gridExtra::grid.arrange(graf_10, graf_11, graf_12, ncol = 3, nrow = 1)

nortest::lillie.test(scale(res_1$.resid))
nortest::lillie.test(scale(res_2$.resid))
nortest::lillie.test(scale(res_6$.resid))
# Ninguno cumple la normalidad

# Veamos la significación de los parámetros

report(mod_1) # Parece ser que el theta estimado no es significativo
report(mod_2) # Parece ser que el phi estimado no es signifcativo
report(mod_6) # Todos los parametros son significativos 

list(
  c(0.988-1.96*0.0233,0.988+1.96*0.0233),
  c(-0.9312-1.96*0.0604, -0.9312+1.96*0.0604),
  c(-0.2886-1.96*0.1365, -0.2886+1.96*0.1365)
)
# En el modelo 6, todos los parametros cumplen con las condiciones para que el 
# modelo sea admisible: |phi|<1; |theta|<1, |theta grande|<1.


# Veamos si los modelos son estables
# Para hacer eso debo ver la matriz de correlación de los parámetros del modelo 
# pero como los modelos 1 y 2 solo tienen 1 parámetro, creo yo que no tiene sentido 
# porque no hay nada con lo que puedan tener correlación.
# El modelo 6 es otra historia.

# Modelo 6:
arima_mod6 <- arima(cerveza1$produccion, order = c(1, 0, 1), 
                    seasonal = list(order = c(1, 1, 0), period = 12))

cov_matrix <- arima_mod6$var.coef
correlation_matrix <- cov2cor(cov_matrix)
# Hay una correlación alta


# Si bien sabemos que el modelo 6 tiene al mejor AIC y  AICc, y tiene menor variancia
# veamos el MAPE de los 3 modelos para ver con cuál nos quedamos.

datos_test <- cerveza_tsibble %>% 
  filter(anio == 1994) %>%  
  mutate(mes = rep(c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                     "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"), times = 1),
         mes = factor(mes,levels = c( "Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                      "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")))

bind_rows(
  mod_1 %>% accuracy() %>% select(c(.model, .type,MAPE)),
  mod_1 %>% forecast(h = 6) %>% accuracy(datos_test) %>% select(c(.model, .type,MAPE)),
  mod_2 %>% accuracy() %>% select(c(.model, .type,MAPE)),
  mod_2 %>% forecast(h = 6) %>% accuracy(datos_test) %>% select(c(.model, .type,MAPE)),
  mod_6 %>% accuracy() %>% select(c(.model, .type,MAPE)),
  mod_6 %>% forecast(h = 6) %>% accuracy(datos_test) %>% select(c(.model, .type,MAPE))
)

# El modelo 6 tiene por lejos el mejor MAPE

# Gráfico con los pronosticos y los valores reales:

pron <- mod_6 %>% 
  forecast(h = 6)

pron1 <- data.frame(
  anio = rep("1994", times = 6),
  mes = c("Ene", "Feb", "Mar", "Abr", "May", "Jun"),
  fecha = pron$fecha,
  produccion = round(pron$.mean,0), 
  tipo = "Pronóstico"
) %>% 
  as_tibble()

cerveza_pron <- cerveza_tsibble %>% 
  filter(anio >= "1988",
         anio < "1995") %>% 
  mutate(mes = rep(c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                     "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"), times = 7),
         mes = factor(mes,levels = c( "Ene", "Feb", "Mar", "Abr", "May", "Jun",
                                      "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
         tipo = "Observado") %>% 
  as_tibble()


datos <- bind_rows(cerveza_pron[61:78,], pron1)

ggplot(datos) +
  aes(x = fecha, y = produccion, color = tipo) +
  geom_line() + 
  geom_point() +
  scale_color_manual("Dato",values = c(color[11], color[3])) +
  scale_x_yearmonth(breaks = c(datos$fecha[1], datos$fecha[6],
                               datos$fecha[13], datos$fecha[18])) +
  labs(title = "Gráfico 9: Producción mensual de cerveza",
       x = "Fecha", y = "Megalitros") +
  theme(axis.text = element_text(color = "black"), legend.position = "bottom")
# hacer tabla con los intervalos