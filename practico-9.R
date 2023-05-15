options(scipen = 999)

if (!require("pacman")) install.packages("pacman") # Instalamos pacman en caso de necesitarlo
pacman::p_load(wooldridge, #Para descargar los datos
               dplyr, #Para procesar datos
               ggplot2,#Para graficar
               sjmisc, #Para explorar los datos
               sjPlot, #Para explorar los datos
               texreg) #Para presentar el modelo de regresión estimado
data("hprice2")

m1 = lm(price ~ nox + rooms, data = hprice2)
screenreg(m1, custom.model.names = "Modelo 1")

m2 = lm(price ~ lnox + rooms, data = hprice2)
screenreg(list(m1,m2), custom.model.names = c("Modelo 1", "Modelo 2"))

m3 = lm(lprice ~ nox + rooms, data = hprice2)
screenreg(list(m1,m2, m3), custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3"))

m4 = lm(lprice ~ lnox + rooms, data = hprice2)
screenreg(list(m1,m2, m3, m4), custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4"))

hprice2$rooms2 = (hprice2$rooms)^2
m5 = lm(price ~ rooms + rooms2, data = hprice2)
screenreg(list(m5), custom.model.names = c("Modelo 5"))
