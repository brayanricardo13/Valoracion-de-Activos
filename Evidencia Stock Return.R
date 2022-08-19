#Evidencia Empírica de Retorno de las Acciones
#El siguiente código permite evaluar si ya existen las librerías e instalar las que no se tengan instaladas

packages<-c("rugarch", "fGarch", "forecast", "tseries", "aTSA", "readr", "psych", "ggfortify", "quantmod", "dynlm", "FinancialMath", "ggplot2", "dplyr", "reshape2")

package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
}
)


# Para descargar los datos financieros podemos usar dos códigos:
#El primero que podemos utilizar es "get.hist.quote"
Indice<- get.hist.quote(instrument = "^GSPC", 
                        start=as.Date("2010-01-04"), 
                        end=as.Date("2021-07-31"), quote = "AdjClose")

#Se puede hacer el gráfico de las series secargadas con "Plot" 
plot(Indice, col="darkgreen", xlab="Fecha", ylab="AdjClose"); title(main="Evoluci?n S&P 5OO")

#"Summary siempre nos permite ver el resumen de los datos

summary(Indice)

#Los retornos se calculan con la primera diferencia del logaritmo de las series, ya que son en tiempo continuo

RetornoIndice<-diff(log(Indice))
head(RetornoIndice,10)

# Graficamos nuevamente
plot(RetornoIndice, main=" ", col="darkgreen", xlab="Fecha", ylab="Rendimientos")
title(main="Rendimientos del Indice S&P 500")

#También podemos calcular el rendimiento promedio y la desviación Estandar
RendimientoPromedio = c(mean(RetornoIndice))
Volatilidad = c(sd(RetornoIndice))

#La otra forma para descargar los datos financieros es usando la librería "quantmod" y el comando getSymbols

getSymbols("^GSPC", src = "yahoo", from = "2010-01-01", to = "2022-07-30", periodicity = "daily", adjusted = FALSE)
#Gráfico
chartSeries(GSPC, TA=NULL)
#Velas Japonesas
chartSeries(GSPC, subset = "last 3 months")

#TAmbién se puede utilizar "ggplot2" para los gráficos

gspc <- as.data.frame(GSPC)
g1 <- ggplot(gspc) + geom_line(mapping = aes(index(gspc),GSPC.Adjusted))
g1 <- g1 + labs(title = "S&P 500", subtitle = "Desde Enero 2010 a 2018") + xlab("Fecha") + ylab("")
g1 <- g1 + theme_bw()
g1

#Calculamos el retorno del índice
logret <- log(GSPC$GSPC.Adjusted+1)
head(logret,10)
chartSeries(logret, TA=NULL)
retorno_syp<-diff(logret)
head(retorno_syp,10)
chartSeries(retorno_syp, TA=NULL)
# Se calcula la media y la volatilidad
retorno_prom = c(mean(retorno_syp))
Vol_syp = c(sd(retorno_syp))


#Veámos algunos ejemplos de varios stocks

#Primero creamos un conjunto al cual nombraremos "portfolio" con acciones de Oracle, AMD, IBM y Nvidia
portfolio <- c("ORCL","AMD","IBM","NVDA")

#Descargamos los datos
getSymbols(portfolio, src = "yahoo", from = "2010-01-01", to = "2022-08-16", periodicity = "daily")

#En este punto usaremos una función que nos permite usar solo el precio de cierre
list <- lapply(portfolio, function(x) Cl(get(x)))
precio.cierre <- do.call(merge,list)

retornos <- data.frame(apply(precio.cierre, 2, function(x) Delt(x, type = "log")),
            fecha = index(precio.cierre)) %>% 
            rename(orcl = ORCL.Close, amd = AMD.Close, ibm = IBM.Close, nvda = NVDA.Close) %>% 
            na.omit() 

            
            
acumulados <- data.frame(apply(retornos[1:4], 2, function(x) cumsum(x)), fecha = index(precio.cierre[-1]))

reshape <- melt(acumulados, id.vars = "fecha")

g2 <- ggplot(reshape) + geom_line(mapping = aes(fecha,value, color = variable))
g2 <- g2 + labs(title = "Retornos Acumulados", subtitle = "Oracle, AMD, IBM y Nvidia")
g2 <- g2 + theme_bw() + xlab("Fecha") + ylab("Retornos Acumulados")
g2 <- g2 + scale_color_manual("portfolio", values = c("red","blue","green","orange"))
g2 <- g2 + theme(legend.position = "bottom")
g2

library(fBasics)
summary <- basicStats(retornos[1:4])[c("Mean", "Stdev", "Median", "Minimum", "Variance",
                                       "Maximum", "nobs","Skewness","Kurtosis"),]

################################################################################
###########################      Bonos   #######################################
################################################################################

Bonos <- c("DTB3","T10Y2Y", "DGS20", "DGS30")
getSymbols(Bonos, src = "FRED", from = "2010-01-01", to = "2022-08-16", periodicity = "daily")
chartSeries(DTB3, TA=NULL)
chartSeries(T10Y2Y, TA=NULL)
chartSeries(DGS20, TA=NULL)
chartSeries(DGS30, TA=NULL)

X = cbind(DTB3,T10Y2Y, DGS20, DGS30)
X = na.omit(X)
df = data.frame(X)

colnames(df) = c("Bill", "BondCP", "BondLP", "BondLLP")
date = as.Date(index(X))
k = ncol(df)

meltdf <- melt(df,id=date)


g3 <- ggplot(df) + geom_line(mapping = aes(x = date, y = Bill)) + 
  labs(title = "Letras del Tesoro", subtitle = "Corto Plazo") + 
  theme_bw() + xlab("Date") + ylab("Tasa de Interés") + 
  scale_color_manual("df", values = c("red","blue","green","orange")) + 
  theme(legend.position = "bottom")
g3

g4 <- ggplot(df) + geom_line(mapping = aes(x = date, y = BondCP)) + 
  labs(title = "Bonos del Tesoro", subtitle = "Corto Plazo") + 
  theme_bw() + xlab("Date") + ylab("Tasa de Interés") + 
  scale_color_manual("df", values = c("red","blue","green","orange")) + 
  theme(legend.position = "bottom")
g4

g5 <- ggplot(df) + geom_line(mapping = aes(x = date, y = BondLP)) + 
  labs(title = "Bonos del Tesoro", subtitle = "Largo Plazo 20Y") + 
  theme_bw() + xlab("Date") + ylab("Tasa de Interés") + 
  scale_color_manual("df", values = c("red","blue","green","orange")) + 
  theme(legend.position = "bottom")
g5

g6 <- ggplot(df) + geom_line(mapping = aes(x = date, y = BondLLP)) + 
  labs(title = "Bonos del Tesoro", subtitle = "Largo Plazo 30Y") + 
  theme_bw() + xlab("Date") + ylab("Tasa de Interés") + 
  scale_color_manual("df", values = c("red","blue","green","orange")) + 
  theme(legend.position = "bottom")
g6
