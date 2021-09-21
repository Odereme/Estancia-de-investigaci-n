library(quantmod)
library(xts)
library(lubridate)
library(timeDate)
library(survival)
library(TSA)
library(tseries)
#------------Defining dates 
  
start.d <- as.Date("2017-09-13")
end.d <- as.Date("2020-06-09")


#--------------loading data 

BTC_vsIndx <- c("BTC-USD","^GSPC","^DJI", "^SP400") #error with S&P 400!

getSymbols(BTC_vsIndx, src = "yahoo", from=start.d, to=end.d)

BTC <- `BTC-USD` #replacing for intuitive name

#---------verifying missing values

summary(is.na(BTC))
rows.na <- which(is.na(BTC))

#isWeekday(BTC)

summary(is.na(GSPC)) #no NA´s 

#----------Visualizing time series

plot(BTC$`BTC-USD.Close`, main="Precio histórico del BTC en USD", ylab="Close Price")

plot(GSPC$GSPC.Close, main="Precio histórico del S&P 500", ylab="Close Price")

plot(DJI$DJI.Close, main="Precio histórico del S&P 500", ylab="Close Price")



#----------creating data frame

BTC_df <- as.data.frame(BTC)
SP_df <- as.data.frame(GSPC)

BTC_df <- BTC_df["BTC-USD.Close"]
SP_df <- SP_df["GSPC.Close"]
SP_df

BTC_df$Date <- row.names(BTC_df)
row.names(BTC_df) <- NULL
colnames(BTC_df) <- c("Close", "Date")

SP_df$Date <- row.names(SP_df)
row.names(SP_df) <- NULL
colnames(SP_df) <- c("Close", "Date")

BTC_df
SP_df 

BTC_SP_df <- merge(SP_df,BTC_df, by = "Date")
colnames(BTC_SP_df) <- c("Date","S&P","BTC")
class(BTC_SP_df)
setwd("D:\\ESCUELA\\Investigacion")
write.csv(BTC_SP_df, "BTC_SP_df.csv")

#----------paired data plot

plot(BTC_SP_df$BTC, BTC_SP_df$`S&P`, main="Gráfico de dispersión de S&P vs BTC", ylab="S&P 500", xlab = "BTC")

BTC_SP<-read.csv("D:\\ESCUELA\\Investigacion\\BTC_SP_df.csv")
class(BTC_SP)
BTC_SP$Date<-as.Date(BTC_SP$Date)

SPts<-xts(BTC_SP$S.P,BTC_SP$Date)
BTCts<-na.omit(xts(BTC_SP$BTC,BTC_SP$Date))

#TENDENCIA LINEAL
#SP500
plot(BTC_SP$S.P~BTC_SP$Date,main="Precio historico del S&P 500",xlab="tiempo",ylab="Precio de cierre")
tls<-lm(BTC_SP$S.P~BTC_SP$Date, data=BTC_SP)
summary(tls)
abline(tls,col="red")

#p-valor<.05 por lo tanto R Ho entonces  tiene tendencia LINEAL
#por lo tanto NO es estacionaria en media

#TENDENCIA CUBICA
tcs<-lm(BTC_SP$S.P~poly(BTC_SP$Date,degree=3))
summary(tcs) 
lines(BTC_SP$Date,fitted.values(tcs), col="magenta")
#p-valor<.05 por lo tanto R Ho entonces  tiene tendencia CUBICA
#por lo tanto NO es estacionaria en media

#varianza
adf.test(BTC_SP$S.P, alternative = "stationary")
#p-valor=.088>.05 
#por lo tanto NO es estacionaria en varianza


#TENDENCIA LINEAL
#BTC
plot(BTC_SP$BTC~BTC_SP$Date,main="Precio historico del BTC",xlab="tiempo",ylab="Precio de cierre")
tlb<-lm(BTC_SP$BTC~BTC_SP$Date, data=BTC_SP)
summary(tlb)
abline(tlb,col="red")

#p-valor>.05 por lo tanto NO R Ho entonces  tiene NO tendencia LINEAL
#por lo tanto es estacionaria en media

#TENDENCIA CUBICA
tcb<-lm(BTC_SP$BTC~poly(BTC_SP$Date,degree=3))
summary(tcb) 
lines(time(BTCts),fitted.values(tcb), col="magenta")
#p-valor<.05 por lo tanto R Ho entonces  tiene tendencia CUBICA
#por lo tanto NO es estacionaria en media

#varianza
adf.test(na.omit(BTC_SP$BTC), alternative = "stationary")
#p-valor=.255>.05 
#por lo tanto NO es estacionaria en varianza

#Clasificando la serie como NO ESTACIONARIA (SP)


diff(SPts) #restas X_n-X_(n-1)
n.SPts<-diff(log(SPts))
plot(n.SPts)
n.SPts

#Verificando la eficiencia de la correccion (SP)
#media 
summary(lm(n.SPts~time(n.SPts)))
#La serie corregida no muestra tendencia lineal
#estacionaria en media

#Varianza
adf.test(na.omit(n.SPts),alternative="stationary")
#Serie estacionaria en varianza

#La serie es clasificada como estacionaria

#Clasificando la serie como NO ESTACIONARIA (BTC)

diff(BTCts) #restas X_n-X_(n-1)
n.BTCts<-diff(log(BTCts))
plot(n.BTCts)
n.BTCts

#Verificando la eficiencia de la correccion (BTC)
#media 
summary(lm(n.BTCts~time(n.BTCts)))
#La serie corregida no muestra tendencia lineal
#estacionaria en media

#Varianza
adf.test(na.omit(n.BTCts),alternative="stationary")
#Serie estacionaria en varianza

#La serie es clasificada como estacionaria


#Modelado de serie n.SPts
#Mejor modelo AR para n.SPts------------------------------------------
arr<-ar(na.omit(n.SPts))
arr
#Mejor modelo MA para n.SPts------------------------------------------


aux<-Inf
for(i in 1:10)
{
  x<-AIC(arima(n.SPts,order=c(0,0,i), method = "ML"))
  
  if(x<aux)
  {
    aux<-x
    order.ma<-i
  }
  else{
    
  }
  
  
}
aux #muestra el aic del mejor modelo
order.ma #muestra el orden del mejor modelo


#Mejor modelo ARMA para n.SPts-------------------------------------------
aux2<-Inf
for(j in 1:10)
{
  for(i in 1:10)
  {
    x<-AIC(arima(n.SPts,order=c(j,0,i), method = "ML"))
    
    if(x<aux2)
    {
      aux2<-x
      order.arma<-c(j,i)
    }
    
  }
}
aux2 #muestra el aic del mejor modelo ARMA
order.arma #muestra el orden del mejor modelo ARMA

#Modelado de serie n.BTCts
#Mejor modelo AR para n.BTCts------------------------------------------
arr<-ar(na.omit(n.BTCts))
arr
#Mejor modelo MA para n.BTCts------------------------------------------


aux<-Inf
for(i in 1:10)
{
  x<-AIC(arima(n.BTCts,order=c(0,0,i), method = "ML"))
  
  if(x<aux)
  {
    aux<-x
    order.ma<-i
  }
  else{
    
  }
  
  
}
aux #muestra el aic del mejor modelo
order.ma #muestra el orden del mejor modelo


#Mejor modelo ARMA para n.BTCts-------------------------------------------
aux2<-Inf
for(j in 1:10)
{
  for(i in 1:10)
  {
    x<-AIC(arima(n.BTCts,order=c(j,0,i), method = "ML"))
    
    if(x<aux2)
    {
      aux2<-x
      order.arma<-c(j,i)
    }
    
  }
}
aux2 #muestra el aic del mejor modelo ARMA
order.arma #muestra el orden del mejor modelo ARMA
