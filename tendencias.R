data = read.csv("inv_limpio.csv", header = TRUE, sep=" ")
n = length(data[1,])

###
library(ggplot2)
activ <- data.frame()
##Ciclo para variar los l??mites 1-Activos 2-clientes que abandonaron
limite = 6 
activos = numeric()
aa<- numeric()
xt = 1:(n-1)
xl = c(1:12, 1:12, 1:12, 1:4)

for (i in 1:length(data$client)) {
  historico = unlist(data[i,])
  contador = 0
  pos = n
  while (contador < limite & is.na(historico[pos])) {
    contador = contador + 1
    pos = pos - 1
  }
  if (contador < limite & !is.na(historico[pos])) {
    cola = historico[pos]
    while (pos > 0 && !is.na(historico[pos - 1])) {
      pos = pos - 1
      # cat(historico[pos], cola, "\n")
      if (historico[pos] == cola) {
        contador = contador + 1
      } else {
        break
      }
    }
  }
  activos = c(activos, contador >= limite)
}
data$activo = activos
####
Mtodas <- data()
##
medidas = function(m,y,k){
  T = length(y)
  yest = fitted(m)
  sse = sum((yest-y)^2)
  ssr = sum((y-mean(y))^2)
  mse = sse/(T-k)
  R2 = 1 - sse/ssr
  Ra2 = 1 - (T-1)*(1-R2)/(T-k)
  aic = log((T-k)*exp(2*k/T)*mse/T)
  bic = log(T^(k/T)*(T-k)*mse/T)
  M = c(Ra2, mse, aic, bic)
  names(M) = c("R2-ajus","MSE","logAIC","logBIC")
  return(M)}
##
for(i in 1:length(data$client)){
  i<-1
  cl <- t(data[i,2:n])
  y <- na.omit(cl)
  y <- ts(y,frequency=12,start=c(2014,01))
  ##
#  fechas = seq(as.Date("2014/1/1"), length.out = length(y), by = "months")
#  ts.plot(y,main="Inventario de Cliente")
#  np = length(y)
#  ejex.mes = seq(fechas[1],fechas[np], "months")
#  ejex.anno = seq(fechas[1],fechas[np],"years")
#  if(data[i,]$activo==1){color="red"}else{color="green" }
#  filename = paste(data[i,1],"movimiento.png", sep="")
#  png(filename, width=1200, height=1000)
#  plot(fechas,y, xaxt="n", panel.first = grid(),type='l',
#       ylab='Movimiento de Guias',xlab = 'Meses', lwd = 2, main =paste("Cliente ",data[i,1]), col=color )
#  axis.Date(1, at=ejex.mes, format="%m/%y")
#  axis.Date(1, at=ejex.anno, labels = FALSE, tcl = -0.2)
#  graphics.off()
  ##An??lisis##
  
  #Inicio de an??lisis#
  T = length(y)
  yi = y[1:(T-12)]
  yf = y[(T-12+1):T]
  ##Cuatro modelos ##
  t = seq(1:(T-12))
  t2 = t^2
  t3 = t^3
  lyi = log(yi)
  ##
  mod.lin = lm(yi~t)
  mod.cuad = lm(yi~t+t2)
  mod.cub = lm(yi~t+t2+t3)
  mod.llin = lm(lyi~t)
  ###
  summary(mod.lin)
  summary(mod.cuad)
  summary(mod.cub)
  #1
  mod.llin = lm(lyi~t)
  #2
  b0.est = mod.llin$coefficient[1]
  b1.est = mod.llin$coefficient[2]
  #3
  Ds = data.frame(yi,t)
  #4
  mod.exp = nls(yi~exp(beta0+beta1*t), data=Ds, start=list(beta0=b0.est, beta1=b1.est))
  #5
  summary(mod.exp)
  ##Todos##
  M.lin = medidas(mod.lin,yi,2)
  M.cuad = medidas(mod.cuad,yi,3)
  M.cub = medidas(mod.cub,yi,4)
  M.exp = medidas(mod.exp,yi,2)
  M = cbind(M.lin,M.cuad,M.cub,M.exp)
  Mtodas <- rbind(Mtodas,M)
}

#Inicio de an??lisis#
T = length(y)
yi = y[1:(T-12)]
yf = y[(T-12+1):T]
##Cuatro modelos ##
t = seq(1:(T-12))
t2 = t^2
t3 = t^3
lyi = log(yi)
##
mod.lin = lm(yi~t)
mod.cuad = lm(yi~t+t2)
mod.cub = lm(yi~t+t2+t3)
mod.llin = lm(lyi~t)
###
summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)
#1
mod.llin = lm(lyi~t)
#2
b0.est = mod.llin$coefficient[1]
b1.est = mod.llin$coefficient[2]
#3
Ds = data.frame(yi,t)
#4
mod.exp = nls(yi~exp(beta0+beta1*t), data=Ds, start=list(beta0=b0.est, beta1=b1.est))
#5
summary(mod.exp)
##Todos##
medidas = function(m,y,k){
T = length(y)
yest = fitted(m)
sse = sum((yest-y)^2)
ssr = sum((y-mean(y))^2)
mse = sse/(T-k)
R2 = 1 - sse/ssr
Ra2 = 1 - (T-1)*(1-R2)/(T-k)
aic = log((T-k)*exp(2*k/T)*mse/T)
bic = log(T^(k/T)*(T-k)*mse/T)
M = c(Ra2, mse, aic, bic)
names(M) = c("R2-ajus","MSE","logAIC","logBIC")
return(M)}

M.lin = medidas(mod.lin,yi,2)
M.cuad = medidas(mod.cuad,yi,3)
M.cub = medidas(mod.cub,yi,4)
M.exp = medidas(mod.exp,yi,2)
M = cbind(M.lin,M.cuad,M.cub,M.exp)

r = mod.cub$residuals
