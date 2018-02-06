data = read.csv("inv_limpio.csv", header = TRUE, sep=" ")
n = length(data[1,])

for(i in 1:length(data$client)){
  i<-1
  cl <- data[i,6:n]
  y = na.omit(cl)
  y <- ts(y,frequency=12,start=c(2014,05))
  ##
  fechas = seq(as.Date("2014/1/1"), length.out = length(y), by = "months")
  ts.plot(y,main="Inventario de Cliente")
  np = length(y)
  ejex.mes = seq(fechas[1],fechas[np], "months")
  ejex.anno = seq(fechas[1],fechas[np],"years")
}