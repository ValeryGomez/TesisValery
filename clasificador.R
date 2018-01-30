data = read.csv("inv_limpio.csv", header = TRUE, sep=" ")
n = length(data[1,])
limite = 6 
activos = numeric()
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

##Graficos de activos##
xt = 1:(n-1)
xl = c(1:12, 1:12, 1:12, 1:4)
for (i in 1:k) {
  main = paste("Client", data[i,]$client, sep=" ")
  filename = paste(data[i,]$client, ".png", sep="")
  png(filename, width=1200, height=1000)
  if(data[i,]$activo==1){
    plot(xt, data[i,2:n], type="s", xaxt="n", lwd=3,
         xlab="From 2014 to present", ylab="Client's inventory", col="dark red")
    axis(side=1, at=1:(n-1), labels=xl)
  }else{
    plot(xt, data[i,2:n], type="s", xaxt="n", lwd=3,
         xlab="From 2014 to present", ylab="Client's inventory", col="dark green")
    axis(side=1, at=1:(n-1), labels=xl)
  }
  graphics.off()
}
