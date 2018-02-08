library(ggplot2)
data = read.csv("inv_limpio.csv", header = TRUE, sep=" ")
n = length(data[1,])
activ <- data.frame()

##Ciclo para variar los l??mites 1-Activos 2-clientes que abandonaron
#for(a in 1:9){
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
#aa <-c(limite,sum(data$activo),i-sum(data$activo))
#activ <- rbind(activ,aa)

###Ejemplo visual###
#main = paste("Clientes en ",limite," meses (5 Clientes)", sep=" ")
#filename = paste(limite,"mesesEjemplo.png", sep="")
#png(filename, width=1200, height=1000)
#if(data[1,]$activo==1){color="dark red"}else{color="dark green" }
#plot(xt, data[1,2:n], type="s", xaxt="n", lwd=3,
#     xlab="From 2014 to present", ylab="Client's inventory", col=color)
#for (i in 2:5){
#  color = if(data[i,]$activo==1){color="dark red"}else{color="dark green" }
#  points(xt, data[i,2:n], type="s", lwd=3, col=color)
#}
#axis(side=1, at=1:(n-1), labels=xl)
#graphics.off()
###FinEjemplo###
#}
#colnames(activ)<- c("meses","activos","noactivos")
#library(reshape2)
#library(plyr)
#library(geom_bar)
#xlong <- ddply(melt(activ, id.vars = 'meses'), .(meses), mutate, prop = value / sum(value))
#png("mesesPorciento.png", width=1200, height=1000)
#ggplot(xlong, aes(x= meses, y = prop, fill = variable)) + geom_bar(stat = 'identity') + xlab("N??mero de meses sin actividad") + ylab("Porcentaje de Activos/No Activos") + theme(axis.text=element_text(size=16),
#        axis.title=element_text(size=18,face="bold"),legend.title = element_text(size=18,face="bold"),legend.text = element_text(size=20))+ labs(fill = "Clientes:")
#graphics.off()


#####Gr??fico con todas las series
#xt = 1:(n-1)
#xl = c(1:12, 1:12, 1:12, 1:4)
#for (i in 1:length(data$client)) {
#  main = paste("Client", data[i,]$client, sep=" ")
#  filename = paste(data[i,]$client, ".png", sep="")
#  png(filename, width=1200, height=1000)
#  if(data[i,]$activo==1){
#    plot(xt, data[i,2:n], type="s", xaxt="n", lwd=3,
#         xlab="From 2014 to present", ylab="Client's inventory", col="red")
#    axis(side=1, at=1:(n-1), labels=xl)
#  }else{
#    plot(xt, data[i,2:n], type="s", xaxt="n", lwd=3,
#         xlab="From 2014 to present", ylab="Client's inventory", col="green")
#    axis(side=1, at=1:(n-1), labels=xl)
#  }
#  graphics.off()
#}
##### Final de imagenes, permanecen en comentario para no generarlas