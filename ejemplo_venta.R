library(xtable)

D = read.table("ventas_al_menudeo.dat",header=T)
attach(D) # utiliza el nombre de las columnas como variables
y = na.omit(RTRR)/10000
y = ts(y,frequency=12,start=c(1955,01))
fechas = seq(as.Date("1955/1/1"), length.out = length(y), by = "months")
ts.plot(y,main="Ventas al menudeo en UDS de 1999")
np = length(y)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.anno = seq(fechas[1],fechas[np],"years")

plot(fechas,y, xaxt="n", panel.first = grid(),type='l',
     ylab='ventas.mes', lwd = 2)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.anno, labels = FALSE, tcl = -0.2)
