#indicadora
#nos indica si un conjunto de columnas tienen el mismo valor
#por defecto 0 y 1
#la función nos marca 1 o 0 si las fila del conjunto de columnas 
# tiene por lo menos un 1

#ejemplo, sea la siguiente matriz
# a=matrix(ifelse(runif(15)<0.3,1,0),ncol = 3)
# a

#funciona mejor en dataframes con valores dicatomicos 0 1

indicador=function(base,valor_indicador=1){
a=base
total=rowSums(a==valor_indicador)
total
indicador=ifelse(total >0,1,0)

a=cbind(a,indicador)

return(a)
}

base=data.frame(nombre=c("vero","cris","adro","sabri"),tiene_casa=c(1,0,1,0)
                ,tiene_pares=c(1,0,0,0),Come_diario=c(0,0,1,1))

base=data.frame(nombre=c("vero","cris","adro","sabri"),
                tiene_casa=c("si","no","si","no")
                ,tiene_pares=c("si","no","no","no"),
                Come_diario=c("no","no","si","si"))

base
indicador(base,valor_indicador = "si")
a
