#indicadora

a=matrix(ifelse(runif(15)<0.3,1,0),ncol = 3)
a=base2[,c(34,35,36)]


indicador=function(columnas){ #base que indica es columnas
a=columnas
total=rowSums(a)
total
columna_indicadora=ifelse(total >0,1,0)

return(columna_indicadora)
}
