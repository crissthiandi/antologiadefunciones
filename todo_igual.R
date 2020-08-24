#función que verifica si dos elementos R son iguales numericamente

todo_igual=function(a,b,x_columnas=T,solo_valores=T,identico=F,columnas_dif=F,mismo_orden=T){
  #si no es por columnas se hace por filas
  
  if(x_columnas==T){
    #checamos diferencia de colunas
    numero_columnas=ncol(a)
    aux=ncol(b)
    tab="     "
    
    if(ifelse(is.null(aux),yes = 0,no = aux)!=ifelse(is.null(numero_columnas),0,numero_columnas)){
      stop("Numero de columnas diferentes")
    }
    
    for (i in numero_columnas) {
      resultado=identical(a[,i],b[,i])
      message(paste0(tab,sprintf("La columna %d identicidad= %c",i,resultado)))
    }
      sprintf("    Hola2")
    
    
  }
}

todo_igual(c(2),c(3))
