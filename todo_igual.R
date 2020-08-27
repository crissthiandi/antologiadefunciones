#función que verifica si dos elementos R son iguales numericamente

todo_igual=function(a,b,x_columnas=T,solo_valores=T,identico=F,columnas_dif=F,mismo_orden=T){
  #si no es por columnas se hace por filas
  
  if(x_columnas==T){
    #checamos diferencia de colunas
    numero_columnas=ncol(a)
    aux=ncol(b)
    #nombre de las variables por si algo sale mal
    nombres=names(a)
    tab="     "
    #verifica si son de la misma clase
    if(!is.data.frame(a) | !is.data.frame(b)){
      stop("algun elemento no es data frame")
    }
    #si son diferente tamaño bye
    if(aux!=numero_columnas){
      stop("Numero de columnas diferentes")
    }
    
    #ciclo de revisión
    i=1
    repeat{
      resultado=identical(a[,i],b[,i])
      # Sys.sleep(1)
      #id del posible diferencia
      message(sprintf("La columna %d identicidad= %s",i,resultado))
      #si diferencia, muestra cual es
      if(!resultado){
        #nombre de la columna diferente y diferencias [impresión]
        diferencia=all.equal(a[,i],b[,i])
        message(paste0(tab,sprintf("La columna %s es diferente \n %s Posible diferencia= %s",nombres[i],tab,diferencia)))
      }
      #conteo del ciclo
      i=i+1
      #codificion de final del ciclo, si es mayor a 500 ciclos, break!
      if(i>numero_columnas | i>500L){
        break
      }
    }
  }
}

#testeo
# todo_igual(Nacional,Nacional1[,-11])
