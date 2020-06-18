# son iguales for data frame
a=data.frame(x=c(1,7,4,3,"c",1),y=c(1,2,4,3,"c",1))
b=a

a
b

son_iguales=function(a,b){
  columnas1=ncol(a)
  columnas2=ncol(b)
  i=1
  filas=nrow(a)
  
  if(columnas1==columnas2 & filas==nrow(b)){
    repeat{
      
    if(sum(b[,i]==a[,i])==filas){
      
      i=i+1
      
      if(i>columnas1){
        print("El proceso se termino")
        print("Son iguales")
        break
      }
      
    }else{
      
      print("Algun elemento es diferente")
      return(sprintf("el proceso se detuvo en la columna %d",i))
      
      break
    }
      
    }
    
  }else{
    return("Los elementos son de diferente dimensiones")
  }
  
}


son_iguales(a,b)
