# son iguales para data frame
# Se verifica si dos bases son iguales
# se diferencia de una función atravez de un boleano en que soporta 
# elementos de tipo factor
# Esta función verifica hasta posición, por lo que diferentes posiciones
# en los elementos seran detectadas
# al igual que diferentes dimensiones
a=data.frame(x=c(1,7,4,3,"t",1),y=c(1,2,4,3,"c",1))
b=a

a
b

son_iguales=function(base1,base2){
  a=base1
  b=base2
  columnas1=ncol(a)
  columnas2=ncol(b)
  i=1
  filas=nrow(a)
  
  if(columnas1==columnas2 & filas==nrow(b)){
    repeat{
      
    if(sum(b[i,]==a[i,])==columnas1){
      
      i=i+1
      
      if(i>filas){
        print("El proceso se termino")
        print("Son iguales")
        break
      }
      
    }else{
      
      print("Algun elemento es diferente")
      return(sprintf("el proceso se detuvo en la fila %d",i))
      
      break
    }
      
    }
    
  }else{
    return("Los elementos son de diferente dimensiones")
  }
  
}


son_iguales(a,b)
a
b

