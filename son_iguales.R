# son iguales para vectores
# Se verifica si dos bases son iguales
# se diferencia de una función atravez de un boleano en que soporta 
# elementos de tipo factor
# Esta función verifica hasta posición, por lo que diferentes posiciones
# en los elementos seran detectadas
# al igual que diferentes dimensiones

  #en chequeo de si aun aplica
    # a=data.frame(x=c(1,7,4,3,"t",1),y=c(1,2,4,3,"c",1))
    # b=a
    # 
    # a
    # b

son_iguales=function(vector1,vector2){
  a=vector1
  b=vector2
  columnas1=length(a)
  columnas2=length(b)
  i=1
  #cuenta errores
  num_error=0
  
  if(columnas1==columnas2){
    repeat{
      
    if(ifelse(is.na(b[i]==a[i]),F,b[i]==a[i]) | (is.na(b[i]) & is.na(a[i])) ){
      
      i=i+1
      
      if(i>columnas1){
        print("El proceso se termino")
        if(num_error==0){print("Son iguales")}
        print(sprintf("    Hay en total %d errores",num_error))
        break
      }
      
    }else{
      
      print("Algun elemento es diferente")
      print(sprintf("checa la fila %d",i))
      i=1+i
      num_error=num_error+1
    }
      
    }
    
  }else{
    return("Los elementos son de diferente dimensiones")
  }
  
}


# son_iguales(target,current)
# a=target
# b=current


