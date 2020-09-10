#estafunción modifica los valores de una función

cambia_niveles=function(vector,niveles,mayusculas=F){
  aux=vector
  #modifica el vector
  aux=as.factor(aux)
  print(levels(aux))
  salir=readline("¿Desea salir por cuestiones de nivel? \n True \n False \n")
  
  if(salir){
    return(NULL)
  }
  
  #asigna los nuevos niveles
  if(mayusculas){
    levels(aux)=stringr::str_to_upper(levels(aux))
  }else{
    levels(aux)=niveles
  }
  
  return(aux)
}

#testing
# xlm=cambia_niveles(mtcars$cyl,c("seis","cuatro","ocho"))
# xlm