#estafunción modifica los valores de una función

cambia_niveles=function(vector,niveles){
  aux=vector
  #modifica el vector
  aux=as.factor(aux)
  
  #asigna los nuevos niveles
  levels(aux)=niveles
  
  return(aux)
}

