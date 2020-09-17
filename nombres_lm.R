#busca devolver el nombre de variables en un regresión lineal si se codfica como abajo

# de esta forma se codifican los nombres

#cambio de nombre para lm()

  # variables=names(base_Modelar)
  # variables_aux=names(base_Modelar)
  # variables[3:25]=paste0("c",as.character(seq2(1,23)))
  # names(base_Modelar)=variables
  # remove(variables)


nombres_lm= function(modelo) {
  stopifnot(is.list(modelo) | is.lm(modelo))
  if(!exists("seleccion")){
    message("Se debe llamar la base selección")#aquella que tiene los nombres con sus codigos
  }

  coeficientes=names(modelo[["coefficients"]])
  filtro=str_starts(coeficientes,"c")
  
  coeficientes=coeficientes[filtro]
  a=as.character(modelo[["call"]][["data"]])
  nombres=names(get(a))
  
  for (i in 1:length(coeficientes)) {
    #encuentra la primera posición
    posicion=grep(coeficientes[i],nombres)[1]
    #condicional para numeros, base seleccion
    if(posicion+2<16){
      cat(coeficientes[i],": ",variables_aux[posicion+2],"\n")
    }else{
      #busca en seleccion base
      newpos=grep(variables_aux[posicion+2],seleccion$id_indicador)[1]
      #imprime en seleccion base
      cat(coeficientes[i],": ",seleccion$select[newpos],"\n")
    }
  }
}

is.lm = function(object){
  clase=class(object)
  if(clase=="lm"){
    return(T)
  }else{
    return(F)
  }
}
