#busca devolver el nombre de variables en un regresión lineal si se codfica como abajo

# de esta forma se codifican los nombres

#cambio de nombre para lm()
variables=names(base_Modelar)
variables_aux=names(base_Modelar)
variables[3:25]=paste0("c",as.character(seq2(1,23)))
names(base_Modelar)=variables
remove(variables)


nombres= function(modelo) {
  stopifnot(is.list(modelo) | is.lm(modelo))
  if(!exists("seleccion")){
    message("Se debe llamar la base selección")#aquella que tiene los nombres con sus codigos
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
