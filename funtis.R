#Inserta un data.frame() o tybble y obten una tabla de frecuencias 
# de los valores para cierta columna
# permite agrupar valores al insertar un vector en la entrada

#La salida es similar a la función table() solo que el objetos salida es un
#data.frame()
#y se puede hacer la agrupación por valores

#Ejemplo

# Considere una base como la siguiente:
#
#   base=
#         nombres edad tiene-casa?
#         Samuel  15    No
#         Sabrina 19    Si
#         Cris    22    No-se
#         Saul    77    Yes
#
# Si usamos tabla(3,base,"Si","No",)
#

tabla=function(columna,grupo=base2,filtro1=1,filtro2=0,filtro3=NULL,
               titulo_3=NULL,titulo_1="Filtro 1",titulo_2="Filtro 2",
               otros_filtros=NULL,titulos_otros=NULL,rowotros=NULL,row3=" ",
               nombre=NULL){
  
  
  if(is.null(nombre)){nombre=names(grupo[,columna])}
  
  if(is.vector(filtro2)| is.vector(filtro3) | is.vector(filtro1))
    {grupo=as.data.frame(grupo)}
  
  if(is.vector(filtro2)){
    tam=length(filtro2)
    vec=grupo[,columna]
    for (h in 2:tam) {
      vec=replace(vec,vec==filtro2[h],filtro2[1])
    }
    grupo[,columna]=vec
    grupo=dplyr::as_tibble(grupo)
    filtro2=filtro2[1]
  }
  if(is.vector(filtro3)){
    tam=length(filtro3)
    vec=grupo[,columna]
    for (h in 2:tam) {
      vec=replace(vec,vec==filtro3[h],filtro3[1])
    }
    grupo[,columna]=vec
    grupo=dplyr::as_tibble(grupo)
    filtro3=filtro3[1]
  }
  if(is.vector(filtro1)){
    tam=length(filtro1)
    vec=grupo[,columna]
    for (h in 2:tam) {
      vec=replace(vec,vec==filtro1[h],filtro1[1])
    }
    grupo[,columna]=vec
    grupo=dplyr::as_tibble(grupo)
    filtro1=filtro1[1]
  }
  
  
  #tabla
   tabla_=table(subset(x=grupo,
                             select = columna ))
   
  # tabla_taller=table(subset(x=grupo,
  #                           subset = Asistio_Talleres_Iniciativa ==1, 
  #                           select = columna ))
  #sin taller
  # tabla_sin_taller=table(subset(x=grupo,
  #                               subset = Asistio_Talleres_Iniciativa ==0, 
  #                               select = columna ))
  
  
  
  j=c(filtro1,filtro2,filtro3,otros_filtros)
  total=length(j)
  tall=rep(0,total)
  for (i in 1:total) {
    tall[i]=ifelse(length(which(names(tabla_)==j[i]))>0,{
      a=which(names(tabla_)==j[i])
      tabla_[a][[1]]
    },0)
  }
  
  
  if(!is.null(otros_filtros)){rowotros=make.names(rep("",length(otros_filtros)),
                                          unique = T)}
  if(is.null(filtro3)){row3=titulo_3=filtro3}

  
  # elemento=data.frame(row.names =c("",nombre,rownose,rowotros),
  #                     estatus=c(titulo_si,titulo_no,titulo_nose,
  #                               titulos_otros),
  #                     Con_taller = tall, Sin_taller = sin_tall)
  
  if(is.null(nombre)){nombre=paste("Columna",columna)}
  
  elemento=data.frame(row.names =c("",nombre,row3,rowotros),
                      Filtro=c(titulo_1,titulo_2,titulo_3,
                                titulos_otros),
                      Frecuencia = tall)
  return(elemento)
}


library("printr")
