
tabla=function(columna,grupo=base2,si=1,no=0,nose=NULL,titulo_nose=NULL,titulo_si="Si",
               titulo_no="no",otros=NULL,titulos_otros=NULL,rowotros=NULL,rownose=" ",
               nombre=NULL){
  
  
  if(is.null(nombre)){nombre=names(grupo[,columna])}
  
  if(is.vector(no)| is.vector(nose) | is.vector(si)){grupo=as.data.frame(grupo)}
  
  if(is.vector(no)){
    tam=length(no)
    vec=grupo[,columna]
    for (h in 2:tam) {
      vec=replace(vec,vec==no[h],no[1])
    }
    grupo[,columna]=vec
    grupo=as_tibble(grupo)
    no=no[1]
  }
  if(is.vector(nose)){
    tam=length(nose)
    vec=grupo[,columna]
    for (h in 2:tam) {
      vec=replace(vec,vec==nose[h],nose[1])
    }
    grupo[,columna]=vec
    grupo=as_tibble(grupo)
    nose=nose[1]
  }
  if(is.vector(si)){
    tam=length(si)
    vec=grupo[,columna]
    for (h in 2:tam) {
      vec=replace(vec,vec==si[h],si[1])
    }
    grupo[,columna]=vec
    grupo=as_tibble(grupo)
    si=si[1]
  }
  
  
  #taller
  tabla_taller=table(subset(x=grupo,subset = Asistio_Talleres_Iniciativa ==1, select = columna ))
  #sin taller
  tabla_sin_taller=table(subset(x=grupo,subset = Asistio_Talleres_Iniciativa ==0, select = columna ))
  
  j=c(si,no,nose,otros)
  total=length(j)
  tall=rep(0,total)
  for (i in 1:total) {
    tall[i]=ifelse(length(which(names(tabla_taller)==j[i]))>0,{
      a=which(names(tabla_taller)==j[i])
      tabla_taller[a][[1]]
    },0)
  }
  
  sin_tall=rep(0,total)
  j=c(si,no,nose,otros)
  for (i in 1:total) {
    sin_tall[i]=ifelse(length(which(names(tabla_sin_taller)==j[i]))>0,{
      a=which(names(tabla_sin_taller)==j[i])
      tabla_sin_taller[a][[1]]
    },0)
  }
  
  if(!is.null(otros)){rowotros=make.names(rep("",length(otros)),unique = T)}
  if(is.null(nose)){rownose=titulo_nose=nose}

  
  elemento=data.frame(row.names =c("",nombre,rownose,rowotros),
                      estatus=c(titulo_si,titulo_no,titulo_nose,titulos_otros),
                      Con_taller = tall, Sin_taller = sin_tall)
  return(elemento)
}


library("printr")
