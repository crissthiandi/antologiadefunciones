#extracción de id

#función que extrae el id de un link generico de google drive,
#se probo en links obtenidos de compartir con todo publico con el link
getidgoogle= function(link) {
  a=regexpr("/d/",link)
  a=as.integer(a)
  b=regexpr("/view",link)
  b=as.integer(b)
  id=substring(link,a+3,b-1)
  return(id)
}

#leer desde internet
#esta función lee un link, usa la función getidgoogle() para extraer
#la id para su descarga

getcsv_drive= function(link){
  id=getidgoogle(link = link)
  #descarga el objeto con ayuda de un link de desgarga que 
  #requiere de la id del documentos, usando sprintf()
  a=read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
  #regresa el objeto descargado
  return(a)
}

