leer_api=function(url,direccion_guardar=NULL,s=0){
  s=1+s
  if(try(require("jsonlite"), TRUE)){
    library("jsonlite")
  }else{
    install.packages("jsonlite")
    print("Reiniciando Función")
    if(s>3){
      print("Error Reinicie la función e instale jsonlite")
      return(0)
    }else{
    leer_api(url=url,direccion_guardar = direccion_guardar,s=s)
    return(0)
    }
    library("jsonlite")
  }

  

  
  data <- fromJSON(txt = url)
  json_a_data_frame <- as.data.frame(data)
  
  if(!is.null(direccion_guardar)){
    write.csv(json_a_data_frame,file = direccion_guardar)
  }
    
  return(json_a_data_frame)

}


