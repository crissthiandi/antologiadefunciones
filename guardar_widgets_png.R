## El siguiente codigo verifica la existencia de phantom


#guarda la imagen

#dirección sera la forma en la que se le asigna una nueva sub ruta para 
#guardar los png

guardar_imagen= function(widget,direccion=NULL){
  
  #verifica installacion de htmlwidgets
  if(require('htmlwidgets')){
  #verifica installacion de webshot
    if(require('webshot')){
      if(!webshot:::is_phantomjs_installed()){
        #phantonjs es una libreria que se installa con ayuda de webshot
        webshot:::install_phantomjs()
      }
      #necesita stringr para str_c and str_remplace_all
      if(require(stringr)){
        #se checa que dirección inicie con un "/"
        if(!is.null(direccion)){
          #se anida porque no se deberia aplicar la función a algo NULL
          #nos ahorramos warning o mensajes
          if(!stringr::str_starts(direccion,pattern ="/" )){
            #se completa la dirección
            direccion=paste0("/",direccion)
          }
        }
        #verifica si existe la dirección donde guaradara los png, booleana
        check=dir.exists(paste0(getwd(),direccion,"/widgets/graficas_",
                                as.character(Sys.Date())))
        if(!check){#si no existe se crea la dirección
          dir.create(file.path(getwd(),"widgets",
                               stringr:::str_c("graficas_", stringr:::str_replace_all(
                                 Sys.Date(),"-", "-"))))#la dirección se nombra en base al dia de uso
        }
        
        ruta<- paste0(getwd(), #ruta considerada desde la raiz
                    stringr:::str_c("/widgets/graficas_",
                        stringr:::str_replace_all(Sys.Date(), "-", "-"), "/"))
        
        htmlwidgets:::saveWidget(widget, file=paste0(getwd(), "/widgets.html"))
        
        webshot:::webshot("widgets.html",paste0(ruta,
                    stringr:::str_c("widget_", stringr:::str_replace_all(
                        Sys.Date(),"-", "-"),".png")),vwidth = 1200,vheight = 768)
        
        #se verifica si se puede borrar un archivo, si se puede se borra
        if(!tryCatch(expr = file.remove("widgets.html"))){
          message("Este error es insignificante, puede ser ignorado")
        }
        message("La imagen se guardo en el directorio de trabajo")
      } else {
        warning("se intentara instalar Stringr ")
        install.packages('stringr')  
      }      
    } else {
      warning("se intentara instalar webshot ")
      install.packages('webshot')
    }
    
  } else {
    warning("se intentara instalar htmlwidgets ")
    install.packages('htmlwidgets')
  }
}

