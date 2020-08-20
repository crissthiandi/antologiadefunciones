## El siguiente codigo verifica la existencia de phantom


#guarda la imagen

guardar_imagen= function(widget){
  
  #verifica installacion de htmlwidgets
  if(require('htmlwidgets')){
    if(require('webshot')){
      if(!webshot:::is_phantomjs_installed()){
        webshot:::install_phantomjs()
      }
      
      if(require(stringr)){
        check=dir.exists(paste0(getwd(),"/widgets/graficas_",as.character(Sys.Date())))
        if(!check){
          dir.create(file.path(getwd(),"widgets",
                               stringr:::str_c("graficas_", stringr:::str_replace_all(
                                 Sys.Date(),"-", "-"))))
        }
        
        ruta<- paste0(getwd(), 
                    stringr:::str_c("/widgets/graficas_", 
                        stringr:::str_replace_all(Sys.Date(), "-", "-"), "/"))
        
        htmlwidgets:::saveWidget(widget, file=paste0(getwd(), "/widgets.html"))
        
        webshot:::webshot("widgets.html",paste0(ruta,
                    stringr:::str_c("widget_", stringr:::str_replace_all(
                        Sys.Date(),"-", "-"),".png")),vwidth = 1200,vheight = 768)
        
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

