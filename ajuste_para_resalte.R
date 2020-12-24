
lista = list(color="black",fill="#ff8c00")


librerias=c("dplyr","rlang","ggplot2","gghighlight")

sapply(librerias,require, character.only = TRUE)


#función hermosa 
seleccion_filtro=function(...,datos,color,limitesx=c(-Inf,Inf),limitesy=c(-Inf,Inf),alfa=0.1){

  predicates <- enquos(...)
  check_bad_predicates(predicates)
  
  filtro=datos %>% filter(...)
  
  
  return(geom_rect(data=filtro,aes(xmin=limitesx[1],xmax=limitesx[2],ymin=limitesy[1], ymax=limitesy[2]),
            fill=color, alpha=alfa))
}

# testeo de la función de arriba ----
# ggplot()+
#   geom_line(data = mpg2,aes(y=displ,x=cty,colour=class),size=1)+
#   seleccion_filtro(manufacturer=="jeep",datos = mpg2,color = 'red')+
#   theme_classic()+
#   facet_wrap(vars(manufacturer))


check_bad_predicates <- function(x) {
  have_name_idx <- have_name(x)
  
  if (any(have_name_idx)) {
    bad_x <- x[have_name_idx]
    bad_x_deparsed <- purrr::imap_chr(
      bad_x,
      ~ paste(.y, "=", quo_text(.x))
    )
    
    abort(
      sprintf(
        "Use ya sea `==`?: %s",
        paste(bad_x_deparsed, collapse = ",")
      )
    )
  }
}


  
  