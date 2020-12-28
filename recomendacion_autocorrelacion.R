#' Recomendaciones a un grafico ACF o PACF
#'
#' Obten un recomendación a tu ajuste de series de tiempo ACF o PACF
#'
#' @param objeto_cf objeto ACF o PACF
#'
#' @return
#' @export
#'
#' @examples
#' base=data.frame(x=seq(Sys.Date(),by="days",length=20),y=1:20*3+runif(1))
#' recomendacion_autocorrelaciones(acf(base$y,plot = FALSE))
#'
recomendacion_autocorrelaciones <- function(objeto_cf) {
  llamada=match.call()
  ruta=match(c("objeto_cf"),names(llamada))


  if(ruta!=2){#chequeo de que se agregaron bien los parametros
    #stopifnot(ruta!=2)
    message("Objeto_cf no encontrado o hay más de un parametro en la función")
    stop()
  }

  print(class(p))
  print(llamada)
}


#Zona de cuidado
a=llamada[[2]]
a=as.character(a)
a
#si la salida es un vector de 3 elementos entonces hay dos parametros

llamada[[2]]
lm()
