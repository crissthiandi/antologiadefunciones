#Codigo hecho y en mantenimiento por @crissthiandi <albertocenaa@gmail.com>
#última versión en github.com/crissthiandi/antologiadefunciones


#' Descomposición de series de tiempo by any smooth
#'
#' Descompone tu serie por estacionalidad, tendencia y ruido. Personaliza el suavizamiento a usar, por defecto \code{\link{loess}} smooth
#'
#' Similar a \code{\link{decompose}} el modelo aditivo es de la forma
#' \t $Y[t] = T[t]+S[t]+e[t]$
#'
#' @param time_serie Un objeto series de tiempo
#' @param type Tipo de estacionalidad.
#' @param smooth Cual suavizamiento se usara para calcular la tendencia loess por default
#'
#' @return
#' @export
#'
#' @examples
#' #Graficando la descomposición
#' plot(decompose_ts(AirPassengers))
#'
#'
decompose_ts <- function(time_serie,type=c("additive", "multiplicative"),
                         smooth=c("loess","promedio_movil","lineal")) {
  x <- time_serie
  type <- match.arg(type)
  smooth <- match.arg(smooth)
  l <- length(x)
  f <- frequency(x)
  if (f <= 1 || length(na.omit(x)) < 2 * f){
    stop("time series has no or less than 2 periods")
  }

  if(smooth=="loess"){
    a=data.frame(valor=x,fila=1:length(x))
    #a
    objeto=loess(valor~fila,data = a)
    #plot(predict(objeto,data.frame(fila =a$fila)),type = "l",col="blue")
    #lines(valor~fila,data = a)

    trend <- predict(objeto,data.frame(fila =a$fila))

  }else{
    if(smooth=="promedio_movil"){
        filter <- if(!f%%2){
          c(0.5, rep_len(1, f - 1), 0.5)/f
        }
      trend <- filter(x, filter)
    }else{
    stopifnot(smooth=="lineal")
    a=data.frame(valor=x,fila=1:length(x))
    lineal=lm(valor~fila,data = a)

    trend <- predict(lineal,data.frame(fila =a$fila))

  }
  }
  season <- if (type == "additive")
    x - trend
  else x/trend
  periods <- l%/%f
  index <- seq.int(1L, l, by = f) - 1L
  figure <- numeric(f)
  for (i in 1L:f) figure[i] <- mean(season[index + i], na.rm = TRUE)
  figure <- if (type == "additive")
    figure - mean(figure)
  else figure/mean(figure)
  seasonal <- ts(rep(figure, periods + 1)[seq_len(l)], start = start(x),
                 frequency = f)
  structure(list(x = x, seasonal = seasonal, trend = trend,
                 random = if (type == "additive") x - seasonal - trend else x/seasonal/trend,
                 figure = figure, type = type), class = "decomposed.ts")
}
pp=AirPassengers
autoplot(decompose_ts(pp,smooth = "promedio_movil"))
autoplot(decompose_ts(pp)) #loess metodo
autoplot(decompose_ts(pp,smooth = "lineal"))


