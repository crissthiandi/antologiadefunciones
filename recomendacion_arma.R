#Codigo hecho y en mantenimiento por @crissthiandi <albertocenaa@gmail.com>
#última versión en github.com/crissthiandi/antologiadefunciones

#' Recomendación de modelo ARMA
#'
#' Usando una matriz eacf de TSA paqueteria se propone un posible vector con los valores de p y q de un modelos ARMA(p,q)
#'
#' Se utiliza un metodo de busqueda de esquinas para proponer el valor de
#' p,q para ARMA(p,q).
#'
#' @param time_series Objeto Serie de tiempo
#' @param print_matrix Indicador, imprimir o no matriz de eacf
#'
#' @return Vector de longitud 2, primera entrada valor de \bold{p}, segunda valor de \bold{q}
#' @export
#'
#' @examples
#' recomendaciones_arma(AirPassengers)
recomendaciones_arma <- function(time_series,print_matrix=TRUE) {
  x=time_series

  modelo_arma <- matriz_eacf(x,7,7,print_matrix)

  matriz_true_false <- modelo_arma$symbol=="o"
  matriz_true_false=matriz_true_false[-1,][,-1]

  for (i in 1:7) {
    if(sum(matriz_true_false[,i])>0){
      for (j in 1:7) {
        zz=matriz_true_false[,i]
        if(zz[j]==1){
          #se analizan vecinos
          izquierda=matriz_true_false[j,i+1]
          abajo=matriz_true_false[j+1,i]
          diagonal=matriz_true_false[j+1,i+1]
          #condicion algun vecino o diagonal no null
          if(izquierda+abajo+diagonal >2 | diagonal>0){
            vec=c(i,j)
            return(vec)
          }
        }
      }
    }
  }
}


#' Matriz extendida de acf
#'
#' Se calcula la matriz extendida, igual que en TSA solo que con un control de impresión de matriz
#'
#' @param z Serie de tiempo a analizar
#' @param ar.max Maximo valor del orden de AR
#' @param ma.max Maximo valor del orden de MA
#' @param print_matrix Indicador de si se emprime o no la matriz eacf
#'
#' @return Lista de valores del analisis eacf: matriz de acf extendida
#'    Symbol: matrix con los valores de cuales cordenadas de ARMA son significativos "x" para ese caso, "o" para no significativos.
#' @export
#'
#' @examples
#'
#' matriz_eacf(AirPassengers)
matriz_eacf <- function (z, ar.max = 7, ma.max = 13,print_matrix=TRUE)
{
  lag1 <- function(z, lag = 1) {
    c(rep(NA, lag), z[1:(length(z) - lag)])
  }
  reupm <- function(m1, nrow, ncol) {
    k <- ncol - 1
    m2 <- NULL
    for (i in 1:k) {
      i1 <- i + 1
      work <- lag1(m1[, i])
      work[1] <- -1
      temp <- m1[, i1] - work * m1[i1, i1]/m1[i, i]
      temp[i1] <- 0
      m2 <- cbind(m2, temp)
    }
    m2
  }
  ceascf <- function(m, cov1, nar, ncol, count, ncov, z, zm) {
    result <- 0 * seq(1, nar + 1)
    result[1] <- cov1[ncov + count]
    for (i in 1:nar) {
      temp <- cbind(z[-(1:i)], zm[-(1:i), 1:i]) %*% c(1,
                                                      -m[1:i, i])
      result[i + 1] <- acf(temp, plot = FALSE, lag.max = count,
                           drop.lag.0 = FALSE)$acf[count + 1]
    }
    result
  }
  ar.max <- ar.max + 1
  ma.max <- ma.max + 1
  nar <- ar.max - 1
  nma <- ma.max
  ncov <- nar + nma + 2
  nrow <- nar + nma + 1
  ncol <- nrow - 1
  z <- z - mean(z)
  zm <- NULL
  for (i in 1:nar) zm <- cbind(zm, lag1(z, lag = i))
  cov1 <- acf(z, lag.max = ncov, plot = FALSE, drop.lag.0 = FALSE)$acf
  cov1 <- c(rev(cov1[-1]), cov1)
  ncov <- ncov + 1
  m1 <- matrix(0, ncol = ncol, nrow = nrow)
  for (i in 1:ncol) m1[1:i, i] <- ar.ols(z, order.max = i,
                                         aic = FALSE, demean = FALSE, intercept = FALSE)$ar
  eacfm <- NULL
  for (i in 1:nma) {
    m2 <- reupm(m1 = m1, nrow = nrow, ncol = ncol)
    ncol <- ncol - 1
    eacfm <- cbind(eacfm, ceascf(m2, cov1, nar, ncol, i,
                                 ncov, z, zm))
    m1 <- m2
  }
  work <- 1:(nar + 1)
  work <- length(z) - work + 1
  symbol <- NULL
  for (i in 1:nma) {
    work <- work - 1
    symbol <- cbind(symbol, ifelse(abs(eacfm[, i]) > 2/work^0.5,
                                   "x", "o"))
  }
  rownames(symbol) <- 0:(ar.max - 1)
  colnames(symbol) <- 0:(ma.max - 1)
  if(print_matrix){
    cat("AR en Filas MA En columnas\n")#definir bien quien es quien
    print(symbol, quote = FALSE)
  }
  invisible(list(eacf = eacfm, ar.max = ar.max, ma.ma = ma.max,
                 symbol = symbol))
}

recomendaciones_arma(AirPassengers)
recomendaciones_arma(AirPassengers,print_matrix = FALSE)
