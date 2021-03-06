#Codigo hecho y en mantenimiento por @crissthiandi <albertocenaa@gmail.com>
#última versión en github.com/crissthiandi/antologiadefunciones

#' Recomendaciones a un grafico ACF o PACF
#'
#' Obten un recomendación a tu ajuste de series de tiempo ACF o PACF
#'
#' El valor IC se calcula siguiendo la metodologia de la paqueteria Stats en a función acf(). Se retorna el valor positivo talque el intervalo se forma con (IC,-IC)
#'
#' @param objeto_cf objeto ACF o PACF
#' @param print_IC Indicador True/False para mostrar valos absoluto de intervalo de confianza ver details
#'
#' @return Valor del ultimo lag significativo de la funcion de autocorrelacion
#' @export
#'
#' @examples
#' base=data.frame(x=seq(Sys.Date(),by="days",length=200),y=1:20*3+runif(20))
#' recomendacion_autocorrelaciones(acf(base$y,plot = FALSE))
#'
recomendacion_autocorrelaciones <- function(objeto_cf,print_IC=FALSE) {
  llamada=match.call()
  ruta=match(c("objeto_cf"),names(llamada))


  if(ruta!=2){#chequeo de que se agregaron bien los parametros
    #stopifnot(ruta!=2)
    message("Objeto_cf no encontrado o hay más de un parametro en la función")
    stop()
  }

  #Zona de plot not True
  a=llamada[[2]]
  a=as.character(a)
  if(length(a)!=3 | as.logical(a[3])){
    message("EL objeto debe ser un ACF o PACF con parametro plot = FALSE")
    message("Ver el ejemplo en la documentación")
    ?recomendacion_autocorrelaciones
  }
  #si la salida es un vector de 3 elementos entonces hay dos parametros
  serie=tryCatch(get(objeto_cf$series),error= function(e){message(e," \nSe busca otra entrada..."); return(NULL)})
  #en caso de serie NULL
  if(is.null(serie)){
    vec=strsplit(a[2],split = "$",fixed = TRUE)
    message("\nSe encontro la base de datos llamada: ",vec[[1]][1])
    message("\nDentro de ella se encontro el vector llamado: ",vec[[1]][2])
    serie=eval(str2lang(a[2]))
    cat("\nLos primeros 6 valores de este vector son:\n")
    print(head(serie))
  }
  serie=ts(serie)

  order_=NULL
  if(objeto_cf$type=="partial"){
    matriz=TSRutina::matriz_eacf(serie,ar.max = 1, ma.max = 15,print_matrix = FALSE)
    matriz=matriz$symbol=="o"
    for(i in 1:15){
      if(matriz[1,i]==1){
        order_=i
        cat("\nProponemos MA(q) con q=:",order_)
        break
      }
    }
    if(is.null(order_)){
      cat("El valor de la q propuesta es mayor a 15...")
      order_=16
    }

  }
  if(objeto_cf$type=="correlation"){
    matriz=TSRutina::matriz_eacf(serie,ar.max = 15,ma.max = 1,print_matrix = FALSE)
    matriz=matriz$symbol=="o"
    for(i in 1:15){
      if(matriz[i,1]==1){
        order_=i
        cat("\nProponemos AR(p) con p=:",order_)
        break
      }
    }
    if(is.null(order_)){
      cat("El valor de la p propuesta es mayor a 15...")
      order_=16
    }
  }



  #obtener los intervalos de confianza dando el objeto
  IC=intervalo_confianza_acf(objeto_cf)
  #mayores=abs(objeto_cf$acf)>IC
  #cat("\nLos siguientes elementos son propuestas de r: ")
  #posibles_lags=objeto_cf$lag[mayores]
  #cat(posibles_lags)
  #cat("\nProponemos que r sea:",posibles_lags[length(posibles_lags)])

  if(print_IC){
    cat("\nEl IC de modelo es: ",IC)
  }

  return(invisible(order_))
}



#' Obten el intervalo de confianza de un objeto ACF o PACF
#'
#' Atravez del metodo usado en Stats se obtiene el valor de los intervalos de confianza
#'
#'
#' Esta pensado para su uso en Rutinas de la paqueteria TSRutina, aunque su uso como función independiente
#' deberia ser sencillo, se recomienda usarse solo si se conoce sobre el codigo y el tema.
#'
#' @param x Objeto ACF o PACF
#' @param ci Valor de Alpha, por defecto 0.95
#' @param type Tipo de linea en la grafica (No usar, deshabilitado)
#' @param xlab (No usar, deshabilitado)
#' @param ylab (No usar, deshabilitado)
#' @param ylim (No usar, deshabilitado)
#' @param main (No usar, deshabilitado)
#' @param ci.col (No usar, deshabilitado)
#' @param ci.type Tipo de intervalo de confianza por defecto es white
#' @param ... Parametros que se pueden pasar a el grafico
#'
#' @return Valor numerico que representa el limite del intervalo de confianza
#' @export
#'
#' @examples
#' base=data.frame(x=seq(Sys.Date(),by="days",length=20),y=1:20*3+runif(1))
#' p=acf(base$y,plot=FALSE)
#' intervalo_confianza_acf(p) #0.4382613
intervalo_confianza_acf=function (x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
          ylim = NULL, main = NULL, ci.col = "blue", ci.type = c("white",
                                                                 "ma"), max.mfrow = 6, ask = Npgs > 1 && dev.interactive(),
          mar = if (nser > 2) c(3, 2, 2, 0.8) else par("mar"), oma = if (nser >
                                                                         2) c(1, 1.2, 1, 1) else par("oma"), mgp = if (nser >
                                                                                                                       2) c(1.5, 0.6, 0) else par("mgp"), xpd = par("xpd"),
          cex.main = if (nser > 2) 1 else par("cex.main"), verbose = getOption("verbose"),
          ...)
{
  ci.type <- match.arg(ci.type)
  if ((nser <- ncol(x$lag)) < 1L)
    stop("x$lag must have at least 1 column")
  if (is.null(ylab))
    ylab <- switch(x$type, correlation = "ACF", covariance = "ACF (cov)",
                   partial = "Partial ACF")
  if (is.null(snames <- x$snames))
    snames <- paste("Series ", if (nser == 1L)
      x$series
      else 1L:nser)
  with.ci <- ci > 0 && x$type != "covariance"
  with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"
  if (with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
    warning("can use ci.type=\"ma\" only if first lag is 0")
    with.ci.ma <- FALSE
  }
  clim0 <- if (with.ci)
    qnorm((1 + ci)/2)/sqrt(x$n.used)
  else c(0, 0)
  Npgs <- 1L
  nr <- nser
  if (nser > 1L) {
    if (nser > max.mfrow) {
      Npgs <- ceiling(nser/max.mfrow)
      nr <- ceiling(nser/Npgs)
    }
    # opar <- par(mfrow = rep(nr, 2L), mar = mar, oma = oma,
    #             mgp = mgp, ask = ask, xpd = xpd, cex.main = cex.main)
    # on.exit(par(opar))
    if (verbose) {
      message("par(*) : ", appendLF = FALSE, domain = NA)
      str(par("mfrow", "cex", "cex.main", "cex.axis",
              "cex.lab", "cex.sub"))
    }
  }
  if (is.null(ylim)) {
    ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
    if (with.ci)
      ylim <- range(c(-clim0, clim0, ylim))
    if (with.ci.ma) {
      for (i in 1L:nser) {
        clim <- clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1,
                                                   i, i]^2)))
        ylim <- range(c(-clim, clim, ylim))
      }
    }
  }
  for (I in 1L:Npgs) for (J in 1L:Npgs) {
    dev.hold()
    iind <- (I - 1) * nr + 1L:nr
    jind <- (J - 1) * nr + 1L:nr
    if (verbose)
      message(gettextf("Page [%d,%d]: i =%s; j =%s", I,
                       J, paste(iind, collapse = ","), paste(jind,
                                                             collapse = ",")), domain = NA)
    for (i in iind) for (j in jind) if (max(i, j) > nser) {
      frame()
      box(col = "light gray")
    }
    else {
      clim <- if (with.ci.ma && i == j)
        clim0 * sqrt(cumsum(c(1, 2 * x$acf[-1, i, j]^2)))
      else clim0
      # plot(x$lag[, i, j], x$acf[, i, j], type = type,
      #      xlab = xlab, ylab = if (j == 1)
      #        ylab
      #      else "", ylim = ylim, ...)
      # abline(h = 0)
      if (with.ci && ci.type == "white")
        clim=clim
        # abline(h = c(clim, -clim), col = ci.col, lty = 2)
      else if (with.ci.ma && i == j) {
        clim <- clim[-length(clim)]
        # lines(x$lag[-1, i, j], clim, col = ci.col, lty = 2)
        # lines(x$lag[-1, i, j], -clim, col = ci.col,
              # lty = 2)
      }
      # title(if (!is.null(main))
        # main
        #else if (i == j)
         # snames[i]
        #else paste(sn.abbr[i], "&", sn.abbr[j]), line = if (nser >
                                                          #  2)
         # 1
        #else 2)
    }
    if (Npgs > 1) {
      mtext(paste("[", I, ",", J, "]"), side = 1, line = -0.2,
            adj = 1, col = "dark gray", cex = 1, outer = TRUE)
    }
    dev.flush()
  }
  return(clim)

}
