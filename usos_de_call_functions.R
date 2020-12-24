data=iris

testeo <- function(formula,data,subset, weights, na.action, method = "qr",
                   model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
                   contrasts = NULL, offset, ...) {
  ret.x <- x
  ret.y <- y
  cl <- match.call() #objeto de llamadas
  mf <- match.call(expand.dots = FALSE) #objeto llamada para acompletar
  #llama los elementos del enviroment esta forma es fija
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)#vector de llamadas
  mf <- mf[c(1L, m)]#supongo que el numero de parametros a usar (?)
  mf$drop.unused.levels <- TRUE #asignación de el primer parametro
  #output:   match.call(drop.unused.levels = TRUE)
  mf[[1L]] <- quote(stats::model.frame) #crea texto que se agrega en la posicion 1L
  mf <- eval(mf) #Se evalua la llamada anterior, el objeto debe ser call class
  if (method == "model.frame")
    return(mf)
  else if (method != "qr")
    warning(gettextf("method = '%s' is not supported. Using 'qr'",
                     method), domain = NA)
  mt <- attr(mf, "terms") #obtiene los atributos del elemento mf, especificamente los de terms
  y <- model.response(mf, "numeric") #De los valores obtenidos anteriormente crea un vector
  w <- as.vector(model.weights(mf))
  if (!is.null(w) && !is.numeric(w))
    stop("'weights' must be a numeric vector")
  offset <- model.offset(mf)
  mlm <- is.matrix(y)
  ny <- if (mlm)
    nrow(y)
  else length(y)
  if (!is.null(offset)) {
    if (!mlm)
      offset <- as.vector(offset)
    if (NROW(offset) != ny)
      stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                    NROW(offset), ny), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (mlm) matrix(NA_real_, 0,
                                             ncol(y)) else numeric(), residuals = y, fitted.values = 0 *
                y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w !=
                                                                                0) else ny)
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
  }
  else {
    x <- model.matrix(mt, mf, contrasts)
    z <- if (is.null(w))
      lm.fit(x, y, offset = offset, singular.ok = singular.ok,
             ...)
    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
                 ...)
  }
  class(z) <- c(if (mlm) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model)
    z$model <- mf
  if (ret.x)
    z$x <- x
  if (ret.y)
    z$y <- y
  if (!qr)
    z$qr <- NULL
  z
}
testeo(Sepal.Length ~ Petal.Length + Petal.Width,data)
lm(Sepal.Length ~ Petal.Length,data,tol = 1e-17) # tol es un parametro de lm.fit


#Call section
#se hacen llamada desde la función eval()

#imprimiendo desde eval()

#como no hacerlo

eval(print(2)) #repite el proceso, ya que la entrado no es un call class
evalq(print(2)) #entiende que el objeto ingresado es de tipo quote y realiza el ajuste

#como hacerlo
llamada=quote(print(2)) #creas el quote
llamada

eval(llamada) #se evaluo la llamada
evalq(print(2))












