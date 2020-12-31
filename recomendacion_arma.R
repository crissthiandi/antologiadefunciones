

recomendaciones_arma <- function(time_series) {
  x=time_series

  modelo_arma <- TSA::eacf(x, ar.max=4, ma.max=4)

  matriz_true_false <- modelo_arma=="0"

  for (i in 1:5) {
    if(sum(matriz_true_false[,i])>0){
      for (j in 1:5) {
        zz=matriz_true_false[,i]
        if(zz[j]==1){

        }
      }
    }
  }



  # arma_pq<-if(arma_pq==""){
  #   c(as.numeric(rec[1]),numero_diferenciaciones,as.numeric(rec[2]))
  # }else{
  #   #checar herramientas de expresiones regulares
  #   #la idea es dividir el elemento en dos
  #   arma_pq
  #   c(0,numero_diferenciaciones,as.numeric())
  # }
  # pausa()

}
