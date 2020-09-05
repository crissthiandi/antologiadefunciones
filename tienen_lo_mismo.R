#función que busca ver si dos columnas tiene los mismos elementos sin importar el orden



tienen_lo_mismo = function(base_x, base_y,por_join=F,por_merge=T,ayuda=F,FULL=F,recursion=T,...) {
  if(ayuda){
    browseURL("https://rpubs.com/Rortizdu/140174")
    browseURL("https://pmoracho.github.io/blog/2017/06/26/Combinacion-de-datos-en-R/")
  }
  #parche para inconcistencias
  if(!por_merge){por_join=T}

  #se checan los nombres
  nombre=names(base_x)
  if(length(nombre)==1){
    names(base_x)="base_col"
  }else{
    names(base_x)[1]="base_col"
  }
  
  nombre=names(base_y)
  if(length(nombre)==1){
    names(base_y)="base_col"
  }else{
    names(base_y)[1]="base_col"
  }
  
  #respaldo para ver quienes no tiene pareja
  base_x["Originales_x"]=base_x["base_col"]

  #se checa join
  if(por_join){
    base_output=as.list(NULL)
    if(recursion){#se usa condicional porque en la recursividad no se quiere volver a calcular la base en comun
      #de esta forma de ahorra tiempo de ejecución
      base_output[["en_comun"]]=base_x %>% inner_join(base_y ,by="base_col") %>% select(base_col)
      message("Excluido_y incluye los elementos que no estan en x pero si en y")
    }
    base_output[["todo_en_y"]]=base_x %>% right_join(base_y ,by="base_col")
    base_output[["Excluidos_y"]]=base_x %>% right_join(base_y ,by="base_col") %>%filter(is.na(Originales_x))
  }else{
    message("Use all=T para obtener los elementos que no estan en x pero si en y")
    base_output=merge(base_x,base_y,by = "base_col",...)
  }
  
  if(FULL & por_join){#se usa por join y full ya que no se puede usar full sin join
    base_x["Originales_x"]=NULL
    
    #Resolver luego posible generador de latencia uwu reparar luego
    Salida=tienen_lo_mismo(base_x = base_y,base_y = base_x,por_merge = F,recursion = F)
    base_output[["todo_en_x"]]=Salida$todo_en_y
    base_output[["Excluidos_x"]]=Salida$Excluidos_y
    message("Excluido_x incluye los elementos que no estan en y pero si en x")
    
    #se cambia nombre de los elementos recursivos para que tenga sentido que falta
    names(base_output[["todo_en_x"]])=c("base_col","Originales_y")
    names(base_output[["Excluidos_x"]])=c("base_col","Originales_y")
  }
  

  return(base_output)
  
}

#testing ----

# a=tibble(
#   x=1:5
# )
# 
# b=tibble(
#   y=2:8
# )
# 
# tienen_lo_mismo(a,b,por_merge = F,FULL = T)
# 





