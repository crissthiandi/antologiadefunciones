#función que busca ver si dos columnas tiene los mismos elementos sin importar el roden



tienen_lo_mismo = function(base_x, base_y,por_join=F,por_merge=T,ayuda=F,...) {
  if(ayuda){browseURL("https://rpubs.com/Rortizdu/140174")}
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
  #se checa join
  
  if(por_join){
    base_output=as.list(NULL)
    base_output[["en_comun"]]=base_x %>% inner_join(base_y ,by="base_col")
    base_output[["solo_en_x"]]=base_x %>% left_join(base_y ,by="base_col")
    base_output[["solo_en_y"]]=base_x %>% right_join(base_y ,by="base_col")
    base_output[["Excluidos"]]=base_x %>% filter(base_output$en_comun)
  }else{
    base_output=merge(base_x,base_y,by = "base_col",...)
  }
  
  return(base_output)
}

#testing

a=tibble(
  x=1:5
)

b=tibble(
  y=2:8
)

tienen_lo_mismo(a,b,all.y=T)






