
tags = function(Sankey,nodos){
  
nodostag1= Sankey %>% select(Valor,color=Color_origen) %>%
  group_by(color) %>% summarise(tag=sum(Valor))
nodostag2=Sankey %>% select(Valor,color=Color_destino) %>%
  group_by(color) %>% summarise(tag=sum(Valor))

nodostag=rbind(nodostag1,nodostag2)

nodos=nodos %>% left_join(nodostag,by = c("nombre"="color"))

Sankey$IDOrigen <- match(Sankey$Color_origen, nodos$nombre)-1
Sankey$IDDestino <- match(Sankey$Color_destino, nodos$nombre)-1

Sankey$Color_origen<-as.factor(Sankey$Color_origen)
Sankey$Color_destino<-as.factor(Sankey$Color_destino)
Sankey=as.data.frame(Sankey)

return(list(Sankey,nodos))

}

centrar_tags=function(widget,distancia){
  
  ajuste=sprintf('
  function(el, x) {
    d3.selectAll(".node text").attr("text-anchor", "begin").attr("x", %d);
  }
  ',distancia)
  
  p=htmlwidgets::onRender(
    widget,
    ajuste
  )
  return(p)
}
