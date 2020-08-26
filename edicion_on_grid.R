library("ggplot2")
d <- ggplot(mtcars, aes(x=gear)) + 
  geom_bar(aes(y=gear), stat="identity", position="dodge") +
  facet_wrap(~cyl)

geditGrob <- function(..., grep = TRUE, global = TRUE) {	
  editGrob(..., grep = grep, global = global)	
}

grob <- ggplotGrob(d)
strip_bg <- grid.ls(getGrob(grob, "strip.background.rect",
                            grep=TRUE, global=TRUE))$name
panel_bg <- grid.ls(getGrob(grob, "panel.background.rect",
                            grep=TRUE, global=TRUE))$name
strip_text <- grid.ls(getGrob(grob, "strip.text.x",
                              grep=TRUE, global=TRUE))$name
grob <- geditGrob(grob, strip_bg[2], gp=gpar(fill="gray60"),grep=T,global = T)
grob <- geditGrob(grob, panel_bg[2], gp=gpar(fill="darkolivegreen2"),grep=T,global = T)
grob <- geditGrob(grob=grob, strip_text[2], gp=gpar(col="white"),grep=T,global = T)
grid.draw(grob)


############ Intento 2 ----

grob <- ggplotGrob(d)

elem <- grob$grobs$panel2
panel_bg <- grid.ls(getGrob(elem, "panel.background.rect", grep=TRUE))$name
grob$grobs$panel2 <- editGrob(elem, panel_bg, gp=gpar(fill="darkolivegreen"), grep=TRUE)

elem <- grob$grobs$strip_t.1
strip_bg <- grid.ls(getGrob(elem, "strip.background.rect", grep=TRUE))$name
grob$grobs$strip_t.1 <- editGrob(elem, strip_bg, gp=gpar(fill="gray60"), grep=TRUE)

elem <- grob$grobs$strip_t.1
strip_text <- grid.ls(getGrob(elem, "strip.text.x.text", grep=TRUE))$name
grob$grobs$strip_t.1 <- editGrob(elem, strip_text, gp=gpar(col="white"), grep=TRUE)

grid.draw(grob)
