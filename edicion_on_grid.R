library("ggplot2")
d <- ggplot(mtcars, aes(x=gear)) + 
  geom_bar(aes(y=gear), stat="identity", position="dodge") +
  facet_wrap(~cyl)

grob <- ggplotGrob(d)
strip_bg <- grid.ls(getGrob(grob, "strip.background.rect",
                            grep=TRUE, global=TRUE))$name
panel_bg <- grid.ls(getGrob(grob, "panel.background.rect",
                            grep=TRUE, global=TRUE))$name
strip_text <- grid.ls(getGrob(grob, "strip.text.x",
                              grep=TRUE, global=TRUE))$name
grob <- geditGrob(grob, strip_bg[2], gp=gpar(fill="gray60"))
grob <- geditGrob(grob, panel_bg[2], gp=gpar(fill="darkolivegreen2"))
grob <- geditGrob(grob, strip_text[2], gp=gpar(col="white"))
grid.draw(grob)