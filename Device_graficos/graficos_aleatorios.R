#  @Crissthiandi 

# La mayoria de este codigo es publicado con licencia CC por lo que es publicado en este script sin faltar a dichos derechos
# Saludos lector, esto no tiene nada que ver con analisis de imagen o así, solo es codigo de un gran hacker de origen chino que
# admiro mucho y me gusta ver su contenido ¿Que opinas de su forma de programar?

troops <-
  read.table(system.file("extdata", "troops.txt", package = "MSG"), header = TRUE)
cities <-
  read.table(system.file("extdata", "cities.txt", package = "MSG"), header = TRUE)
library(ggplot2)
p <- ggplot(cities, aes(x = long, y = lat)) # 框架
p <-
  p + geom_path(
    aes(
      size = survivors,
      colour = direction,
      group = group
    ),
    data = troops,
    lineend = "round"
  ) # 军队路线
p <- p + geom_point() # 城市点
p <-
  p + geom_text(aes(label = city),
                hjust = 0,
                vjust = 1,
                size = 2.5) # 城市名称
p <- p + scale_colour_manual(values = c("grey50", "red")) +
  scale_size(range = c(1, 10)) +
  theme(legend.position = "none") +
  xlim(24, 39) # 细节调整工作
print(p) # 打印全图

#Esta padre la representación de la caminata napoloonica, no?



# Del como hacer graficos con degradados usando la paqueteria base de R
xx <- c(1912, 1912:1971, 1971)
yy <- c(min(nhtemp), nhtemp, min(nhtemp))
plot(xx,
     yy,
     type = "n",
     xlab = "Year",
     ylab = "Temperatures")
for (i in seq(255, 0, -3)) {
  yy <-
    c(45, nhtemp - (nhtemp - min(nhtemp)) * (1 - i / 255), 45) # rgb() 中的绿色成分逐渐变小
  polygon(xx, yy, col = rgb(1, i / 255, 0), border = NA)
  # 读者可以在这里加上 Sys.sleep(0.05) 以便看清作图过程
}
box() # 补齐边


# Le llaman arte con R, no es algo formal pero es interesante como podemos hacer que unas distribuciones aleatorias nos den graficos lindos
size <- devAskNewPage(TRUE)
par(mar = c(0.2, 0.2, 0.2, 0.2), mfrow = c(2, 2))
for (n in c(42, 61, 64, 65)) {
  set.seed(711)
  plot.new()
  size = c(replicate(n, 1 / rbeta(2, 1.5, 4)))
  center = t(replicate(n, runif(2)))
  center = center[rep(1:n, each = 2),]
  color = apply(replicate(2 * n,
                          sample(c(0:9, LETTERS[1:6]), 8, replace = TRUE)), 2,
                function(x) {
                  sprintf("#%s", paste(x, collapse = ""))
                })
  points(center,
         cex = size,
         pch = rep(20:21, n),
         col = color)
  box()
  text(0.5, 0.5, n)
}



# al menos en mi windows no funciona el paquete cairoDevice, supongo debe ser algo para linux que no corre en windows :c
library(cairoDevice)
Cairo_png("points-desktop.png",
          width = 13.66 * 1.39,
          height = 7.68 * 1.39)
par(mar = c(0, 0, 0, 0))
n = 76
set.seed(711)
plot.new()
size = c(replicate(n, 1 / rbeta(2, 1.5, 4)))
center = t(replicate(n, runif(2)))
center = center[rep(1:n, each = 2), ]
color = apply(replicate(2 * n, sample(c(0:9, LETTERS[1:6]),
                                      8, replace = TRUE)), 2, function(x)
                                        sprintf("#%s", paste(x,                                                                                           collapse = "")))
points(center,
       cex = size,
       pch = rep(20:21, n),
       col = color)
dev.off()



# Poligonos de
set.seed(77)
x = rnorm(3)
y = rnorm(3)

for (i in 1:150) {
  x = c(x, NA, rnorm(3, tail(x, 1)))
  y = c(y, NA, rnorm(3, tail(y, 1)))
}
x = c(x, NA)
y = c(y, NA)
xr = diff(range(x, na.rm = TRUE))
yr = diff(range(y, na.rm = TRUE))
x = c(x, x + xr, x + xr / 2, x + xr / 2)
y = c(y, y, y + yr, y - yr)
par(mar = rep(0, 4))
plot(x,
     y,
     type = "n",
     ann = FALSE,
     axes = FALSE)

polygon(x, y, col = sample(colors(), 151), border = NA)



#incorpora el uso de usage() fun

library(formatR)
usage(grid)
