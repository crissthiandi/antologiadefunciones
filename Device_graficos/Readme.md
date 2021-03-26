Device Grafics
================
 @Crissthiandi 
2021-03-25

I really <i class="nf nf-fa-heart"></i> <i class="nf nf-custom-vim"></i>

La mayoria de este codigo es publicado con licencia CC por lo que es
publicado en este script sin faltar a dichos derechos.

Saludos lector, esto no tiene nada que ver con analisis de imagen o así,
solo es codigo de un gran hacker de origen chino que admiro mucho y me
gusta ver su contenido ¿Que opinas de su forma de programar?

### Ruta napoleonica

![](Readme_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

\#Esta padre la representación de la caminata napoloonica, no?

# Degradado

# Del como hacer graficos con degradados usando la paqueteria base de R

xx &lt;- c(1912, 1912:1971, 1971) yy &lt;- c(min(nhtemp), nhtemp,
min(nhtemp)) plot(xx, yy, type = “n”, xlab = “Year”, ylab =
“Temperatures”) for (i in seq(255, 0, -3)) { yy &lt;- c(45, nhtemp -
(nhtemp - min(nhtemp)) \* (1 - i / 255), 45) \# rgb() polygon(xx, yy,
col = rgb(1, i / 255, 0), border = NA) \# Sys.sleep(0.05) } box()

# Rte

# Le llaman arte con R, no es algo formal pero es interesante como podemos

# hacer que unas distribuciones aleatorias nos den graficos lindos

size &lt;- devAskNewPage(TRUE) par(mar = c(0.2, 0.2, 0.2, 0.2), mfrow =
c(2, 2)) for (n in c(42, 61, 64, 65)) { set.seed(711) plot.new() size =
c(replicate(n, 1 / rbeta(2, 1.5, 4))) center = t(replicate(n, runif(2)))
center = center\[rep(1:n, each = 2),\] color = apply(replicate(2 \* n,
sample(c(0:9, LETTERS\[1:6\]), 8, replace = TRUE)), 2, function(x) {
sprintf(“\#%s”, paste(x, collapse = "")) }) points(center, cex = size,
pch = rep(20:21, n), col = color) box() text(0.5, 0.5, n) }

# Cairodevice

# Al menos en mi windows no funciona el paquete cairoDevice,

# supongo debe ser algo para linux que no corre en windows :c

library(cairoDevice) Cairo\_png(“points-desktop.png”, width = 13.66 \*
1.39, height = 7.68 \* 1.39) par(mar = c(0, 0, 0, 0)) n = 76
set.seed(711) plot.new() size = c(replicate(n, 1 / rbeta(2, 1.5, 4)))
center = t(replicate(n, runif(2))) center = center\[rep(1:n, each = 2),
\] color = apply(replicate(2 \* n, sample(c(0:9, LETTERS\[1:6\]), 8,
replace = TRUE)), 2, function(x) sprintf(“\#%s”, paste(x, collapse =
""))) points(center, cex = size, pch = rep(20:21, n), col = color)
dev.off()

# Poligonos de caledoscopio

set.seed(77) x = rnorm(3) y = rnorm(3)

for (i in 1:150) { x = c(x, NA, rnorm(3, tail(x, 1))) y = c(y, NA,
rnorm(3, tail(y, 1))) } x = c(x, NA) y = c(y, NA) xr = diff(range(x,
na.rm = TRUE)) yr = diff(range(y, na.rm = TRUE)) x = c(x, x + xr, x + xr
/ 2, x + xr / 2) y = c(y, y, y + yr, y - yr) par(mar = rep(0, 4))
plot(x, y, type = “n”, ann = FALSE, axes = FALSE)

polygon(x, y, col = sample(colors(), 151), border = NA)

\#incorpora el uso de usage() fun \#\#\#\#

library(formatR) usage(grid)

# Barras laterales

data(Export.USCN, package = “MSG”) par(mar = c(4, 4.5, .1, 4.5)) \#
看似条形图，实为粗线条，宽度 lwd = 10 plot(1:13, Export.USCN$Export,
xlab = “Year / Country”, ylab = “US Dollars (10<sup>16</sup>)”, xaxt =
“n”, type = “h”, lwd = 10, col = c(rep(2, 6), NA, rep(4, 6)), lend = 1,
panel.first = grid() ) \# 设置 x 轴的刻度标记：的意思是换行符 xlabel
&lt;- paste(Export.USCN$Year, "\\n", Export.USCN$Country) xlabel\[7\]
&lt;- "" abline(v = 7, lty = 2) \# 添加一条分隔线 \#
使用带有换行符的刻度标记 axis(1, 1:13, labels = xlabel, tick = FALSE,
cex.axis = 0.75) \# 换算为人民币再计算另一个坐标轴刻度（汇率 8.27）
ylabel &lt;- pretty(Export.USCN$Export \* 8.27) axis(4, at = ylabel /
8.27, labels = ylabel) mtext(“Chinese RMB (10<sup>16</sup>)”, side = 4,
line = 2) box()

# 七天之内的唯一访问次数数据（周一为 2010 年 8 月 2 日）

auv &lt;- c(939, 1005, 973, 910, 875, 658, 688) \# 相邻两天作差
diff(auv) par(mar = c(4, 4, .5, .1)) plot(auv, xlab = “Primera semana de
agosto”, ylab = “Visitas absolutamente únicas”, type = “n”, xlim =
c(0.5, 7.5), ylim = c(0, max(auv)), xaxt = “n”, panel.first = grid() )
axis(1, 1:7, sprintf(“semana%s”, c(
“uno”,“dos”,“tres”,“cuatro”,“cinco”,“seis”,“siete” ))) rect(1:7 - 0.3,
c(0, auv\[1:6\]), 1:7 + 0.3, auv, col = c(NA, ifelse(diff(auv) &lt; 0,
“red”, NA)) )
