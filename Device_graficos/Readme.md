Device Grafics
================
 @Crissthiandi 
2021-03-25

La mayoria de este codigo es publicado con licencia CC por lo que es
publicado en este script sin faltar a dichos derechos.

Saludos lector, esto no tiene nada que ver con analisis de imagen o así,
solo es codigo de un gran hacker de origen chino que admiro mucho y me
gusta ver su contenido ¿Que opinas de su forma de programar?

### Ruta napoleonica

![](Readme_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Esta padre la representación de la caminata napoloonica, no?

#### Degradado

Del como hacer graficos con degradados usando la paqueteria base de R
![](Readme_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### Rte (R arte)

Le llaman arte con R, no es algo formal pero es interesante como podemos
hacer que unas distribuciones aleatorias nos den graficos lindos.

![](Readme_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### Cairodevice

Al menos en mi windows no funciona el paquete cairoDevice, supongo debe
ser algo para linux que no corre en windows :c

![](Readme_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

    ## png 
    ##   2

#### Poligonos de caledoscopio

![](Readme_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

#### incorpora el uso de usage() fun

``` r
library(formatR)
usage(grid)
```

    ## grid(nx = NULL, ny = nx, col = "lightgray", lty = "dotted", lwd = par("lwd"),
    ##     equilogs = TRUE)

#### Barras laterales

![](Readme_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

#### Gráfico de cascada

    ## [1]   66  -32  -63  -35 -217   30

![](Readme_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
