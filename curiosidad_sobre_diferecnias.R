#supongamos que tenemos una secuencia como sigue

a=seq(1,20,0.1)
#luego tenemos que f(x)=e^(x) esta dado por
f=exp(a)
plot(f)
#en teoria a derivada de f debe ser f misma, pero que pasa con la diferencia?
plot(diff(f,differences = 1))
#parece tener un coportamiento exponencial casi similar a la original
#y si volvemos a diferenciar?
plot(diff(f,differences = 2))

#la duda es cuantas diferencias seriannecesarias para peder esa caracteristica exponencial?
#Respuesta, para este caso 10-11
plot(diff(f,differences = 10))
plot(diff(f,differences = 11))

#y que hay despues de la onceava diferenciacion
layout(matrix(1:4,2))
plot(diff(f,differences = 12))
plot(diff(f,differences = 13))
plot(diff(f,differences = 14))
plot(diff(f,differences = 15))
#otras 4
layout(matrix(1:4,2))
plot(diff(f,differences = 16))
plot(diff(f,differences = 17))
plot(diff(f,differences = 18))
plot(diff(f,differences = 19))

#interesante el saber porque esto pasa
#mas interesante ver que pasa cuando aumentamos la finura de la curva, haciendo más densa la recta

#spoiler, la diferenciacion 5 da los resultados que la 12 en adelante

curva_exp <- function(finura=0.01,inicio=1,fin=20){
    dev.off()
    TSRutina::dev.TRS()
    a=seq(inicio,fin,finura)
    #luego tenemos que f(x)=e^(x) esta dado por
    f=exp(a)
    plot(f)
    #en teoria a derivada de f debe ser f misma, pero que pasa con la diferencia?
    plot(diff(f,differences = 1))
    TSRutina::pausa()
    #parece tener un coportamiento exponencial casi similar a la original
    #y si volvemos a diferenciar?
    plot(diff(f,differences = 2))
    TSRutina::pausa()
    plot(diff(f,differences = 3))
    TSRutina::pausa()
    plot(diff(f,differences = 4))
    TSRutina::pausa()
    plot(diff(f,differences = 5))
    TSRutina::pausa()
    plot(diff(f,differences = 6))
    TSRutina::pausa()
    #la duda es cuantas diferencias seriannecesarias para peder esa caracteristica exponencial?
    #Respuesta, para este caso 10-11
    plot(diff(f,differences = 10))
    TSRutina::pausa()
    plot(diff(f,differences = 11))
    TSRutina::pausa()
    #y que hay despues de la onceava diferenciacion
    layout(matrix(1:4,2))
    plot(diff(f,differences = 12))
    plot(diff(f,differences = 13))
    plot(diff(f,differences = 14))
    plot(diff(f,differences = 15))
    TSRutina::pausa()
    #otras 4

    layout(matrix(1:4,2))
    plot(diff(f,differences = 16))
    plot(diff(f,differences = 17))
    plot(diff(f,differences = 18))
    plot(diff(f,differences = 19))
    TSRutina::pausa()
}

curva_exp(finura = 2,fin = 60)

