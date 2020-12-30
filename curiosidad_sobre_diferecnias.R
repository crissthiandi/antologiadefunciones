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





