#' Fix #10 ¿cuales son los intentos de tener decoradores en R?
#' 
#' visite https://www.r-bloggers.com/2020/03/how-to-create-decorators-in-r/
#' la realiadad es que esta implementación de decoradores parece ser la solución, 
#' tiene sugar syntax y todo. Pero tiene unos detalles.
#' 
#' Para empezar es una libreria totalmente ajena a la CRAN
#' lo cual al principio no tiene nada de malo pero que llama la atención
#' 
#' segundo el repositorio de github donde se aloja no ha tenido updates desde el 2016...
#' 
#' Esto puede ser un problema, sin embargo es normal pensar que hay proyectos simples que no necesitan tantas mods...
#' 
#' tercero, tiene un rival muy convincente y que parece no abusar de la api de rstudio.
#' 
#' 
#' Otros proyectos:
#' https://github.com/klmr/decorator
#' Este proyecto sigue vigente pero tiene un enorme cartel,
#' al menos al dia 04/jul/2022 que avisa que la api es actualmente
#' muy volatil, pues esta en desarrollo
#' 
#' get-starting
tryCatch(
  library(tinsel),
  error = function(e){
    message(e,'n')
    devtools::install_github('nteetor/tinsel')
    library(tinsel)
  }
)

#' función decoradora
print_start_end <- function(f)
{
  wrapper <- function(...)
  {
    print("Starting function call...") 
    
    f() 
    
    print("Finished function call...") 
    
  }
  
  return(wrapper) 
  
}

#. print_start_end
todays_date <- function()
{
  print(Sys.Date())
  
}

#' con estos sencillos pasos cuando uses la función source_decorators
#' entonces veras la magia...
# source_decoratees("test_dec.R")

#' el problema esta en el hecho de que los codigos R muchas veces son 
#' usados al momento, y el tener que trabajar con decoradores de esta forma es algo incomodo.
#' 
#' podemos hacer un equivalente util?
#' 
detach("package:tinsel", unload = TRUE)


#' función cloussere
contador <- function(fun_start)
{
  wrapper <- function()
  {
    n <- fun_start
    
    while (n>0) {
      print(sprintf('T-minus %d',n))
      n <- n-1
    }
  }
  return(wrapper) 
  
}
#' retorna una función con memoria
contador(5)
#' ejecutemos la
contador(5)()
#' almacenado
f1 <- contador(5)
f1()

#' usemos la memoria de closures en un trabajito xd

create_adder <- function(x){
  adder <- function(y){
    return(x+y)
  }
  return(adder)
}
#' almacena el 15 como valor a recordar
add_15 = create_adder(15)

print(add_15(10)) # espero 25
print(add_15(-5)) # espero 10

# crearemos un decorador
library(magrittr)

decorador_1 <- function(fun_fun){
  wrapper <- function(...){
    print("before Execution")
    fun_return <- fun_fun(...)
    print(fun_return)
    print("after Execution")
    # returning the value to the original frame
    return(fun_return)
  }
  return(wrapper)
}

suma <- function(x,y){
  return(x+y)
}

decorado <- decorador_1(suma)
decorado_pipe <- suma %>% decorador_1
all.equal(decorador_1,decorado_pipe)

salida_temp <- decorado(2,3)
salida_temp

salida_temp <- decorado_pipe(2,3)
salida_temp

#' and that is!!
#' ya tenemos un decorador de la función que funciona correctamente

#' mejoresmos el asunto!
#' como sabemos `%@%` no esta definido, ni en magrittr hay un uso
#' por lo que podemos adoptar esta sintaxis

`%@%` <- function(lhs, rhs){
  
  tryCatch(
    is.function(lhs),
    error = function(e){
      stop("El decorador no es una función, remueve ()",call. = F)
    }
  )
  tryCatch(
    is.function(rhs),
    error = function(e){
      stop("El decorador no es una función, remueve ()",call. = F)
    }
  )
  
  # if(!clase){
  #   rhs <- deparse(quote(suma())[[1]])
  #   lhs <- deparse(quote(lhs))
  #   return(eval(str2expression(paste(lhs,"(",rhs,")"))))
  # }else{
  #   return(lhs(rhs))
  # }
  return(lhs(rhs))
}

decorado_ <- decorador_1 %@% suma
decorado_(2,3)

decorado_ <- decorador_1 %@% suma()
decorado_ <- decorador_1() %@% suma
decorado_ <- decorador_1() %@% suma()

#' en la practica este operador `%@%` puede facilmente ser remplazado
#' por %>% pero sin mensajes de errores aprueba de typos



