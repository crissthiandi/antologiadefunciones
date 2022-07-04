# un R6 simple
# 
# 
clase1 <- R6::R6Class(classname = "esoxd",
                      public = list(
                        cosa_inside = NULL,
                        initialize = function(eso = NULL){
                          self$cosa_inside <- eso
                          cat(eso)
                        }
                      ))

objeto <- clase1$new(
  eso = "Peroxido"
)
objeto

