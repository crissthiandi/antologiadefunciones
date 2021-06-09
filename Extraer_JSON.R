check_date = "https://covidmap.umd.edu/api/datesavail?country=Mexico"
library(dplyr)

solicitame <- function(indicador=NULL,tipo = NULL,pais = NULL,region=NULL,
                       rango_fecha=NULL,path = NULL) {
    # Función hecha para extraer datos de un repositorio que los almacene el json 
    if(is.null(path)){
        path <- paste0("https://covidmap.umd.edu/api/resources?indicator=",
                       indicador,"&type=",
                       tipo,"&country=",
                       pais, ifelse(is.null(region),"",paste0("&region=",region)),
                       "&daterange=",
                       rango_fecha)
    }
    
    # request data from api
    request <- httr::GET(url = path)
    
    # make sure the content is encoded with 'UTF-8'
    response <- httr::content(request, as = "text", encoding = "UTF-8")

    # now we have a dataframe for use!
    coviddata <- jsonlite::fromJSON(response, flatten = TRUE) %>% data.frame()
    coviddata %>% return()
    
}

solicitame()


