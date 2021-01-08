# Tienen un data frame con 3 columnas, Columna Año, Columna mes, y Columna semana, 
# se quiere obtener la fecha de esa semana, por ejemplo, tienen 2020, mes 1, semana 1, 
# entonces el resultado debe de ser la fecha de  semana debe de ser 31 de diciembre de 2017, 
# que es cuando dio inicio dicha semana
## 29 de diciembre de 2019 **

df=data.frame(
        ano=c(2020,2019),
        mes=c(1,4),
        semana=c(1,17)
)
df


get_fecha=function(df){
        Fecha=rep("",nrow(df))
        for (i in 1:nrow(df)) {
                a=df[i,]
                fecha=as.Date(paste(a[1],a[2],1,sep = "-"))
                x=lubridate::epiweek(fecha)
                diff_=x-a[3]
                fecha=fecha+as.numeric(-diff_)*7
                b=lubridate::wday(fecha)-1
                fecha=fecha-b
                Fecha[i]=as.character(fecha)
        }
        df=data.frame(df,Fecha=Fecha)
        return(df)
}
get_fecha(df)
