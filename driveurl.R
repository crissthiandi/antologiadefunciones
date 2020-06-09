#extracción de id

getidgoogle= function(link) {
  a=regexpr("/d/",link)
  a=as.integer(a)
  b=regexpr("/view",link)
  b=as.integer(b)
  id=substring(link,a+3,b-1)
  return(id)
}

#leer desde internet
getcsv_drive= function(link){
  id=getidgoogle(link = link)
  a=read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
  return(a)
}

