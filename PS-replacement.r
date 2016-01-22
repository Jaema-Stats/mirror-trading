esperanza = function(s,n)
{
  return((2*s - 1)*n)
}

cambiar = function(dias, i, totales, specialist)
{
  cambia = FALSE
  totales = totales[-1]
  
  for(j in 1:length(totales)){
    if(esperanza(specialist,(dias-i)) < esperanza(totales[j],length(totales)-j)){
      cambia = TRUE
    }
  }
  
  return(cambia)
}
PS_replacement = function(dias, ganancia, perdida)
{
  capital = 0
  recorrido = vector()
  i = 0
  
  totales = runif(dias-i)
  
  specialist = totales[1]
  
  while(i < dias){
    
    result = sample(c(ganancia,perdida), 1, replace = TRUE, prob = c(specialist, 1- specialist))
    recorrido <- c(recorrido, result)
    capital = capital + result
    
    if(cambiar(dias, i, totales, specialist) == TRUE){
      totales = runif(dias-i)
      specialist = totales[1]
    }
    
    i = i + 1
  }
  
  return(capital)
}


PS_replacement(10,1,-1)
