


#FUNCIÓN TOTAL

Mirror_Trading2 = function(dias, ganancia, perdida)
{
  capital = 0
  specialist = 0
  i = 0
  
  specialist <- runif(1, min = 0, max= 1)
  result = sample(c(ganancia,perdida), 1, replace = TRUE, prob = c(specialist, 1- specialist))
  recorrido <- c(result)
  i = i + 1
  
  while(i < dias){
    result = sample(c(ganancia,perdida), 1, replace = TRUE, prob = c(specialist, 1- specialist))
    capital = capital + result
    recorrido <- c(recorrido, capital)
    
    if(specialist < 0.9){
      specialist <- runif(1, min = 0, max= 1)
    }
    
    i = i + 1
  }
  
  #plot(recorrido, type = "l", xlab = "Días", ylab = "Capital")
  #abline(h = 0, lty = 3, col = 2)
  
  #print("El capital el día 100 es de:")
  return(capital)
  #print(specialist)
}
  
