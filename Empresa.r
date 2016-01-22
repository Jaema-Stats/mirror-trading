


#FUNCIÓN TOTAL

Mirror_Trading = function(dias, ganancia, perdida)
{
  predictionSpecialist = function(especialista)
  {
    especialista <- runif(1, min = 0, max= 1)
  }
  
  outcomeEvent = function(especialista, recorrido, ganancia, perdida)
  {
    result = sample(c(ganancia,perdida), 1, replace = TRUE, prob = c(especialista, 1- especialista))
    recorrido <- c(recorrido, result)
  }
  
  specialist = 0
  recorrido <- c(0)
  i = 0
  
  predictionSpecialist(specialist)
  
  while(i < dias){
    if(specialist > 0.5){
      outcomeEvent(specialist, recorrido, ganancia, perdida)
      i = i + 1
    }else{
      outcomeEvent(specialist, recorrido, ganancia, perdida)
      predictionSpecialist(specialist)
      i = i + 1
    }
  }
  
  plot(recorrido, type = "l", xlab = "Días", ylab = "Capital")
  abline(h = 0, lty = 3, col = 2)
  
  print("La ganancia total es de:")
  print(sum(recorrido))
}
