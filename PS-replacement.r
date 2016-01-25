esperanza = function(s,n)
{
  return((2*s - 1)*n)
}

cambiar = function(dias, i, specialist)
{
  cambia = FALSE
  
  for(j in 1:(dias-i)-1){
    if(esperanza(specialist,(dias-i)) < esperanza(runif(1),(dias-i)-j)){
      cambia = TRUE
    }
  }
  
  return(cambia)
}

cambiar2 = function(dias, i ,specialist)
{
  
  if(dias <= 10){
    if(i < 6 && specialist < 0.7){
      specialist = runif(1)
    }
    else{
      if(i > 5 && specialist < 0.5){
        specialist = runif(1)
      }
    }
  }
  else{
    if(cambiar(dias, i, specialist) == TRUE && specialist < 0.9){
      specialist = runif(1)
    }
  }
  
  return(specialist)
}
PS_replacement = function(dias, ganancia = 1, perdida = -1)
{
  capital = 0
  recorrido = vector()
  i = 0
  
  specialist = runif(1)
  
  while(i < dias){
    print(i)
    result = sample(c(ganancia,perdida), 1, replace = TRUE, prob = c(specialist, 1- specialist))
    recorrido <- c(recorrido, result)
    capital = capital + result
    
    specialist = cambiar2(dias, i, specialist)

    i = i + 1
  }
  
  #plot(recorrido, type = "l", xlab = "Días", ylab = "Capital")
  #abline(h = 0, lty = 3, col = 2)
  
  #print("El capital el día 100 es de:")
  return(capital)
}

#v=PS_replacement(10,1,-1) 

z=0
for(i in  1:1000)
{
  z[i]=Mirror_Trading2(10,1,-1) 
}

v=0
for(j in  1:1000)
{
  v[j]=PS_replacement(10,1,-1) 
}

mean(z)
mean(v)


