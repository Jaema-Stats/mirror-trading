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
PS_replacement = function(dias, ganancia, perdida)
{
  capital = 0
  recorrido = vector()
  i = 0
  
  specialist = runif(1)
  
  while(i < dias){
    
    result = sample(c(ganancia,perdida), 1, replace = TRUE, prob = c(specialist, 1- specialist))
    recorrido <- c(recorrido, result)
    capital = capital + result
    
    if(cambiar(dias, i, specialist) == TRUE && specialist < 0.9){
      specialist = runif(1)
    }
    
    i = i + 1
  }
  
  return(capital)
}

z=0
for(i in  1:1000)
{
  z[i]=Mirror_Trading2(10,1,-1) 
}

v=0
for(i in  1:1000)
{
  v[i]=PS_replacement(10,1,-1) 
}

mean(z)
mean(v)

