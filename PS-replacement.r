Esperanza = function(s,n)
{
  return((2*s - 1)*n)
}

PS_replacement = function(dias, ganancia, perdida)
{
 n = dias
 g = ganancia
 p = -perdida
 capital=0
 recorrido=vector()
 
 i = 0
 
 cambia = FALSE
 
 while(cambia == FALSE){
    a = runif(n-i)
 s=a[1]
 a=c(2:(n-i))
 
 for(j in 1:length(a)){
   if(Esperanza(s,(n-i)) < Esperanza(a[j],length(a)-j)){
     cambia = TRUE
   }
 }

 result = sample(c(ganancia,perdida), 1, replace = TRUE, prob = c(s, 1-s))
 capital = capital + result
 recorrido <- c(recorrido, capital)
 i=i+1
 }

 for(i in 1:n-i ){
 result = sample(c(ganancia,perdida), 1, replace = TRUE, prob = c(s, 1-s))
 capital = capital + result
 recorrido <- c(recorrido, capital)
 }
 return(recorrido)
}

PS_replacement(100,1,1)

