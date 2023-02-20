simulasi.mc=function(x,y,n){ 
  #menghitung pdf dan cdf banyaknya permintaan 
  pdf=y/sum(y) 
  cdf=NULL 
  cdf[1]=pdf[1] 
  for(i in 2:length(y)){ 
    cdf[i]=cdf[i-1]+pdf[i] 
  } 
  #mencetak pdf dan cdf banyaknya permintaan 
  tabel1=data.frame(permintaan=x,freq=y,pdf,cdf) 
  print(tabel1)
  #merandom bilangan sebanyak n 
  random=runif(n)
  cat("\n") 
  cat("Nilai Random: ", round(random,2),"\n") 
  #membandingkan bilangan random dengan interval bilangan random 
  kesimpulan=NULL 
  cat("\n") 
  for(i in 1:n){ 
    if(round(random,2)[i]<=0.05){ 
      kesimpulan[i]=0 
    } 
    else if(round(random,2)[i]<=0.15){ 
      kesimpulan[i]=1 
    } 
    else if(round(random,2)[i]<=0.35){ 
      kesimpulan[i]=2 
    } 
    else if(round(random,2)[i]<=0.65){ 
      kesimpulan[i]=3 
    } 
    else if(round(random,2)[i]<=0.85){ 
      kesimpulan[i]=4 
    } 
    else { 
      kesimpulan[i]=5 
    } 
  } 
  #mencetak tabel ramalan banyaknya permintaan 
  tabel2=data.frame(Hari=c(1:n),Ban=kesimpulan) 
  print(tabel2) 
} 
permintaan=c("0","1","2","3","4","5") 
freq=c(10,20,40,60,40,30) 
n=10 
simulasi.mc(permintaan,freq,10) 