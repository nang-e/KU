# Arithmetic sequence(등차수열)
seq(3,100,by=6) # seq(a,b,by=k) : a부터 b까지 k가 등차인 등차수열

# Arithmetic mean(등차수열 평균)
x=0.2:5.9 # colon은 1단위로 증가=> x= 0.2,1.2,2.2,3.2,4.2,5.2
mean(x)
sum(x)/length(x)
arith_mean=function(x){
  s=0
  n=length(x)
  for (i in 1:n){
    s=s+x[i]}
    return(s/n)
}
arith_mean(x)

# Fibonacci sequence
phi=(1+sqrt(5))/2
n=1:10
F=(phi^n-(-phi)^n)/sqrt(5)
F

Fn=rep(0,20)
Fn[1]=1;Fn[2]=1
for (i in 3:20){
  Fn[i]=Fn[i-1]+Fn[i-2]
}
Fn
Fn[which(Fn<=100)]

Fn=c(1,1);n=2
while (Fn[n]<=100){
  n=n+1
  Fn[n]=Fn[n-1]+Fn[n-2]
}
n

# Factorial function
n=6
n_fac=1
for(i in 1:10){
  n_fac=n_fac*i
}
n_fac
prod(1:10) # 누적곱

factorial(10) # 팩토리얼 함수의 10항
factorial(1:10) # 팩토리얼 함수의 1항부터 10항까지

nfact=numeric(n)
nfact[1]=1
for (i in 1:5){
  nfact[i+1]=nfact[i]*(i+1)
}
nfact

factorial(n)
factorial(1:n)

# The birthday problem
pbday=function(n){
  1-exp(lfactorial(365)-lfactorial(365-n)-n*log(365))
}
pbday(20)

# Combination
choose(98,50)/choose(100,50)

n_choose_r1=function(n,r){
  ncr=factorial(n)/{factorial(n-r)*factorial(r)}
  return(ncr)
}
n_choose_r1(98,50)/n_choose_r1(100,50)

choose(200,100)
n_choose_r1(200,100) # overflow -> NaN

n_choose_r2=function(n,r){
  ncr=10^{(lfactorial(n)-lfactorial(n-r)-lfactorial(r))/log(10)}
  return(ncr)
}
n_choose_r2(200,100)
n_choose_r2(100,50)

prod(101:200)/prod(1:100)

# 수열의 극한
n=1:10000000
tail((1+1/n)^n)
exp(1)

# limit of Fibonacci sequence = golden ratio
n=200
Fn=numeric(n)
Fn[1]=1;Fn[2]=1
for(i in 3:n){
  Fn[i]=Fn[i-2]+Fn[i-1]
}
Fn[3:n]/Fn[2:(n-1)]
(1+sqrt(5))/2

# Stirling's formula
stirling=function(n){
  form=sqrt(2*pi*n)*n^n*exp(-n)
  return(form)
}
stirling(52)
factorial(52)

factorial(1000)/stirling(1000)

lstirling=function(n){
  form=log(sqrt(2*pi*n))+n*log(n)-n
  return(form)
}
exp(lfactorial(1000)-lstirling(1000))

# Limit of sequence
upper=1000;n=1:upper
an=1/n;bn=1/n^2
partial_an=cumsum(an);partial_bn=cumsum(bn)
plot(n,partial_an,xlab='n',ylab='cumsum of 1/n',type='l',lty=2,lwd=2,col='red')
lines(n,partial_bn,lty=1,col='blue',lwd=2)
legend(800,5,c(expression(a[n]),expression(b[n])),lwd=c(2,2),lty=c(2,1),col=c('red','blue'))
sum(bn)
pi^2/6


n=c(1,5,13,10^2,10^3,10^4)
