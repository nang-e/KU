# 피보나치 수열
n=20
Fn=numeric(n) # numeric(n): 실수 중에 n개의 벡터를 추출
Fn[1]=1
Fn[2]=1
for (i in 3:n){
  Fn[i]=Fn[i-1]+Fn[i-2]
}
Fn

# 처음으로 피보나치 수열이 100 이상인 경우 n 구하기


# Fibonacci, cumulatively up to n=50
n=50
Fn=rep(0,n)
Fn[c(1,2)]=1
for(i in 3:n) Fn[i]=Fn[i-1]+Fn[i-2]
Fn

# Factorial
n=10
factorial_ex=1
for (i in 1:n){
  factorial_ex=factorial_ex*i
}
factorial_ex

# prod() : 전체곱(1부터 n까지)
prod(1:n)  # cumprod() : 누적곱 

# Combination
com_ex=function(n,r){
  nCr=factorial(n)/(factorial(r)*factorial(n-r))
return(nCr)
}
com_ex(98,50)/com_ex(100,50)
