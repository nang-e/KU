# 데인 9장 연습문제 1번
install.packages('ngramr')
library(ngramr)
install.packages('reshape')
library(reshape)

ng <- ngram(c('Charles Dickens','Merry Christmas'))
Dickens=cast(ng,Year~Phrase,value='Frequency')
Dickens=data.frame(Dickens[-1],row.names=Dickens$Year)
str(Dickens)
Dickens[1:3,]
tail(Dickens,3)

# 데인 9장 연습문제 2번
plot(rownames(Dickens),Dickens$Charles.Dickens,type='l',lty=1,ylim=c(0,max(Dickens)),main='Charles Dickens vs. Merry Christmas',xlab='Year',ylab='Relative Frequency',col='blue')
lines(rownames(Dickens),Dickens$Merry.Christmas,col='red',type='l',lty=2)
legend('topright',gsub('[.]',' ',colnames(Dickens)),col=c('blue','red'),lty=1:2,inset=0.02,cex=0.8) #inset은 여백 설정, cex는 폰트크기, gsub를 사용하지 않는다면 범례의 label에 .이 여전히 포함되어 있을 것임.
## plot(rownames(Dickens),Dickens[1])하면 오류가 뜸-> 대괄호 안에 숫자만 쓰면 rownames는 벡터고 Dickens[1]은 데이터프레임이어서 오류가 뜸.

fame=read.delim('C:/Users/user/Desktop/KU/3-1/데인/data/09_fame.txt',header=T)
ng=data.frame(ngram(fame$Name[1],year_start=1809,year_end=1809+99))
ng=data.frame(age=0:99,ng[-1])
ng=cast(ng,age~Phrase,value='Frequency')
ng
apply(ng[-1],2,median)
ng[-1]
library(reshape)
library(ngramr)
for(i in 1:nrow(fame)){
  ng=data.frame(ngram(fame$Name[i],year_start=fame$Birth.Year[i],year_end=fame$Birth.Year[i]+99))
  ng=rbind(df,ng)
}
df
age=as.data.frame(rep(0:99,nrow(fame)))
df=rbind(age,df)
df
cut(fame$Birth.Year,breaks=seq(1800,1920,20))
table(cut(fame$Birth.Year,breaks=seq(1800,1920,20),labels=paste0(seq(1801,1901,20),'~',seq(1820,1920,20))))
fame$Birth.Year
sort(table(fame$Occupation),decreasing=T)
