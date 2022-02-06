verbs=read.delim('C:/Users/user/Desktop/KU/3-1/데인/data/08_verbs.txt',header=T)
ng=data.frame()
for(i in 1:nrow(verbs)){
  ng=rbind(ng,data.frame(ngram(verbs$Regular[i])))
}


# 연습문제 1번
burn=data.frame(ngram(c('burned','burnt')))
head(burn)
str(burn)

# 연습문제 2번
library(reshape)
burn=cast(burn,Year~Phrase,value='Frequency')
burn=data.frame(burn[-1],row.names=burn$Year)
head(burn)
tail(burn)

plot(burn$burned,burn$burnt,xlab='Burned',ylab='Burnt',las=0)
plot(rownames(burn),burn$burned,type='l',lty=1,ylim=c(0,max(burn)),col='blue')
lines(rownames(burn),burn$burnt,type='l',lty=2,col='hotpink')
legend('topright',colnames(burn),inset=0.02,lty=c(1,2),col=c('blue','hotpink'))

# 연습문제 3번
q3=data.frame(ngram(c('speeded','sped')))
q3=cast(q3,Year~Phrase,value='Frequency')
q3=data.frame(q3[-1],row.names=q3$Year)
plot(rownames(q3),q3$speeded,xlab='Year',ylab='Relative Frequency',ylim=c(0,max(q3)),type='l',lty=1,col='green')
lines(rownames(q3),q3$sped,type='l',lty=3,col='skyblue')
legend('topleft',c('Speeded','Sped'),lty=1:2,col=c('green','skyblue'),inset=0.02)
