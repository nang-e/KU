# 8-2
setwd('C:/Users/user/Desktop/KU/3-1/데인/data')
burn=data.frame(ngram(c('burned','burnt')))
burn=data.frame(burn[,-1],row.names=burn$Year)
library(reshape)
burn=cast(burn[,-1],Year~Phrase,value='Frequency')
plot(burn)
plot(rownames(burn),burn$burned,type='l',lty=1,ylim=c(0,max(burn)),col='red')
lines(rownames(burn),burn$burnt,type='l',lty=2,col='blue')
legend('topright',colnames(burn),lty=1:2,col=c('red','blue'),inset=0.02)

q3=data.frame(ngram(c('speeded','sped')))
q3=cast(q3,Year~Phrase,value='Frequency')
q3=data.frame(q3[,-1],row.names=q3[,1])
q3
plot(rownames(q3),q3$speeded,xlab='Year',ylab='Relative Frequency',main='speeded vs. sped',type='l',lty=1,ylim=c(0,max(q3)),col='red')
lines(rownames(q3),q3$sped,type='l',lty=2,col='blue')
legend('topleft',c('speeded','sped'),lty=1:2,col=c('red','blue'),inset=0.02)

# 8-3
verbs=read.delim('08_verbs.txt',header=T,sep='\t')
library(ngramr)
past=data.frame(ngram(c(verbs$Regular,verbs$Irregular)))
reg=verbs$Regular
irreg=verbs$Irregular
a=data.frame()
b=data.frame()
for(i in 1:23){
  a=rbind(a,data.frame(ngram(reg[i])))
}
for(i in 1:23){
  b=rbind(b,data.frame(ngram(irreg[i])))
}
library(reshape)
a=cast(a,Year~Phrase,value='Frequency')
b=cast(b,Year~Phrase,value='Frequency')
a['Total']=rowSums(a[-1])
b['Total']=rowSums(b[-1])
plot(a$Year,a$Total,type='l',lty=1,lwd=2,col='blue',xlab='Year',ylab='Relative Frequency',main='Regular vs. Irregular',ylim=c(0,max(a[-1],b[-1])))
lines(b$Year,b$Total,type='l',lty=2,lwd=2,col='hotpink')
legend('topright',c('Regular','Ireegular'),lty=1:2,col=c('blue','hotpink'),cex=0.8)
test=b[1:5,2:4]
test
apply(test,1,mean)
apply(test,2,mean)

plot(a$Year,apply(a[c(-1,-25)],1,mean),type='l',ylim=c(0,max(apply(a[c(-1,-25)],1,mean),apply(b[c(-1,-25)],1,mean))))
reg.median=apply(a[c(-1,-25)],1,median)
reg.change=diff(reg.median)/reg.median[-length(reg.median)]
plot(a$Year[-1],reg.change,type='l')
reg.median[-length(reg.median)]
length(reg.median)
a$Year
abline(h=0,lty=2,col='purple',lwd=2)
abline(v=1900,lty=3,col='hotpink',lwd=2)

a
regular.early=data.frame(a[a$Year<=1825,-1],row.names=a$Year[a$Year<=1825])
irregular.early=data.frame(b[b$Year<=1825,-1],row.names=b$Year[b$Year<=1825])
regular.late=data.frame(a[a$Year>=1994,-1],row.names=a$Year[a$Year>=1994])
irregular.late=data.frame(b[b$Year>=1994,-1],row.names=b$Year[b$Year>=1994])

early=data.frame(regular=apply(regular.early,2,median),row.names=colnames(regular.early))
early['irregular']=apply(irregular.early,2,median)
early['ratio']=early$regular/rowSums(early)
