# 10장-1
library(ngramr)
chagall=data.frame(ngram('Marc Chagall',corpus=c('fre_2019','eng_us_2019','ger_2019'),year_start = 1900,year_end = 1950))
library(reshape)
chagall=cast(chagall,Year~Corpus,value='Frequency')
chagall=data.frame(chagall[-1],row.names=chagall$Year)
head(chagall)
colnames(chagall)=c('American English','French','German')
plot(rownames(chagall),chagall[[1]],type='l',lty=1,xlab='Year',ylab='Relative Frequency',main='Chagall',ylim=c(0,max(chagall)))
for(i in 2:3){
  lines(rownames(chagall),chagall[[i]],type='l',lty=1,col=hcl.colors(3,palette = 'dark 3')[i])
}

x=c(1910,1917,1923,1941)
y=2e-08+seq(0,3e-08,1e-08)
z=c('프랑스\n유학','러시아\n예술감독','프랑스\n귀화','미국\n이주')
for(i in 1:length(x)){
  arrows(x,y,x,0,angle=25,length=0.03)
}
text(x,y,labels=z,cex=0.8,pos=3,font=4,col='pink')
rect(1910,0,1930,5e-08,border=NA,col=heat.colors(1,alpha=0.05))
rect(1933,0,1945,6e-08,border=NA,col=terrain.colors(3,alpha=0.1))
text(1920,5e-08,labels='독일 미술 부흥기',cex=0.8,font=4,pos=3)
text(1939,6e-08,labels='독일 나찌 정권',cex=0.8,font=4,pos=3)

painter=scan('C:/Users/user/Desktop/KU/3-1/데인/data/10_painter.txt',what='',sep='\n')
ng=data.frame(ngram(painter,corpus=c('fre_2019','eng_us_2019','ger_2019'),year_start=1900,year_end=1960))
ng=cast(ng[ng$Corpus=='ger_2019',],Year~Phrase,value='Frequency')
ng=data.frame(row.names=ng[,1],ng[-1])
plot(rownames(ng),ng[[1]],ylim=c(0,max(ng)),col='green',xlab='Year',ylab='Relative Frequency',main='German Corpus:Painters',type='l',lty=1)
for(i in 2:ncol(ng)){
  lines(rownames(ng),ng[[i]],type='l',lty=1,col=terrain.colors(10,alpha=1)[i],xlab='',ylab='',axes=NULL)
}
rect(1910,0,1930,0.9e-07,border=NA,col=heat.colors(1,0.1))
rect(1934,0,1945,1.2e-07,border=NA,col=heat.colors(2,0.1)[2])
text(c(1920,1938),c(0.9e-07,1.2e-07),labels=c('독일 미술 부흥기','독일 나찌 정권'),cex=0.8,pos=3,font=3)
legend('topleft',inset=0.02,cex=0.5,lty=1,legend=colnames(ng),col=terrain.colors(10,1))

# 10장-2
rus=read.delim('C:/Users/user/Desktop/KU/3-1/데인/data/10_Russian_suppression.txt',header=T,sep=',')
rus10=data.frame(ngram(rus$Russian.Name,corpus='rus_2019',year_start=1900,year_end=1990))
rus10=cast(rus10,Year~Phrase,value='Frequency')
rus10=data.frame(row.names=rus10[,1],rus10[-1])

plot(rownames(rus10),rus10[[1]],ylim=c(0,max(rus10)),type='l',lty=1,xlab='Year',ylab='Relative Frequency',main='Russian Corpus: Russian Suppression')
for(i in 2:3){
  lines(rownames(rus10),rus10[[i]],xlab='',ylab='',axes=NULL,type='l',lty=i)
}
legend('topleft',legend=gsub('[.]',' ',colnames(rus10)),lty=1:3,cex=0.6,inset=0.01)
x=c(1927,1936,1940,1988)
y=c(rus10[,3][rownames(rus10)==x[1]],rus10[,1][rownames(rus10)==x[2]],rus10[,3][rownames(rus10)==x[3]],rus10[,3][rownames(rus10)==x[4]])
segments(x,y,x,0,lty=2)
text(x,y,pos=3,labels=x,srt=90,cex=0.75,col='blue',font=3)

sup=read.delim('C:/Users/user/Desktop/KU/3-1/데인/data/10_USA_suppression.txt',header=F)
supindex=read.delim('C:/Users/user/Desktop/KU/3-1/데인/data/10_SuppressionIndex.txt',row.names=1,sep='\t')
usblack=read.delim('C:/Users/user/Desktop/KU/3-1/데인/data/10_SI_RemoveUSBlacklists.txt',row.names=1,sep='\t')
den1=density(supindex[[3]])
den2=density(usblack[[3]])
den1
plot(den1,main='1951~1960 vs. 1946~1950/1961~1965',xlab='Suppression Index',ylab='Density',xlim=c(min(c(den1$x,den2$x)),max(c(den1$x,den2$x))),ylim=c(min(c(den1$y,den2$y)),max(c(den1$y,den2$y))),type='n')
polygon(den1,border=NA,col=hcl.colors(1,palette = 'dark3',alpha=0.5))
polygon(den2,border=NA,col=hcl.colors(2,palette='dark3',alpha=0.5)[2])
legend('topleft',legend=c('+Black Lists','-Black Lists'),cex=0.6,pch=15,pt.cex=1.5,inset=0.02,col=hcl.colors(2,palette='dark3',alpha=0.5)[1:2])

a=supindex[[3]]
names(a)=rownames(supindex)
a[sup]
