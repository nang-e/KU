# 1번
## A영역
setwd('C:/Users/user/Documents/Nang/R')
Q1=read.delim('Q1.txt',quote=NULL,header=F,col.names='Q1_name',sep='\n')
library(ngramr)
q1a=data.frame(ngram(Q1$Q1_name,year_start=1900,year_end=1970,corpus="ger_2019"))
library(reshape)
q1a=cast(q1a,Year~Phrase,value='Frequency') ; q1a=data.frame(q1a[-1],row.names=q1a$Year)
Median=apply(q1a,1,median) ; q1a=cbind(q1a,Median)

par(mar=c(4,4,1,6))
plot(rownames(q1a),q1a$Henri.Matisse,type='l',lty=3,xlab='Year',ylab='Relative Frequency',las=3,ylim=c(0,max(q1a))) # Henri
par(new=T) ; plot(rownames(q1a),q1a$Marc.Chagall,axes=F,xlab='',ylab='',type='l',lty=3,ylim=c(0,max(q1a))) # Marc
par(new=T) ; plot(rownames(q1a),q1a$Pablo.Picasso,axes=F,xlab='',ylab='',type='l',lty=3,ylim=c(0,max(q1a))) # Pablo
par(new=T) ; plot(rownames(q1a),q1a$Paul.Gauguin,axes=F,xlab='',ylab='',type='l',lty=3,ylim=c(0,max(q1a))) # Paul
par(new=T) ; plot(rownames(q1a),q1a$Piet.Mondrian,axes=F,xlab='',ylab='',type='l',lty=3,ylim=c(0,max(q1a))) # Piet
par(new=T) ; plot(rownames(q1a),q1a$Wassily.Kandinsky,axes=F,xlab='',ylab='',type='l',lty=3,ylim=c(0,max(q1a))) # Wassily
par(new=T) ; plot(rownames(q1a),q1a$Median,type='l',lty=1,lwd=5,axes=F,xlab='',ylab='',ylim=c(0,max(q1a))) # Median

## B영역
axis(side=4,at=q1a[71,],labels=gsub('[.]',' ',colnames(q1a)),las=1,cex.axis=0.7)

# 2번
q2=read.delim('Q2.txt',header=T,sep='\t',quote=NULL)
q2.t=table(cut(q2$YEAR,breaks=seq(1800,1960,40),labels=paste0(seq(1801,1921,40),'~','\n',seq(1840,1960,40))))
q2.p=round(prop.table(q2.t)*100,2)
md=barplot(q2.p)
par(mar=c(5,4,3,1))
barplot(q2.p,
        las=3,ylim=c(0,30),axes=F,
        cex.names=0.9,cex.axis=1,
        ylab='Relative Frequency')
axis(side=2,at=seq(0,30,5),labels=paste0(seq(0,30,5),'%'),cex.axis=0.8)
text(md,q2.p,labels=paste0(q2.p,'%'),pos=3)
