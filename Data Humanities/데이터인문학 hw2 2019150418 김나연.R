# 1번
# 1-1
Q1_irregular=read.delim('Q1_irregular.txt',header=T,sep='\t',row.names=1,quote=NULL)
Q1_regular=read.delim('Q1_regular.txt',header=T,sep='\t',row.names=1,quote=NULL)
class(Q1_irregular)
class(Q1_regular)

# 1-2
irregular_1800=apply(Q1_irregular[1:100,],2,median)
irregular_1900=apply(Q1_irregular[101:200,],2,median)
regular_1800=apply(Q1_regular[1:100,],2,median)
regular_1900=apply(Q1_regular[101:200,],2,median)

irregular_1800 # 불규칙형의 1800년대 어휘별 중앙값
irregular_1900 # 불규칙형의 1900년대 어휘별 중앙값
regular_1800 # 규칙형의 1800년대 어휘별 중앙값
regular_1900 # 규칙형의 1900년대 어휘별 중앙값

# 1-3
ratio_1800=regular_1800/(regular_1800+irregular_1800)
ratio_1900=regular_1900/(regular_1900+irregular_1900)
df_ratio=data.frame(ratio_1800,ratio_1900,row.names=names(ratio_1800))
df_ratio

# 1-4
plot(df_ratio$ratio_1800,df_ratio$ratio_1900,type='n',xlim=c(0,1),ylim=c(0,1),xlab='Regularity Ratio 1800 ~ 1899',ylab='Regularity Ratio 1900 ~ 1999')
text(df_ratio$ratio_1800,df_ratio$ratio_1900,labels=rownames(df_ratio),cex=0.9)
abline(a=0,b=1,lty=2)

# 2번
# 2-1
Q2_fame=read.delim('Q2_fame.txt',header=T,sep='\t',quote=NULL)
Q2_fame

# 2-2
library(ngramr)
name=Q2_fame$Name
year=Q2_fame$Birth.Year

q2_2=data.frame()
for(i in 1:34){
  q2_2=rbind(q2_2,data.frame(ngram(name[i],year_start=year[i],year_end=year[i]+99)))
}
q2_2

# 2-3
age=rep(0:99,34)
occupation=rep(Q2_fame$Occupation,each=100)
q2_2.copy=cbind(q2_2,age,occupation)

pol=q2_2.copy[q2_2.copy$occupation=='Politician',]
nopol=q2_2.copy[q2_2.copy$occupation!='Politician',]

pol_md=rep(0,100) ; nopol_md=rep(0,100)
for(i in 1:100){
  pol_md[i]=median(pol$Frequency[pol$age==i-1])
}
pol_md # Occupation이 Politician인 집단의 나이별 중앙값 

for(i in 1:100){
  nopol_md[i]=median(nopol$Frequency[nopol$age==i-1])
}
nopol_md # Occupation이 Politician이 아닌 집단의 나이별 중앙값

# 2-4
plot(0:99,pol_md,xlab='Age',ylab='Median Frequency',type='l')
lines(0:99,nopol_md,type='l',lty=2)
legend('topleft',c('Politician','Non-Politician'),lty=1:2,cex=0.6,inset=0.02)
