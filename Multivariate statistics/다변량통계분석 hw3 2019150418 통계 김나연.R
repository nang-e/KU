# Q1
q1=matrix(c(5,2,2,2),byrow=F,nrow=2)
q1.pca=princomp(covmat=q1,cor=F)
q1.pca
q1.pca$loadings
q1.pca$sdev^2
6/7 # 0.8571429

q1.pca_c=princomp(covmat=q1,cor=T)
q1.pca_c
q1.pca_c$loadings
q1.pca_c$sdev^2
1.6324555+0.3675445 # 2
1.6324555/2 # 0.8162278

# Q2.
xbar=matrix(c(95.5,164.4,55.7,93.4,18,31.3),nrow=6)
S=matrix(c(3266,1344,732,1176,163,238,1344,722,324,537,80,118,732,324,179,281,39,57,1176,537,281,475,64,95,163,80,39,63,10,14,238,118,57,95,14,21),nrow=6,byrow=T)
rownames(S)=c('weight','body length','neck','girth','head length','head width');colnames(S)=c('weight','body length','neck','girth','head length','head width')
S

q2_a=princomp(covmat=S,cor=F)
q2_a
q2_a$loadings
eigen(S)
q2_a$sdev^2
4478.8244216/sum(q2_a$sdev^2) # 0.9584473

q2_b=princomp(covmat=S,cor=T)
q2_b
q2_b$loadings
q2_b$sdev^2
5.65109716/sum(q2_b$sdev^2) # 0.9418495

# 3번
q3=read.table('stock.dat',header=F)
colnames(q3)=c('jp','citibank','wells fargo','royal dutch','exxon mobil')
q3_b=princomp(q3,cor=T)
summary(q3_b)
q3_b$loadings
biplot(q3_b)

# 4번
q4=read.table('airpollution.dat',header=T,col.names=c('wind','solar radiation','co','no','no2','o3','hc'))
q4.pca=princomp(q4,cor=T)
q4.pca
q4.pca$sdev^2
Total.v=sum(q4.pca$sdev^2)
2.3367826/Total.v # 0.3338261
(2.3367826+1.3860007)/Total.v # 0.5318262
(2.3367826+1.3860007+1.2040659)/Total.v # 0.7038356
q4.pca$loadings
eigen(cor(q4))
