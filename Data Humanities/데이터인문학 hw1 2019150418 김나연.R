# 1.
a=read.delim("C:/Users/user/Downloads/Q1.txt",sep="",header=T)
row.names(a)=letters[1:8]
a
str(a)

# 2.
b=a[order(-rank(a$C),a$A),]
b

# 3.
b$C
b$C=factor(b$C,levels=c('banana','orange','apple mango'))
b$C

# 4.
c=b['C']
c

# 5.
c$C=as.vector(c$C)
d=rownames(c)
names(d)=c$C
d=sort(d,decreasing = T)
d
str(d)
