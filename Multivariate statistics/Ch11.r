# Section 11.4.1 An Application of Fisher's Discriminant Function;

skull = read.table("c://tibetan.dat")
attach(skull)

library(MASS)
group=c(rep(1,17), rep(2,15))


# Linear discriminant Function
# Calculate the linear discriminant function
dis1 = lda(skull, group,prior = c(1/2,1/2),CV=F)
dis1$scaling
# Note that the lda function provides the coefficients in a scaled version in which each discriminant function has mean zero and variance one.

# Classify observations using the linear discriminant function
class1 = predict(dis1)$class
class1
# Tabulate the performance of the discriminant function (p.254, table)
tab1 = table(group,class1)
tab1
# Calculate the misclassification rate
1 - sum(tab1[row(tab1)==col(tab1)]*(1/(2*apply(tab1,1,sum))))


# Calculate the linear discriminant function for the leaving-one-out method
dis2 = lda(skull, group,prior = c(1/2,1/2),CV=T)
# Classify observations using the leaving-one-out method
class2 = dis2$class
# Tabulate the performance using the leaving-one-out method (p. 25, table - Ch 11 lecture note)
tab2 = table(group,class2)
tab2
# Calculate the misclassification rate of the leaving-one-out method
1 - sum(tab2[row(tab2)==col(tab2)]*(1/(2*apply(tab2,1,sum))))


# Linear discriminant Function using other prior probabilities 
# Calculate the linear discriminant function
dis3 = lda(skull, group,prior = c(0.2,0.8),CV=F)
# Classify observations using the linear discriminant function
class3 = predict(dis3)$class
class3
# Tabulate the performance of the discriminant function 
tab3 = table(group,class3)
tab3
# Calculate the misclassification rate
1 - sum(tab3[row(tab3)==col(tab3)]*1/apply(tab3,1,sum)*c(0.2,0.8))



# Quadratic discriminant Function
# Calculate the linear discriminant function
dis4 = qda(skull, group,prior = c(1/2,1/2),CV=F)
# Classify observations using the quadratic discriminant function
class4 = predict(dis4)$class
class4
# Tabulate the performance of the discriminant function 
tab4 = table(group,class4)
tab4
# Calculate the misclassification rate
1 - sum(tab4[row(tab4)==col(tab4)]*(1/(2*apply(tab4,1,sum))))


# Calculate the quadratic discriminant function for the leaving-one-out method
dis5 = qda(skull, group,prior = c(1/2,1/2),CV=T)
# Classify observations using the leaving-one-out method
class5 = dis5$class
# Tabulate the performance using the leaving-one-out method  (p. 26, table - Ch 11 lecture note)

tab5 = table(group,class5)
tab5
# Calculate the misclassification rate of the leaving-one-out method
1 - sum(tab5[row(tab5)==col(tab5)]*(1/(2*apply(tab5,1,sum))))



