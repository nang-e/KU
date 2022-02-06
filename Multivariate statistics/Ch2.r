# Read data (husb.txt) from your directory and attach in R for use.

husb = read.table("c://husb.dat",
                   col.names = c("hage", "hheight", "wage", "wheight", "hage_marriage"))
attach(husb)


# Univeriate plots
hist(wage)
stem(wage)
boxplot(wage)

# Figure 2.1.(a)
plot(hage,wage)
 
# Figure 2.5. Scatterplot matrix.
pairs(husb)

# Draw a 3D Scatterplot
library(rgl)
plot3d(hage,wage,hage_marriage) 
# You can right click to identify the observation ID
identify3d(hage,wage,hage_marriage)


########################################################################


# Normal probabiltiy plot for individual variables: Figure 2.23;
# Define the layout.
nf = layout(matrix(c(1:6),3,2,byrow=TRUE))
layout.show(nf)
p=ncol(husb)

# Draw normal probability plots
for(i in 1:p){qqnorm(husb[,i],xlab="Quantiles of Standard Normal",ylab=colnames(husb)[i]
)}



# Calculate the distance (formula 2.2);
Sx = cov(husb)
di2 = mahalanobis(husb, colMeans(husb), Sx)

# Draw the chi-square probability plot;
library(lattice) 
qqmath(di2,distribution = function(p) qgamma(p,shape=2.5,scale=0.5))


########################################################################


# Generate multivariate standard normal random numbers with n=50 and rho=0.5
n=50; p=5
mu=rep(0,p)
vmat=matrix(0.5,p,p)+0.5*diag(p)
library(MASS)
mvdata=mvrnorm(n,mu,vmat)



# Normal probabiltiy plot for individual variables: Figure 2.23;
# Define the layout.
nf = layout(matrix(c(1:6),3,2,byrow=TRUE))
layout.show(nf)

# Draw normal probability plots
for(i in 1:p){qqnorm(mvdata[,i],xlab="Quantiles of Standard Normal",ylab=paste("Var",i))}



# Calculate the distance (formula 2.2);
Sx = cov(mvdata)
di2 = mahalanobis(mvdata, colMeans(mvdata), Sx)

# Draw the chi-square probability plot;
library(lattice) 
qqmath(di2,distribution = function(p) qgamma(p,shape=2.5,scale=0.5))



