---
title: "Approximation of the distribution fuction of N(0,1)"
author: "zm"
date: "2020/3/27"
output: html_document
keywords: Monte Carlo,bookdown
abstract: |
    This is a report which complete approximation of the distribution fuction of N(0,1) by the Monte Carlo methods. In this report, I use table and figures to show the results.
---
# Introduction {#sec:intro} 

In this report, I experiment with the approximation at $n \in \{10^2, 10^3, 10^4\}$ at $t \in \{0.0, 0.67, 0.84, 1.28, 1.65, 2.32, 2.58, 3.09, 3.72\}$ to form a table for comparison. Further, repeat the experiment 100 times and draw box plots of the 100 approximation errors at each $t$ using **ggplot2** for each $n$.
The rest of this report is organized as follows: In Section \@ref(sec:math), I present mathematical equations used.In Section \@ref(sec:table), I present the table containing the experimental results and the true value for comparison.In Section \@ref(sec:figure), I present the box plots of the 100 approximation errors at each t for each $n$.In Section \@ref(sec:conclusion), I present the conclusion drawn from the results.

# Math Equations {#sec:math}
 
In this problem, i consider approximation the distribution
function of  $N(0,1)$ , 
\begin{align}
    \Phi(t) = \int_{-\infty}^t\frac{1}{\sqrt{{2\pi}}}e^
     {(-y^2)/2} \,{\rm d}y,
\end{align}
by the Monte Carlo methods:
\begin{align}
    \hat{\Phi}(t) = \frac{1}{n}\sum_{i=1}^n I(X_i \leq t),
\end{align}
where $X_i$ is a random sample from $N(0,1)$, and $I(.)$ is 
the indicator function.

# The table of comparison {#sec:table}

The following is the R code and the table of comparison:

```{r,eval=TURE}
library(tidyverse)
true_value<-c(pnorm(0),pnorm(0.67),pnorm(0.84),pnorm(1.28),
              pnorm(1.65),pnorm(2.32),pnorm(2.58),pnorm(3.09),
              pnorm(3.72))
t=c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72)

n1=10^2
z1=c(rep(0,9))
w1=matrix(0,9,n1)
y1=c(rnorm(n1,mean=0,sd=1))
for(k in 1:9)
{
  for(j in 1:n1)
  {w1[k,j]=sign(y1[j]<=t[k])}
  z1[k]=sum(w1[k,])/n1}

n2=10^3
z2=c(rep(0,9))
w2=matrix(0,9,n2)
y2=c(rnorm(n2,mean=0,sd=1))
for(k in 1:9)
{
  for(j in 1:n2)
  {w2[k,j]=sign(y2[j]<=t[k])}
  z2[k]=sum(w2[k,])/n2}

n3=10^4
z3=c(rep(0,9))
w3=matrix(0,9,n3)
y3=c(rnorm(n3,mean=0,sd=1))
for(k in 1:9)
{
  for(j in 1:n3)
  {w3[k,j]=sign(y3[j]<=t[k])}
  z3[k]=sum(w3[k,])/n3}

tb<-tibble(
  t=t,
  true_value=true_value,
  '100'=z1,
  '1000'=z2,
  '10000'=z3
)

library(knitr)
library(magrittr)
library(kableExtra)
library(callr)
library(webshot)
kable(tb, booktabs=TRUE) %>%
  kable_styling(bootstrap_options="striped",full_width=F) %>% 
 column_spec(1,bold=T)
```

```{r}
knitr::include_graphics("C:/Users/Lenovo/Desktop/table.png",dpi=NA)
```

# The figures of approximation errors {#sec:figure} 

I just gave the code of n=100, and the other code is similar to it, and you just replace it with a different n.The following is the R code and the figure:

when n=100,the box plots of the 100 approximation errors at each t.

```{r,eval=TRUE}
x=c(pnorm(0),pnorm(0.67),pnorm(0.84),pnorm(1.28),pnorm(1.65),
           pnorm(2.32),pnorm(2.58),pnorm(3.09),pnorm(3.72))
t=c(0.0,0.67, 0.84,1.28,1.65,2.32,2.58,3.09,3.72)
n=10^2
z=matrix(0,100,9)
w=matrix(0,9,n)
for(p in 1:100)
{ y=c(rnorm(n,mean=0,sd=1))
  for(k in 1:9)
  {
   for(j in 1:n)
  {w[k,j]=sign(y[j]<=t[k])}
z[p,k]=sum(w[k,])/n}}
z=as.data.frame(z)
r=c(z$V1,z$V2,z$V3,z$V4,z$V5,z$V6,z$V7,z$V8,z$V9)
e=c(rep(0.0,100),rep(0.67,100),rep(0.84,100),rep(1.28,100),
    rep(1.65,100),rep(2.32,100),rep(2.58,100),rep(3.09,100),
    rep(3.72,100))
q=data.frame(T=rep(0,100),X=0)
for(s in 1:900)
{q[s,2]=r[s]}
for(s in 1:900)
{q[s,1]=e[s]}
for(a in 1:100)
 { q[a,2]=q[a,2]-x[1]
 q[a+100,2]=q[a+100,2]-x[2]
 q[a+200,2]=q[a+200,2]-x[3]
 q[a+300,2]=q[a+300,2]-x[4]
 q[a+400,2]=q[a+400,2]-x[5]
 q[a+500,2]=q[a+500,2]-x[6]
 q[a+600,2]=q[a+600,2]-x[7]
 q[a+700,2]=q[a+700,2]-x[8]
 q[a+800,2]=q[a+800,2]-x[9]}
library(ggplot2)
library(lattice)
library(plyr)
library(Rmisc)
ggplot(data=q,aes(y=X,x=T,group=T))+
  geom_boxplot(fill="thistle")+
  labs(title="Boxplot of error at n=100",y="error",
       x="t")+theme(plot.title=element_text(size=13,hjust=0.5))
```

```{r,eval=TURE}
knitr::include_graphics("C:/Users/Lenovo/Desktop/100.png",dpi=NA)
```

when n=1000,the box plots of the 100 approximation errors at each t

```{r,eval=TURE}
knitr::include_graphics("C:/Users/Lenovo/Desktop/1000.png",dpi=NA)
```

when n=10000,the box plots of the 100 approximation errors at each t

```{r,eval=TURE}
knitr::include_graphics("C:/Users/Lenovo/Desktop/10000.png",dpi=NA)
```

## Conclusion {#sec:conclusion}

According to the above results, I come to the following conclusion: 
there are errors between the results of Monte Carlo simulation and the true value, but as $n$ increases, the errors decrease, and the simulation results are closer to the true value.

