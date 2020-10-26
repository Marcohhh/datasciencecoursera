





library(ggplot2)







x <- 1:10
bin <- pbinom(x,500,0.01)
poi <- ppois(x,500*0.01)

prob <- data.frame(x,bin,poi)

ggplot(prob, aes(x))+
  geom_line(aes(y=bin))+
  geom_line(aes(y=poi))

par(mfrow=c(2,1))  
plot(x,bin)
plot(x,poi)
dev.off()

plot(x,bin)
points(x,poi, col="red")



### Legge dei grandi numeri con distibuzione normale standard, l'asintoto delle medie converge a zero, mu.
n <- 1000
means <- cumsum(rnorm(n))/(1:n)

plot(1:n, means, type = "l")
lines(1:n,rep(0,1000))

### Legge dei grandi numeri con distibuzione bernulliana
means <- cumsum(sample(0:1,n,replace = T))/(1:n)

plot(1:n, means, type = "l")
lines(1:n,rep(0.5,1000))






### teorema centrale del limite

# CTL_bernulli: n di eventi iid che compongono il campione, p probabilità di ogni evento  
# dato n e p dimostra che la media del campione converge ad una distribuzione normale standard 

CTL_bernulli <- function(n,p){
  t <- 1:1000
  for (i in t){
    means[i] <- sum(sample(0:1,n,replace = T, prob = c(1-p,p)))/n
  }
  ctl <- (means-p)*sqrt(n)/sqrt(p*(1-p))
  
  col <- switch(n/10, "blue", "red","green")
  hist(ctl,col = col)
}

CTL_bernulli(10,0.9)

switch(n/10,hist(ctl, col="blue"), hist(ctl, col="red"), hist(ctl, col="green"), hist(ctl, col="yellow"))




