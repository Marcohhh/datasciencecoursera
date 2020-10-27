





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










x <-  seq(50, 150,1)
plot(x = x,
     y = dnorm(x,100,15),
     type = "l",                            # Specify that you want to plot a line graph
     lwd = 2,                               # Thickness of line
     lty = 1,
     col = "black",                          # Colour of line
     xlim=c(40,160),                          # Set limit of x-axis
     frame.plot=TRUE,                       # Do plot the frame of the graph
     xlab=" ",                    # Title for x-axis
     ylab= " ",                             # Title for y-axis
     axes = FALSE,                          # Don't plot the scales by default
     main=paste(" "))     # Main title
title(main="Area Under a Normal Distribution") # "line" adjusts distance to axis, cex.lab adjusts size
title(xlab="Standard Deviation") #  # "line" adj
Axis(side=1,at=seq(40, 160, by = 10))
Axis(side=2, labels=FALSE)

# Create data for the area to shade +-3SD
cord.1x <-  c(80,seq(80,120,0.01),120) 
cord.1y <-  c(0,dnorm(seq(80,120,0.01)),0) 
# Make a curve
#curve(dnorm(x,100,15), xlim=c(40,160), main='Standard Normal',lwd=2) 
# Add the shaded area.
polygon(cord.1x,cord.1y,col='grey60')
#dev.off()




dat <-  data.frame(x, y=dnorm(x,100,15))
ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_line()+
  geom_area(mapping = aes(x = ifelse(x>100 & x< 115 , x, 0)), fill = "darkgrey") +
  scale_x_continuous(limits=c(40,160), breaks = c(100,115))+
  scale_y_continuous(labels = NULL)+
  ylab("QI distribution")
  

x <- seq(0,1,0.01)
dat <- data.frame(x,y=2*x)
ggplot(data=dat, mapping = aes(x=x,y=y))+
  geom_line()+
  geom_area(mapping = aes(x = ifelse(x>0.2 & x< 0.6 , x, 0)), fill = "darkgrey")+
  scale_x_continuous(limits=c(0,2), breaks = c(0.2,0.6))+
  scale_y_continuous(labels = NULL, limits=c(0,2))
  


