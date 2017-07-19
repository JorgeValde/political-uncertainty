#this code plots the PDF for the preferences of each of the three legislators around the zero line, 
#to the right of which the legislator votes for the status quo (B's favorite option)
v0 <- seq(-1.5, 1.5, 0.1) #x-axis, preference-space

s = 1                          #scale parameter to shift each pdf; this is negative of alpha value
lpdf0 <- dlogis(v0, location = 0+s, scale = 1, log = FALSE)      #middle / X
lpdfnp5 <- dlogis(v0, location = -0.5+s, scale = 1, log = FALSE) #foe / left / Z
lpdfp5 <- dlogis(v0, location = 0.5+s, scale = 1, log = FALSE)   #friend / right / Y

plot(v0,lpdf0,col=2, type="l",ylab="")            #col=2 is red for middle legislator
par(new=T)                                        #holds plot for next series
plot(v0,lpdfnp5,col=3, type="l",yaxt="n",ylab="") #col=3 is green for most opposed legislator
par(new=T)
plot(v0,lpdfp5,col=4, type="l",yaxt="n",ylab="")  #col=4 is blue for most friendly legislator
par(new=T)
abline(v=0)                                       #draws vertical black line


#Calculate potential bribe values for WB=8, al=-.3
WB=13
X=s
Y=0.5 +s
Z=-0.5+s

#solve for net position if each legislator was the only one with a NNB
fx <- function(x) ((exp(-Z)+exp(-Y))*exp(-x))/((1+exp(-Z))*(1+exp(-Y))*(1+exp(-x))^2)-1/WB
x <-uniroot(fx, c(0,1.5))
fz <- function(z) ((exp(-X)+exp(-Y))*exp(-z))/((1+exp(-X))*(1+exp(-Y))*(1+exp(-z))^2)-1/WB
z <-uniroot(fz, c(0,2))
fy <- function(y) ((exp(-X)+exp(-Z))*exp(-y))/((1+exp(-X))*(1+exp(-Z))*(1+exp(-y))^2)-1/WB
y <-uniroot(fy, c(0,2))

X=x$root #redefine X to be inclusive of optimal bribe
#compute value when x is only NNB
valx <- WB*((1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-Z))) + (1/(1+exp(-X)))*(1/(1+exp(-Z)))*(1-1/(1+exp(-Y))) + (1/(1+exp(-Z)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-X))) + (1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1/(1+exp(-Z))))- (X -s)

Y=y$root #redefine Y to be inclusive of optimal bribe
#compute value when y is only NNB
valy <- WB*((1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-Z))) + (1/(1+exp(-X)))*(1/(1+exp(-Z)))*(1-1/(1+exp(-Y))) + (1/(1+exp(-Z)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-X))) + (1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1/(1+exp(-Z))))- (X -s)

rm(list = ls(all = TRUE)) #clear workspace for safety's sake

#this code examines shape of first order condition and value function when we know
#that two of the variables are zero
s=1       #again, -alpha
WB =13
X=s
Y=0.5 +s
Z=-0.5+s

#examine shape of optimal value function; remember to shift value of Z
Z = seq(0,1,.05)
valz <- WB*((1/(1+exp(-X)))*(1/(1+exp(-Y))) + (1/(1+exp(-X)))*(1/(1+exp(-Z-.5))) + (1/(1+exp(-Z-.5)))*(1/(1+exp(-Y))) - 2*(1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1/(1+exp(-Z-.5)))) - (Z +.5 -s)
plot(Z,valz)

X = seq(0,2,.05)
valx <- WB*((1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-Z))) + (1/(1+exp(-X)))*(1/(1+exp(-Z)))*(1-1/(1+exp(-Y))) + (1/(1+exp(-Z)))*(1/(1+exp(-Y)))*(1-1/(1+exp(-X))) + (1/(1+exp(-X)))*(1/(1+exp(-Y)))*(1/(1+exp(-Z))))- (X -s)
plot(X,valx)

#set z vector in neighborhood of guess for optimal value of z
#z_foc is first order condition: will be zero at optimal value
z = seq(0,1,.05)
z_foc = ((exp(-X)+exp(-Y))*exp(-z-.5))/((1+exp(-X))*(1+exp(-Y))*(1+exp(-z-.5))^2) -1/WB
plot(z,z_foc)

x = seq(0,1,.05)
x_foc = ((exp(-Z)+exp(-Y))*exp(-x))/((1+exp(-Z))*(1+exp(-Y))*(1+exp(-x))^2)-1/WB
plot(x,x_foc)