#this code plots the three PDFs around the zero line, 
#to the right of which the legislator votes for the status quo (B's favorite option)

s = -.0                          #scale parameter to shift each pdf; add negative to alpha value
lpdf0 <- dlogis(v0, location = 0+s, scale = 1, log = FALSE)      #middle / X
lpdfnp5 <- dlogis(v0, location = -0.5+s, scale = 1, log = FALSE) #friend / left / Z
lpdfp5 <- dlogis(v0, location = 0.5+s, scale = 1, log = FALSE)   #foe / right / Y

plot(v0,lpdf0,col=2, type="l")   #col=2 is red
par(new=T)                       #holds plot for next series
plot(v0,lpdfnp5,col=3, type="l") #col=3 is green
par(new=T)
plot(v0,lpdfp5,col=4, type="l")  #col=4 is blue
par(new=T)
abline(v=0)                      #draws vertical black line