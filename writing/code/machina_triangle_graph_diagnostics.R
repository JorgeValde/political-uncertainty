#values from optim routine with alpha = .5 and WB=10
x = 1.43007
y = 1.43007
z = 0

#some objects of interest
prob_y = (exp(-x)+exp(-z))/((1+exp(-x))*(1+exp(-z)))*(exp(-y)/((1+exp(-y))^2))
prob_x = (exp(-y)+exp(-z))/((1+exp(-y))*(1+exp(-z)))*(exp(-x)/((1+exp(-x))^2))

pdf_Y = (exp(-y)/((1+exp(-y))^2))
pdf_X = (exp(-x)/((1+exp(-x))^2))
cdf_x = 1/(1+exp(-x))
cdf_y = 1/(1+exp(-y))
cdf_z = 1/(1+exp(-z))


#'L' denotes left side of equation, 'R' denotes right side of equation from 
#development just below equation (27) in GS-extension.pdf
t1L = (exp(-x-z)+exp(-y-x))/(1+exp(-x))
t1R = (exp(-x-y)+exp(-y-z))/(1+exp(-y))

t2L = exp(-x-z)+exp(-y-y-x)
t2R = exp(-y-z)+exp(-x-x-y)

t3L = exp(-x-z) - exp(-y-z)
t3R = exp(-x-x-y) - exp(-y-y-x)

t4L = exp(-z)*(exp(-x) - exp(-y))
t4R = exp(-x)*exp(-y)*(exp(-x) - exp(-y))

t4L2 = exp(-z)
t4R2 = exp(-x)*exp(-y)

t5L = exp(-z)
t5R = exp(-x-y)

#Right hand side should equal 1 for legislators with non-zero bribes;
#Right hand side should be less than 1 for those with zero bribes b/c lambda > 0
lhs_y = WB*(exp(-x)+exp(-z))/((1+exp(-x))*(1+exp(-z)))*(exp(-y)/((1+exp(-y))^2))
lhs_x = WB*(exp(-y)+exp(-z))/((1+exp(-y))*(1+exp(-z)))*(exp(-x)/((1+exp(-x))^2))
lhs_z = WB*(exp(-y)+exp(-x))/((1+exp(-y))*(1+exp(-x)))*(exp(-z)/((1+exp(-z))^2))