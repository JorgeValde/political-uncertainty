%%parameters (beta = 1)
WB = 10;
al = .5;

out = zeros(100,5,'double');
for t = 0.01:.01:WB;
    b0 = [0:.01*t:t];
    bp5 = [0:.01*t:t];
    [b0,bp5]=meshgrid(b0,bp5);
    bnp5 = t - b0 - bp5;

    Z = -.5 - al + bnp5;
    X = - al + b0;
    Y = .5 - al + bp5;

    obj = WB.*((1./(1+exp(-X))).*(1./(1+exp(-Y))).*(1-1./(1+exp(-Z))) + (1./(1+exp(-X))).*(1./(1+exp(-Z))).*(1-1./(1+exp(-Y))) + (1./(1+exp(-Z))).*(1./(1+exp(-Y))).*(1-1./(1+exp(-X))) + (1./(1+exp(-X))).*(1./(1+exp(-Y))).*(1./(1+exp(-Z)))) - b0 - bp5 - bnp5;
    obj2 = obj.*(bnp5>=0);

    [value, location] = max(obj2(:));
    [R,C] = ind2sub(size(obj2),location);
    prob = (value+b0(R,C) + bp5(R,C) + bnp5(R,C))/WB;
    t2 = round(t*100);
    out(t2,:) = [t,prob,value,R,C];
end

     [v, l] = max(out(:,3));
     bribe0 = (out(l,5)-1)*l/10000
     bribep5 = (out(l,4)-1)*l/10000
     bribenp5 = l/100 - bribe0 -bribep5

    z = -.5 - al + bribenp5
    x = - al + bribe0
    y = .5 - al + bribep5
    
    lhs_y = (exp(-x)+exp(-z))/((1+exp(-x))*(1+exp(-z)))*(exp(-y)/((1+exp(-y))^2))
    lhs_x = (exp(-y)+exp(-z))/((1+exp(-y))*(1+exp(-z)))*(exp(-x)/((1+exp(-x))^2))
    lhs_z = (exp(-y)+exp(-x))/((1+exp(-y))*(1+exp(-x)))*(exp(-z)/((1+exp(-z))^2))


     
mesh(obj2);