clear
[x1 x2] = meshgrid(-3:0.1:3);
z = 1.5*(x1.^2) + (x1.*x2) + 1.5*(x2.^2) + 1.*x1 + 2.*x2 + 2;
figure;
contour(x1, x2, z);
hold on;

alfa = 0.4;
gamma_0 = 0.1;
eta = 1.5;
rou = 0.5;
ksi = 0.05;

x0 = [-1 -2.5]';
A = [3 1;1 3];
d = [1 2]';
fold = (0.5)*x0'*A*x0 + d'*x0;
dx = zeros(2,1);
xold = x0;
g0 = A*xold + d;

z = [0 0];
w = [0 0]; 
while abs(g0) > 0.001
	dx = gamma_0*dx - (1-gamma_0)*alfa*g0;
   xt = xold + dx;
   ft = (0.5)*xt'*A*xt + d'*xt;
   df = (ft - fold)/fold;
   if df > ksi
      alfa = alfa * rou;
      gamma = 0;
   elseif df <= ksi & df > 0
      z = [xold(1) xt(1)];
      w = [xold(2) xt(2)];
      plot(z,w);
      xold = xt;
      fold = ft;
   else
      z = [xold(1) xt(1)];
      w = [xold(2) xt(2)];
      plot(z,w);
      xold = xt;
      fold = ft; 
      alfa = alfa * eta;
	   gamma = 0.1;
   end 
  	g0 = A*xold + d;
end