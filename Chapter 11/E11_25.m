clear
%Initialize data
W1 = rand(2,1) - 0.5;
W2 = rand(1,2) - 0.5;
b1 = rand(2,1) - 0.5;
b2 = rand - 0.5;
a1 = zeros(2,1);

%Output the initial set
W1_0 = W1
b1_0 = b1
W2_0 = W2
b2_0 = b2

alfa = 0.2;    %learning rate
tol = 0.001;   %tol: tolerance
mse = 1;       %mse: mean square error
iter = 0;

figure;
while (mse > tol)
   mse = 0;
   i = 0;
   iter = iter + 1;
   for P = -2 : .1 :2
      i = i + 1;
      T = 1 + sin(pi*P/8);
    	a1 = logsig(W1*P + b1);
		a2 = purelin(W2*a1 + b2);
      mse = mse + (T - a2)^2;
      A(i) = a2;
            
		dlogsig = [(1 - a1(1))* a1(1) 0;0 (1 - a1(2))* a1(2)];
		s2 = -2 * (T - a2);
		s1 = dlogsig * W2' * s2;

		W2 = W2 - alfa * s2 * a1';
		W1 = W1 - alfa * s1 * P;
		b2 = b2 - alfa * s2;
      b1 = b1 - alfa * s1;
   end
   P = -2 : .1 : 2;
   if (mod(iter,10) == 0)
      plot(P,A,'g:')
   end
   hold on;
end

%Display in graph
P = -2 : .1 : 2;
T = 1 + sin(pi*P/8);
%figure;
plot(P,T,'r-',P,A,'b+')
title('Fig6.1 learning rate = 0.2, initial set #1');
text(-1.8,1.7,'red ---- original function');
text(-1.8,1.6,'blue ---- approximation');
text(-1.8,1.5,'green ---- intermediate results');
xlabel('P'), ylabel('Target vs. output');
W1
b1
W2
b2
iter  
