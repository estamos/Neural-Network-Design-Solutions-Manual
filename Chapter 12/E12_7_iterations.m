x0 = [-1; -2.5];
A = [3 1; 1 3];
d = [1 2];
 
F_x0 = 0.5 * x0' * A * x0 + d * x0 + 2

x1t = [0.62; -0.16];

F_x1t = 0.5 * x1t' * A * x1t + d *x1t  + 2

x2t = [-0.1; -1.2];

F_x2t = 0.5 * x2t' * A * x2t + d *x2t  + 2

x3t = [-0.55; -0.85];

F_x3t = 0.5 * x3t' * A * x3t + d *x3t  + 2