# Implementation of predictive neural net
# Based on Frieder Stolzenberger talk at WSU

n = 99;
M_res = normrnd(0,1/sqrt(n+1),n);
M_in = normrnd(0,1/sqrt(n+1),[n,1]);
x_res = normrnd(0,1/sqrt(n+1),[n,1]);
x_res0 = x_res;

function y = f(x)
  y = 2*sin(x)*cos(x+1) + sin(x+3);
end

xs = linspace(0.1,10,2*n);
as = arrayfun(@f, xs);

A = zeros([n+1,n+1]);

for k = 1:(n+1)
  A(k,:) = vertcat(as(k), x_res);
  x_res = M_in * as(k) + M_res * x_res;
end

M_out = A\transpose(as(2:n+2));

M = vertcat(transpose(M_out), [M_in, M_res]);

# test
x_res = x_res0;
x_in = as(1);
ys = zeros([1,n+20]);

y = vertcat(x_in, x_res);
for k = 1:(n+number_of_guesses)
  ys(k) = y(1);
  y = M * y;
end

# spectral radius of M. Should be approximately 1.
M_rad = max(abs(eig(M)));

# Error
err = norm(as(1:size(ys)(2)) - ys);

err
