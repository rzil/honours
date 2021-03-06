# Implementation of predictive neural net
# Based on Frieder Stolzenburg talk at WSU

pkg load statistics;

n = 99;

M_res = normrnd(0,1/sqrt(n+1),n);
M_in = normrnd(0,1/sqrt(n+1),[n,1]);
x_res = normrnd(0,1/sqrt(n+1),[n,1]);

x_res0 = x_res;

function y = f(x)
  y = sin(x);
end

xs = linspace(0,8*pi,8*n+1);
as = arrayfun(@f, xs);

A = zeros([n+1,n+1]);

for k = 1:(n+1)
  A(k,:) = vertcat(as(k), x_res);
  x_res = M_in * as(k) + M_res * x_res;
end

M_out = A\transpose(as(2:n+2));

M = vertcat(transpose(M_out), [M_in, M_res]);

# test
number_of_guesses = 200;
x_res = x_res0;
ys = zeros([1,n+number_of_guesses]);

y = vertcat(as(1), x_res);
for k = 1:n
  ys(k) = y(1);
  y = M * y;
end

# save this as new input
x_in = y;

for k = n+1:n+number_of_guesses
  ys(k) = y(1);
  y = M * y;
end

# spectral radius of M. Should be approximately 1.
M_rad = max(abs(eig(M)));

# Error
err = norm(as(1:size(ys)(2)) - ys);

# Print results
err
M_rad

# Reduction of M
[V,D] = eig(M);
invV = inv(V);
U = invV * x_in;
A = V(1,:) .* transpose(U);

dimensions = size(find(A > 1e-5))(2)

idx = find(A < 1e-5);

invV(idx,:) = [];

x_hat = invV * x_in;

D_hat = diag(D);
D_hat(idx) = [];

w = V(1,:);
w(idx) = [];

w
D_hat
x_hat

# plot eigenvalues of M
#plot(eig(M),'o')

# plot(sort(abs(eig(M))) .^ 2)
