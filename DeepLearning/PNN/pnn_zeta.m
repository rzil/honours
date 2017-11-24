# Implementation of predictive neural net
# Based on Frieder Stolzenburg talk at WSU
# Applied to the Riemann-Zeta function on the critical strip

pkg load specfun;   # for the Riemann zeta function

# Optimal n seems to be around 400
# Why does the accuracy decrease when n is too large, eg 1000?
# Is this due to numerical errors?
n = 100;

s = 2.1;
W_res = complex(normrnd(0,1/sqrt(s*n),n-1),
                normrnd(0,1/sqrt(s*n),n-1));
W_in = complex(normrnd(0,1/sqrt(s*n),[n-1,1]),
               normrnd(0,1/sqrt(s*n),[n-1,1]));
x_res = complex(normrnd(0,1/sqrt(s*n),[n-1,1]),
                normrnd(0,1/sqrt(s*n),[n-1,1]));

x_res0 = x_res;

function y = f(x)
  y = sin(x);
  #y = zeta(x+1);
  #y = zeta(0.5 + i*(x+100));
end

xs = linspace(0,4*pi,4*n+1);
as = arrayfun(@f, xs);

A = zeros(n);

for k = 1:n
  A(k,:) = vertcat(as(k), x_res);
  x_res = W_in * as(k) + W_res * x_res;
end

# Gives this message
# warning: matrix singular to machine precision, rcond = 3.96047e-22
# Is there a better way to generate the random matrix?
# Or is there are more numerically stable solver?
W_out = A\transpose(as(2:n+1));

M = vertcat(transpose(W_out), [W_in, W_res]);

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

# make some predictions
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

dimensions = size(find(abs(A) > 1e-5))(2)

idx = find(abs(A) < 1e-5);

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
