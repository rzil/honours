# Implementation of predictive neural net
# Based on Frieder Stolzenburg talk at WSU
# Using complex numbers

n = 100;  # total matrix size
k = 3;    # in/out dimension

s = 2.1;
W_res = complex(normrnd(0,1/sqrt(s*n),n-k),
                normrnd(0,1/sqrt(s*n),n-k));
W_in = complex(normrnd(0,1/sqrt(s*n),[n-k,k]),
               normrnd(0,1/sqrt(s*n),[n-k,k]));
x_res = complex(normrnd(0,1/sqrt(s*n),[n-k,1]),
                normrnd(0,1/sqrt(s*n),[n-k,1]));

x_res0 = x_res;

xs = linspace(0,4*pi,4*n+1);

as = vertcat(arrayfun(@sin, xs),
             arrayfun(@(x) (cos(2*x+1)), xs),
             arrayfun(@(x) (3*sin(x+1) - cos(3*x + 2)), xs));

A = zeros(n);

for i = 1:n
 A(i,:) = vertcat(as(:,i), x_res);
 x_res = W_in * as(:,i) + W_res * x_res;
end

W_out = A\(transpose(as)(2:n+1,:));

M = vertcat(transpose(W_out), [W_in, W_res]);

# test
number_of_guesses = 0;
x_res = x_res0;
ys = zeros([k,n+number_of_guesses]);

y = vertcat(as(:,1), x_res);
for i = 1:n
  ys(:,i) = y(1:k);
  y = M * y;
end

# save this as new input
x_in = y;

# make some predictions
for i = n+1:n+number_of_guesses
  ys(:,i) = y(1:k);
  y = M * y;
end

# spectral radius of M. Should be approximately 1.
M_rad = max(abs(eig(M)));

# error
err = norm(as(:,1:n+number_of_guesses) - ys);

# Print results
err
M_rad

# Reduction of M
epsilon = 1e-5;   # threshold for dimension reduction

[V,D] = eig(M);
invV = inv(V);
U = invV * x_in;
A = V(1:k,:) .* transpose(U);

idx = find(prod(abs(A) < epsilon, 1));

dimensions = n - size(idx)(2)

invV(idx,:) = [];

x_hat = invV * x_in;

D_hat = diag(D);
D_hat(idx) = [];

w = V(1:k,:);
w(:,idx) = [];

w
D_hat
x_hat

# plot eigenvalues of M
#plot(eig(M),'o')

# plot(sort(abs(eig(M))) .^ 2)
