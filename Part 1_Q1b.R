# Calculating the Convergence statistic (R_hat)
# Generating the chained sequences, J=4
  
set.seed(12)
numbers_0 <- rnorm(n = 2000, sd = 0.001)

set.seed(13)
numbers_1 <- rnorm(n = 2000, sd = 0.001)

set.seed(14)
numbers_2 <- rnorm(n = 2000, sd = 0.001)

set.seed(15)
numbers_3 <- rnorm(n = 2000, sd = 0.001)

# sample mean , (M_j)
M_j <- (sum(numbers_0)/2000) + (sum(numbers_1)/2000) + (sum(numbers_2)/2000) +(sum(numbers_3)/2000)

# within sample variance , (V_j)
M_0 <- mean(numbers_0)
M_1 <- mean(numbers_1)
M_2 <- mean(numbers_2)
M_3 <- mean(numbers_3)

V_j <- (sum((numbers_0 - M_0)^2)/2000) + (sum((numbers_1 - M_1)^2)/2000) + (sum((numbers_2 - M_2)^2)/2000) + (sum((numbers_3 - M_3)^2)/2000)

# overall within sample variance (W)
W <- (V_j)/4

# overall sample mean (M)
M <- (M_j)/4

# between sample variance (B)
B <- ((M_0 - M)^2 + (M_1 - M)^2 + (M_2 - M)^2 + (M_3 - M)^2)/4

# convergence statistic, (r_hat)
r_hat <- sqrt( (B + W)/W)

print(M_j)
print(V_j)
print(W)
print(M)
print(B)
print(r_hat)