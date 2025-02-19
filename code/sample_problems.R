# goal is to solve max x_1:n min a_1:n {min f_0 \sum_{i=1}^n l (f_0(x_i), a_i)) + min f_b \sum_{i=1}^n l(f_b(x_i), a_i)}

# action space discrete, consider only binary choice problems. So, a_i \in {0, 1}. each x_i describes a left, right option.

# choose an initialization x_1:n^0; maximum number of iterations S; and chosen step size sequence {\eta_s} > 0

# define the following 3 objects
# 1. \hat{\epislon}_0(x_1:n, a_1:n, \theta_0) = n^{-1} \sum_{i=1}^n l(f_{\theta_0}(x_i), a_i)
# 2. \hat{\epislon}_b(x_1:n, a_1:n, \theta_b) = n^{-1} \sum_{i=1}^n l(f_{\theta_b}(x_i), a_i)
# 3. \hat{\epsilon}_m(x_1:n, a_1:n, \theta_0, \theta_b) = \hat{\epislon}_0(x_1:n, a_1:n, \theta_0) + \hat{\epislon}_b(x_1:n, a_1:n, \theta_b)

# for s = 1 to S do

# a^{s+1}_{1:n} = \argmin_{a_1:n} min_\theta_0 \hat{\epislon}_0(x_1:n^s, a_1:n, \theta_0) + \hat{\epislon}_b(x_1:n^s, a_1:n, \theta_b)
# \theta_0^{s+1} = \argmin_{\theta_0} \hat{\epislon}_0(x_1:n^s, a_1:n^{s+1}, \theta_0)
# \theta_b^{s+1} = \argmin_{\theta_b} \hat{\epislon}_b(x_1:n^s, a_1:n^{s+1}, \theta_b)
# x_1:n^{s+1} = x_1:n^s + \eta_s

# problem: the quantity \min_{a_{1:n}} {\min_{\theta_0} \hat{\epislon}_0(x_1:n, a_1:n, \theta_0) + \min_{\theta_b} \hat{\epislon}_b(x_1:n, a_1:n, \theta_b)} is not differentiable everywhere.
# specifically it won't be differentiable at the points in $\theta$ where the minimizing action switches.


