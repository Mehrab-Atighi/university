################ 1
# پارامترها
theta <- 0.3
mu <- 1
R <- 0.1

# تابع احتمال ورشکستگی به روش Cramér-Lundberg
psi_CL <- function(u) {
  C <- (theta * mu) / (R * mu - mu * (1 + theta))
  return(C * exp(-R * u))
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_CL(u)


################ 2
# پارامترها
theta <- 0.3
mu <- 1
mu2 <- 2 # مقدار فرضی برای مثال
mu3 <- 3 # مقدار فرضی برای مثال

# تابع احتمال ورشکستگی به روش De Vylder
psi_E <- function(u) {
  term1 <- 2 * mu * theta * u - mu2
  term2 <- sqrt((mu2)^2 + (4 / 3) * theta * mu * mu3)
  return(exp(-1 - term1 / term2))
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_E(u)





################ 3
# پارامترها
theta <- 0.3
mu <- 1
mu2 <- 2 # مقدار فرضی برای مثال
mu3 <- 3 # مقدار فرضی برای مثال

# تابع احتمال ورشکستگی به روش Lundberg
psi_L <- function(u) {
  term1 <- theta * u - mu2 / (2 * mu)
  term2 <- (4 * theta * mu^2 * mu3) / (3 * (mu2)^3)
  return((1 + term1 * term2) * exp(-2 * mu * theta * u / mu2))
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_L(u)


################ 4
# پارامترها
theta <- 0.3
mu <- 1
mu2 <- 2 # مقدار فرضی برای مثال
mu3 <- 3 # مقدار فرضی برای مثال

# محاسبه پارامترهای توزیع گاما
alpha <- (1 + ((4 * mu * mu3) / (3 * mu2^2) - 1) * theta) / (1 + theta)
beta <- (2 * mu * theta) / (mu2 + (4 * mu * mu3 / (3 * mu2) - mu2) * theta)

# تابع توزیع گاما
G <- function(u) {
  pgamma(u, shape = alpha, rate = beta)
}

# تابع تقریب احتمال ورشکستگی به روش Beekman-Bowers
psi_BB <- function(u) {
  1 / (1 + theta * (1 - G(u)))
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_BB(u)

################ 5
# پارامترها
theta <- 0.3
mu <- 1
mu2 <- 2 # مقدار فرضی برای مثال

# تابع تقریب احتمال ورشکستگی به روش Rénnyi
psi_R <- function(u) {
  exp_term <- exp(-2 * mu * theta * u / (mu2 * (1 + theta)))
  return(1 / (1 + theta) * exp_term)
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_R(u)

################ 6
# پارامترها
theta <- 0.3
mu <- 1
mu2 <- 2 # مقدار فرضی برای مثال
mu3 <- 3 # مقدار فرضی برای مثال

# محاسبه پارامترهای روش De Vylder
bar_beta <- 3 * mu2 / mu3
bar_lambda <- 9 * mu2^3 / (2 * mu3^2)
bar_theta <- 2 * mu * mu3 / (3 * mu2^2) * theta

# تابع تقریب احتمال ورشکستگی به روش De Vylder
psi_DV <- function(u) {
  exp_term <- exp(-bar_beta * mu * u / (1 + bar_theta))
  return(1 / (1 + bar_theta) * exp_term)
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_DV(u)

################ 7
# پارامترها
mu <- 1
mu2 <- 2 # دومین لحظه مرکزی
mu3 <- 3 # سومین لحظه مرکزی

# محاسبه پارامترهای روش De Vylder
bar_mu <- mu
bar_mu2 <- (mu * mu3 + mu2^2) / (2 * mu2)
bar_lambda <- 2 * mu2^2 / (mu * (mu3 + mu2^2))
bar_theta <- (mu * (mu3 + mu2)) / (2 * mu2^2)

# تابع توزیع گاما
G <- function(u) {
  pgamma(u, shape = bar_lambda, rate = bar_theta)
}

# تابع تقریب احتمال ورشکستگی به روش 3-لحظه گاما De Vylder
psi_3MGDV <- function(u) {
  1 / (1 + bar_theta * (1 - G(u)))
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_3MGDV(u)

################ 8
# پارامترها
theta <- 0.3
mu <- 1
mu2 <- 2 # دومین لحظه مرکزی

# تابع تقریب احتمال ورشکستگی به روش ترافیک سنگین
psi_HT <- function(u) {
  exp(-2 * theta * mu * u / mu2)
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_HT(u)

################ 9
# پارامترها
theta <- 0.3
mu <- 1

# تابع توزیع تجمعی F_X (فرضی)
F_X <- function(x) {
  1 - pexp(x, rate = 1 / mu) # توزیع نمایی به عنوان مثال
}

# تابع تابع تکمیلی F_X
F_X_bar <- function(x) {
  1 - F_X(x)
}

# تابع تقریب احتمال ورشکستگی به روش ترافیک سبک
psi_LT <- function(u) {
  integrate(F_X_bar, lower = u, upper = Inf)$value / ((1 + theta) * mu)
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_LT(u)

################ 10
# پارامترها
theta <- 0.3
mu <- 1
mu2 <- 2 # دومین لحظه مرکزی

# تابع تقریب احتمال ورشکستگی به روش ترافیک سبک
psi_LT <- function(u) {
  pexp(u, rate = 1 / mu)
}

# تابع تقریب احتمال ورشکستگی به روش ترافیک سنگین
psi_HT <- function(u) {
  exp(-2 * theta * mu * u / mu2)
}

# تابع تقریب احتمال ورشکستگی به روش ترافیک سنگین-سبک
psi_HLT <- function(u) {
  term_LT <- theta / (1 + theta) * psi_LT(theta * u / (1 + theta))
  term_HT <- 1 / (1 + theta)^2 * psi_HT(u)
  return(term_LT + term_HT)
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_HLT(u)

################ 11
# پارامترها
theta <- 0.3
mu <- 1

# تابع توزیع تجمعی مکمل (فرضی)
F_bar <- function(x) {
  1 - plnorm(x, meanlog = log(mu), sdlog = 1) # توزیع lognormal به عنوان مثال
}

# تابع تقریب احتمال ورشکستگی به روش ادعاهای دنباله سنگین
psi_HTC <- function(u) {
  integral_value <- integrate(F_bar, lower = 0, upper = u)$value
  return(1 / (theta * mu) * (mu - integral_value))
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_HTC(u)

################ 12
# کتابخانه‌ها
library(stats)

# پارامترها
theta <- 0.3
mu <- 1

# تابع توزیع تجمعی مکمل (فرضی)
F_bar <- function(x) {
  1 - plnorm(x, meanlog = log(mu), sdlog = 1) # توزیع lognormal به عنوان مثال
}

# چگالی b0
b0 <- function(x) {
  F_bar(x) / mu
}

# تابع توزیع تجمعی مکمل b0
B0_bar <- function(x) {
  integrate(b0, lower = x, upper = Inf)$value
}

# تابع تقریب احتمال ورشکستگی به روش Pollaczek-Khinchin
psi_PK <- function(u) {
  theta / (1 + theta) * sum(sapply(0:100, function(n) (1 / (1 + theta))^n * B0_bar(u)))
}

# محاسبه احتمال ورشکستگی برای مقدار u مشخص
u <- 10
psi_PK(u)


