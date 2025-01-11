library(VGAM)
normal_numbers = rnorm(10^7 , mean = 100 , sd = 223.607)
c(Mean = mean(normal_numbers) , Sd = sd(normal_numbers))
weibull_numbers = rweibull(10^7 , 0.5 , 50)
c(Mean = mean(weibull_numbers) , Sd = sd(weibull_numbers))
pareto_numbers = rlomax(10^7 , 150 , 2.5)
c(Mean = mean(pareto_numbers) , Sd = sd(pareto_numbers))


par(mfrow = c(1 , 3))
plot(0, 0, xlim = c(-1100, 1000), ylim = c(0, 0.002), type = "n" , xlab = "Loss" , ylab = "Density" , main = "Normal(100 , 223.607)")
curve(dnorm(x, mean = 100, sd = 223.607), from = -1100, to = 1000, col = 1, add = TRUE , type = "l" )
plot(0, 0, xlim = c(0, 1000), ylim = c(0, 0.015), type = "n" , xlab = "Loss" , ylab = "Density" , main = "Weibull(0.5 , 50)")
curve(dweibull(x, 0.5, 50), from = 0, to = 1000, col = 2, add = TRUE , type = "l")
plot(0, 0, xlim = c(0, 1000), ylim = c(0, 0.02), type = "n" , xlab = "Loss" , ylab = "Density" , main = "Pareto(150 , 2.5)")
curve(dlomax(x, 150, 2.5), from = 0, to = 1000, col = 3, add = TRUE , type = "l")

par(mfrow = c(1 , 1))
plot(0, 0, xlim = c(-1100, 1000), ylim = c(0, 0.015), type = "n" , xlab = "Loss" , ylab = "Density")
curve(dnorm(x, mean = 100, sd = 223.607), from = -1100, to = 1000, col = 1, add = TRUE , type = "l")
curve(dweibull(x, 0.5, 50), from = 0, to = 1000, col = 2, add = TRUE , type = "l")
curve(dlomax(x, 150, 2.5), from = 0, to = 1000, col = 3, add = TRUE , type = "l")
legend(750, 0.015, legend=c("Normal", "Weibull" , "Pareto"),
       col= 1:3, lty=1, cex=0.8,
       box.lty=2, box.lwd=2, box.col="black" , text.font=4, bg='lightblue')

n = c();p = c();w = c()
n[1] = qnorm(0.9  , 100 , 223.607)
n[2] = qnorm(0.99 , 100 , 223.607)
n[3] = qnorm(0.999 , 100 , 223.607)
w[1] = qweibull(0.9   , 0.5, 50)
w[2] = qweibull(0.99  , 0.5, 50)
w[3] = qweibull(0.999 , 0.5, 50)
p[1] = qlomax(0.9  , 150 , 2.5)
p[2] = qlomax(0.99 , 150 , 2.5)
p[3] = qlomax(0.999 , 150 , 2.5)

output = data.frame(n , p , w , row.names = row_name)
row_name = c("0.9" , "0.99" , "0.999")
col_name = c("Normal(100 , 223.607)"  , "Pareto(150 , 2.5)" , "Weibull(0.5 , 50)")
colnames(output) = col_name
output

