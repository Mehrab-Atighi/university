alpha <- c(0.5,5, 50)
theta <- c(100, 10 , 1)

plot(0, 0, xlim = c(0, 100), ylim = c(0, 0.06), type = "n" , xlab = "x" , ylab = "Density")
cl = 1
for(i in seq_along(alpha)){
  curve(dgamma(x, shape = alpha[i], scale = theta[i]), from = 0, to = 100, col = cl, add = TRUE , type = "l")
  legend(80, 0.06, legend=c("Line 1", "Line 2" , "Line3"),
         col= 1:3, lty=1, cex=0.8,
         box.lty=2, box.lwd=2, box.col="black" , text.font=4, bg='lightblue')
  cl = cl+1
 
}

