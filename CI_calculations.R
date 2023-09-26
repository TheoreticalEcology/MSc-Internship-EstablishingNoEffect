## CI calculations... 
# d = data.frame(res = c(dat$treatment, dat$control), x = rep(c("T", "C"), each = N))

#fit <- lm(res~x, data = d)
# confint(fit)
#confint(fit, "wt")

#xMean =  mean(dat$treatment)-mean(dat$control)

#xSDvarUneq = sqrt(((N-1)*var(dat$treatment) + (N-1) * var(dat$control))/ (2 * N - 2))


#xSDvarEqual = sd(c(dat$control-mean(dat$control),dat$treatment-mean(dat$treatment)))
#xSDLena = sqrt(var(c(dat$control-mean(dat$control),dat$treatment-mean(dat$treatment)))) 

# Normal approximation CI <- xMean + 1.96 * xSDvarUneq *  sqrt(2/N) 

#CI <- xMean + qt(0.05, df = 2*N-2, lower.tail = FALSE) * xSDvarUneq * sqrt(2/N) 



# how t.test()     
#mean_ttest <- (1/2*N)*(sum(dat$treatment) + sum(dat$control))
#mean_ttest <- mean(dat$treatment)-mean(dat$control) 
#lower_quantile <- qt(1-(0.05/2), df = 2*N-2, lower.tail = FALSE)
#sd <- sd(c(dat$treatment,dat$control))
#CI_ttest <- mean_ttest + lower_quantile * sd/sqrt(2*N)


#upperCI <- mean(dat$treatment)-mean(dat$control) + 
#qt(0.05/2, df = 2*N-2, lower.tail = FALSE) * sqrt(var(dat$control-mean(dat$control)))/sqrt(N) + sqrt(var(dat$treatment-mean(dat$treatment)))/sqrt(N)

