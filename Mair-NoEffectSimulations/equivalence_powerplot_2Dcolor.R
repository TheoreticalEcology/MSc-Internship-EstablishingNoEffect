

n = 3:100
var = seq(0,0.1,0.001)
outfull = expand.grid(n, var)
outfull$power = rep(NA,nrow(outfull))
colnames(outfull) = c("n","variance","power")
str(outfull)


for(i in 1:nrow(outfull)){
  n = outfull$n[i]
  variance = outfull$variance[i]
  outfull$power[i] = power.t.test(delta = 0.1, sd = sqrt(outfull$variance[i]), sig.level = 0.05, 
                                  n = n, power = NULL, alternative = "one.sided")$power
}

# library(RColorBrewer)
# palette(colorRampPalette(c("red","yellow","green"))(1024))
# plot(outfull$n ~outfull$variance, col = as.factor(round(outfull$power,digits = 2)))
# plot(outfull$n ~outfull$variance, col = outfull$power)
p = ggplot(outfull, aes(x = variance, y = n, colour = power)) +
  geom_point(size = 0.7, shape = 15) 

p + scale_color_viridis_c(option = "turbo")



####### power by sample size

out = data.frame(
  n = seq(0,100,1),
  power = rep(NA, 101)
)

for(i in 1:101){
  out$power[i] = power.t.test(delta = 0.05, sd = 0.1, sig.level = 0.05, 
                                  n = out$n[i], power = NULL, alternative = "one.sided")$power
}

plot(out$power ~ out$n, type = "l", bty = "l",
     xlab = "Sample size", ylab = "Power")



####### power by variance

out = data.frame(
  sd = seq(0,0.2,0.01),
  power = rep(NA, length(seq(0,0.2,0.01)))
)

for(i in 1:length(seq(0,0.2,0.01))){
  out$power[i] = power.t.test(delta = 0.05, sd = out$sd[i], sig.level = 0.05, 
                              n = 20, power = NULL, alternative = "one.sided")$power
}

out$var = out$sd^2

plot(out$power ~ out$var, type = "l", bty = "l",
     xlab = "Variance", ylab = "Power")


####### power by true effect

out = data.frame(
  effect = seq(0,0.2,0.01),
  power = rep(NA, length(seq(0,0.2,0.01)))
)

for(i in 1:length(seq(0,0.2,0.01))){
  out$power[i] = power.t.test(delta = out$effect[i], sd = 0.1, sig.level = 0.05, 
                              n = 20, power = NULL, alternative = "one.sided")$power
}

plot(out$power ~ out$effect, type = "l", bty = "l",
     xlab = "True effect size", ylab = "Power")


