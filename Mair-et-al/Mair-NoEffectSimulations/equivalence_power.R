# Aim: calculate power for equivalence testing with border = +-0.1 for varying sample sizes
# Aim2: calculate power depending on variance ?

#Aim1:
# imagine we have a true effect of 0.09 = smaller than 10% colony loss OR 0.05 OR 0
# we want a power of 0.8 or 0.6
# how does sample size correlate with sample size?

## optimization task: low sample size and high variance = easy and less costly

out = data.frame(
  n = rep(3:200,2),
  power = c(rep(0.8,198), rep(0.6, 198)),
  var = rep(NA,396)
)

for(i in 1:396){
  n = out$n[i]
  power = out$power[i]
  out$var[i] = power.t.test(delta = 0.01, sd = NULL, sig.level = 0.05, n = n, power = power, alternative = "one.sided")$sd^2
}

plot(-out$n ~out$var, col = as.factor(out$power), type = "l")


##for 0.5 true effect


out2 = data.frame(
  n = rep(3:200,2),
  power = c(rep(0.8,198), rep(0.6, 198)),
  var = rep(NA,396)
)

for(i in 1:396){
  n = out2$n[i]
  power = out2$power[i]
  out2$var[i] = power.t.test(delta = 0.05, sd = NULL, sig.level = 0.05, n = n, power = power, alternative = "one.sided")$sd^2
}

plot(-out2$n ~out2$var, col = as.factor(out$power), type = "l")

## for true effect of zero:
out3 = data.frame(
  n = rep(3:200,2),
  power = c(rep(0.8,198), rep(0.6, 198)),
  var = rep(NA,396)
)

for(i in 1:396){
  n = out3$n[i]
  power = out3$power[i]
  out3$var[i] = power.t.test(delta = 0.1, sd = NULL, sig.level = 0.05, n = n, power = power, alternative = "one.sided")$sd^2
}

plot(-out3$n ~out3$var, col = as.factor(out$power), type = "l")

### 

outfull = data.frame(
  n = rep(3:100,10),
  power = runif(980, min = 0.1, max = 0.9),
  var = runif(980, min = 0, max = 1)
)
outfull$n = as.numeric(outfull$n)

n = 3:100
var = seq(0,0.1,0.001)
outfull = expand.grid(n, var)
outfull$power = rep(NA,nrow(outfull))
colnames(outfull) = c("n","variance","power")
str(outfull)


for(i in 1:nrow(outfull)){
  n = outfull$n[i]
  variance = outfull$variance[i]
  outfull$power[i] = power.t.test(delta = 0.05, sd = sqrt(outfull$variance[i]), sig.level = 0.05, 
                                  n = n, power = NULL, alternative = "one.sided")$power
}

# library(RColorBrewer)
# palette(colorRampPalette(c("red","yellow","green"))(1024))
# plot(outfull$n ~outfull$variance, col = as.factor(round(outfull$power,digits = 2)))
# plot(outfull$n ~outfull$variance, col = outfull$power)
p = ggplot(outfull, aes(x = variance, y = n, colour = power)) +
  geom_point(size = 0.7, shape = 15) 

p + scale_color_viridis_c(option = "turbo")
p+ scale_color_continuous(type = "viridis")



library(superheat)
superheat(outfull,
          scale = TRUE,
          left.label.text.size=3,
          bottom.label.text.size=3,
          bottom.label.size = .05,
          row.dendrogram = TRUE )

y# Aim 2:
# imagine we have a true effect of 0.09 = smaller than 10 %, OR 0.05
# imagine we have a sample size of 4
# how does power depend on variance

# Aim 3:
# imagine we have a sample size of 4
# and a variance of 0.2
# how does power change with true effect size?

