#18th march 
x<-scan("clipboard")
hist(x,freq =F,xlab = "Inter-Arrival Time", ylab = "Density",main = "Inter-Arrival Follows Exponential")
y = seq(0,0.7,0.01)
lines(y,dexp(y,8.57),col = "red")

obs = table(cut(x,breaks = c(0,0.1,0.2,0.4,0.7)))
exp1 = pexp(0.01,8.57)*466
exp2 = (pexp(0.2,8.57)-pexp(0,8.57))*466
exp3 = (pexp(0.3,8.57)-pexp(0.2,8.57))*466
exp4 = (pexp(0.4,8.57)-pexp(0.3,8.57))*466
exp5 = (pexp(0.5,8.57)-pexp(0.4,8.57))*466
exp6 = (pexp(0.6,8.57)-pexp(0.5,8.57))*466
exp7 = (pexp(0.7,8.57)-pexp(0.6,8.57))*466
exp = rbind(exp1,exp2,exp3+exp4,exp5+exp6+exp7)
a = data.frame(obs,exp)
chi_cal = sum((a$Freq-a$exp)/sqrt(a$exp))
pchisq(chi_cal,4)
a
chi_cal

