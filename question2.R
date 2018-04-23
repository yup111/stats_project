q2data=data.frame(sorption_rates=c(1.06,0.79,0.82,0.89,1.05,0.95, 0.65, 1.15, 1.12, 1.58, 1.12, 1.45, 0.91, 0.57, 0.83, 1.16, 0.43, 0.29, 0.43, 0.06, 0.51, 0.44, 0.10, 0.55, 0.53, 0.61, 0.34, 0.06, 0.09, 0.17, 0.17, 0.60),
                  solvents=factor(c(rep("A",9),rep("C",8),rep("E",15))))
model1 <- with(q2data,aov(sorption_rates~solvents))
summary(model1)
