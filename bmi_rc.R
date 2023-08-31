## Qin Liu  et al A two-stage hierarchical regression model for meta-analysis of epidemiologic nonlinear dose-response data
## Table 6. Summary of published studies on the relation between BMI and renal cell cancer 
# read data from Nishimura 2007.xlsx
install.packages("readxl")

# Loading
library("readxl")

# xlsx files
bmi_rc<-read.delim("clipboard")

bmi_rc <- read_excel("Liu 2009 BMI & RC.xlsx", sheet=1)
bmi_rc

library("dosresmeta")

bmi_rc$n <- bmi_rc$cases + bmi_rc$control

## Linearity using random-effect

lin <- dosresmeta(formula = logrr ~ bmi, type = type, id = id,
	se = se, cases = cases, n = n, data = bmi_rc)
summary(lin)
predict(lin, exp = TRUE, delta = 1)

## Tabulate result
pred <- predict(lin, data.frame(bmi = seq(20, 35, 1)))
round(pred, 2)

## Linear trend centered at bmi = 19.3

#  Create a new dataset with variable bmi = (19, 19.1, 19.2,?€?, 34.9, 35). This is because bmi is centered at 19.3
newdata = data.frame(bmi = seq(19, 35, 0.1))

with(predict(lin, newdata, exp = TRUE, xref = min(bmi_rc$bmi)), {
	plot(newdata$bmi, pred, type = "l", log = "y", ylab = "Relative risk", las = 1,
		xlab = "Body Mass Index (BMI)", ylim = c(.6, 8) , bty = "l", yaxt = "n", xaxt = "n", lwd = 2)
	axis(2, at = c(1,2,4,8),las=2)
	axis(1, at = c(20,22,24,26,28,30,32,34))
	lines(newdata$bmi, ci.lb, lty = "dashed")
	lines(newdata$bmi, ci.ub, lty = "dashed")
})
rug(bmi_rc$bmi)
#  Add data points with their size inverse to their se
points(bmi_rc$bmi, bmi_rc$rr, cex = 0.5/bmi_rc$se)


## Non-linear model using random-effect
library("rms")

## define where the knots are
knots <- quantile(bmi_rc$bmi, c(0.10, 0.50, 0.90))

## fit restricted cubic spline model
spl <- dosresmeta(formula = logrr ~ rcs(bmi, knots), type = type, id = id, se = se, 
cases = cases, n = n, data = bmi_rc)
summary(spl)

waldtest(b = coef(spl), Sigma = vcov(spl), Terms = 2)

## Tabulate result
pred <- predict(spl, data.frame(bmi = seq(20, 35, 1)), xref = 20)
print(pred, digits = 2)

## nonlinear trend centered at bmi = 19.3
with(predict(spl, newdata, exp = TRUE, xref = min(bmi_rc$bmi)), {
	plot(newdata$bmi, pred, type = "l", log = "y", ylab = "Relative risk", las = 1,
		xlab = "Body Mass Index (BMI)", ylim = c(.6, 8) , bty = "l", yaxt = "n", xaxt = "n", lwd = 2)
	axis(2, at = c(1,2,4,8),las=2)
	axis(1, at = c(20,22,24,26,28,30,32,34))
	lines(newdata$bmi, ci.lb, lty = "dashed", lwd = 2)
	lines(newdata$bmi, ci.ub, lty = "dashed", lwd = 2)
	lines(newdata$bmi, predict(lin, newdata, exp = TRUE, xref = min(bmi_rc$bmi))[,"pred"], lty = "dashed")
})
rug(bmi_rc$bmi)
#  Add data points with their size inverse to their se
points(bmi_rc$bmi, bmi_rc$rr, cex = 0.5/bmi_rc$se)


## Linear trend centered at bmi = 22.5
with(predict(lin, newdata, exp = TRUE, xref = 22.5), {
	plot(newdata$bmi, pred, type = "l", log = "y", ylab = "Relative risk", las = 1,
		xlab = "Body Mass Index (BMI)", ylim = c(.6, 8) , bty = "l", yaxt = "n", xaxt = "n", lwd = 2)
	axis(2, at = c(1,2,4,8),las=2)
	axis(1, at = c(20,22,24,26,28,30,32,34))
	lines(newdata$bmi, ci.lb, lty = "dashed")
	lines(newdata$bmi, ci.ub, lty = "dashed")
})
rug(bmi_rc$bmi)
#  Add data points with their size inverse to their se
points(bmi_rc$bmi, bmi_rc$rr, cex = 0.5/bmi_rc$se)

