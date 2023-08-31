# read data from Nishimura 2007.xlsx
install.packages("readxl")

# Loading
library("readxl")
library(mada)

# xlsx files
tseng <- read_excel("Tseng.xlsx", sheet=1)
tseng

madad.tseng<-madad(tseng)
print(madad.tseng)

pdf(file="biforestplot.pdf",width=16,height=6,paper='special') 
split.screen(c(1,2))
screen(1)
forest(madad.tseng, type = "sens", xlab = "Sensitivity", cex = 1.2, snames = tseng$id)

screen(2)
forest(madad.tseng, type = "spec", xlab = "Specificity", cex = 1.2, snames = tseng$id)

dev.off()

bi.tseng <- reitsma(tseng)
summary(bi.tseng)

pdf(file="HSROC.pdf",width=6,height=6,paper='special') 
plot(bi.tseng, sroclwd = 2, main = "SROC curve (bivariate model) for tseng data", 
predict = TRUE, predlty = 3, predlwd = 2, pch = 16)

points(fpr(tseng), sens(tseng), pch = 1, col = "red")

legend("bottomright", c("data", "summary estimate"), pch = c(1,16), 
       col = c("red","black"))

legend("bottomleft", c("SROC", "conf. region", "pred region"), 
       lwd = c(2,1,1), lty = c(1,1,3))
	   
crosshair(bi.tseng, level = 0.95, length = 0.1, pch = 1)

dev.off()