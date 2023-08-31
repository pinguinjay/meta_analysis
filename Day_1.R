### Example 1: Fuccio 2009 AIM
# read data from Dataset0708.xlsx
#install.packages("readxl")

# Loading
library("readxl")

## You have to type the following command line
fuccio <- read.delim("clipboard")

## The rest can be copied and pasted
fuccio <- read_excel("Dataset0706.xlsx", sheet=1)
fuccio

#install.packages("meta")
library(meta)

fuccio.meta<-metabin(txcase,txtotal,cncase,cntotal,study,sm="OR",data=fuccio)
fuccio.meta

forest(fuccio.meta)

funnel(fuccio.meta)

metabias(fuccio.meta,method.bias="linreg",k.min=5,plotit=T)


#################################################################################################################
### Example 2: eGFR

## You have to type the following command line
gfr<-read.delim("clipboard")

## The rest can be copied and pasted
gfr <- read_excel("Dataset0706.xlsx", sheet=2)
gfr

library(meta)

gfr.meta <- metagen(md,se,study,data=gfr,sm="MD")

forest(gfr.meta)

funnel(gfr.meta)

metabias(gfr.meta,method.bias="linreg",k.min=5,plotit=T)

## create contour enhanced funnel plot (with funnel centered at 0)
#install.packages("metafor")
library(metafor)
gfr.res <- rma(md, vi=se^2, data=gfr)
gfr.res
funnel(gfr.res, level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), 
       refline=0, legend=TRUE)	

########Meta-regression###########################################
##Example 3: BCF efficacy & latitude

library(meta)
bcg <- read_excel("Dataset0706.xlsx", sheet="bcg")
bcg
bcg.meta<-metabin(cases1,tot1,cases0,tot0,trialnam,sm="OR",data=bcg)
bcg.meta

forest(bcg.meta)
funnel(bcg.meta)
library(metafor)
metareg(bcg.meta,~latitude)

