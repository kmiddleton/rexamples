library(abd)

data(WalkingStickFemurs)
Error.fit <- aov(femur.length ~ 1 + Error(specimen), 
                 data = WalkingStickFemurs)
vc <- varcomps(Error.fit, n = 2)
vc
repeatability(vc)

library(rptR)
rpt(WalkingStickFemurs$femur.length, WalkingStickFemurs$specimen, 
    datatype = "Gaussian", method = "ANOVA")
# R  = 0.748

rpt(WalkingStickFemurs$femur.length, WalkingStickFemurs$specimen, 
    datatype = "Gaussian", method = "corr")
# R  = 0.754

rpt(WalkingStickFemurs$femur.length, WalkingStickFemurs$specimen, 
    datatype = "Gaussian", method = "REML")
# R  = 0.748

library(ICC)
ICCbare(specimen, femur.length, WalkingStickFemurs)
# [1] 0.7475028
