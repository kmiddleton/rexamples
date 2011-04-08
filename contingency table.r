#Contingency Table

color.blind <- matrix(c(442, 514, 38, 6), nrow=2, byrow=T)

color.blind

dimnames(color.blind) <- list(c("normal","c-b"),c("Male","Female"))

color.blind

chisq.test(color.blind, correct=F)
