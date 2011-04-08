data(dietox) 
dietox$Cu <- as.factor(dietox$Cu) 
mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3))) 
gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1") 
gee1 
summary(gee1) 
mf2 <- formula(Weight~Cu*Time+I(Time^2)+I(Time^3)) 
gee2 <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1") 
anova(gee2) 