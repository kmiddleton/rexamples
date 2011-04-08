def.contr <- options("contrasts")

# Need to change contrasts to contr.helmert to make the output match SPSS
options(contrasts = c("contr.helmert", "contr.poly"))

Anova(XXXXXX, type = "III")
