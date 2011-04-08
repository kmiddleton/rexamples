data(ToothGrowth)

# One way design
bargraph.CI(x.factor = dose, response = len, data = ToothGrowth) 

# Two-way design with options
bargraph.CI(dose, len, group = supp, data = ToothGrowth,
            xlab = "Dose", ylab = "Growth", cex.lab = 1.5, x.leg = 1,
            col = "black", angle = 45, cex.names = 1.25,
            density = c(0,20), legend = TRUE)