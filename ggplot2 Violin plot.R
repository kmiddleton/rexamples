p <- ggplot(mtcars, aes(mpg))
p + geom_ribbon(aes(ymax = ..density.., ymin = - ..density..), stat = "density") + facet_grid(cyl ~ .)
