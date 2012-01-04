library(proto)
library(grid)
library(devtools)
dev_mode()
library(ggplot2)

geom_custom <- function (mapping = NULL, 
                         data = NULL, 
                         stat = "identity",
                         position = "identity", 
                         show_guide = FALSE, ...) { 
  GeomCustom$new(mapping = mapping, 
                 data = data, 
                 stat = stat,
                 position = position, 
                 show_guide = show_guide, ...)
}

GeomCustom <- proto(ggplot2:::Geom, {
  
  objname <- "custom"
  
  new <- function(., data = NULL, mapping = NULL, grob =NULL, ...) {
    .super$new(., data = data, mapping = mapping, grob=grob, inherit.aes = FALSE, ...)
  }
  
  draw <- function(., data, scales, coordinates, grob, just=c("centre", "centre"), ...) {
    with(coord_transform(coordinates, data, scales), {
      width <- grobWidth(grob)
      height <- grobHeight(grob)
      grob <- editGrob(grob, vp=viewport(x = x, y = y,
                                         width=width, height=height, just = just), ...)
      ggname(.$my_name(), grob)
    })
  }
  
  default_aes <- function(.) 
    aes(x=0.5, y=0.5)
  default_stat <- function(.) StatIdentity
  
})


library(gridExtra)
g <- tableGrob(head(iris[1:3,]))

d <- qplot(1,1, geom="blank")
d + annotate("custom", x=1, y=1, grob=g)
