#Is it possible to change the legend title in ggplot that is generated
#by a colour (other than by changing the name of the variable)?  For
#example, this produces a legend title, "tip/total_bill".  How can that
#title be changed?

ggpoint(ggplot(tips,aesthetics=list(y=tip,x=total_bill,colour=tip/total_bill)) )

#You need to add a colour scale, and specify the name to appear on the
#legend (by default it just uses the name of the variable):

p <- ggpoint(ggplot(tips,aesthetics=list(y=tip,x=total_bill,colour=tip/total_bill)))
sccolour(p, "Tip rate")
sccolour(p, "This is a very long label\nsplit over two lines")

#you can also use expressions to create mathematical annotations:

sccolour(p, expression(beta * x^2))
