library(lattice)

data(Chem97, package = 'mlmRev')

histogram(~ gcsescore | factor(score), data = Chem97)

histogram(~ gcsescore, data = Chem97)

densityplot(~ gcsescore | factor(score), data = Chem97,
	plot.points = FALSE, ref = TRUE)

densityplot(~ gcsescore, groups = score, data = Chem97,
	plot.points = FALSE, ref = TRUE, auto.key = list(columns = 3))

densityplot(~ gcsescore, groups = score, data = Chem97,
	plot.points = FALSE, ref = TRUE, 
	auto.key = list(columns = 1, space = 'right'))

xyplot(gcsescore ~ score, data = Chem97)
