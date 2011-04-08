#http://www.psych.ualberta.ca/~phurd/cruft/
source('/Users/kmm/Dropbox/R/rexamples/g.test.R')

nasal <- matrix(c(47, 41, 0, 0), nrow = 2)
row.names(nasal) <- c('Tri', 'Cent')
colnames(nasal) <- c('Normal', 'Patho')
nasal
g.test(nasal)
g.test(nasal)$expected

jugal <- matrix(c(32, 51, 7, 5), nrow = 2)
row.names(jugal) <- c('Tri', 'Cent')
colnames(jugal) <- c('Normal', 'Patho')
jugal
g.test(jugal)
g.test(jugal)$expected

parietal <- matrix(c(44, 60, 1, 2), nrow = 2)
row.names(parietal) <- c('Tri', 'Cent')
colnames(parietal) <- c('Normal', 'Patho')
parietal
g.test(parietal)
g.test(parietal)$expected

parietal <- matrix(c(44, 60, 1, 2), nrow = 2)
row.names(parietal) <- c('Tri', 'Cent')
colnames(parietal) <- c('Normal', 'Patho')
parietal
g.test(parietal)
g.test(parietal)$expected
