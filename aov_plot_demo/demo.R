#load the ggplot2 plotting library
library(ggplot2)
source('/Users/kmm/Desktop/aov_plot_demo/aov_plot.R')
source('/Users/kmm/Desktop/aov_plot_demo/generate_demo_data.R')

#generate data
a = generate_demo_data()

#look at the head of the data
head(a)

#look at a summary of the data
summary(a)


########
# RT Analysis
########

#compute the mean per cue*flanker*group*id
rt = ddply(
	.data = a
	, .variables = .(cue,flanker,group,id)
	, .fun <- function(x){
		each(mean)(x$rt)
	}
)
head(rt)

#run the anova. Group is a between-Ss variable, cue & flanker are within-Ss variables
rt_aov = aov(
	mean~group*cue*flanker+Error(id/(cue*flanker))
	,data=rt
)

#print a summary of the anova
summary(rt_aov)

rt.group = aov_plot(
	.data = rt
	, .dv = .(mean)
	, .id = .(id)
	, .between.variables = .(group)
	, .x = .(group)
	, .lines = FALSE
	, .x.lab = 'Group'
	, .y.lab = 'RT (ms)'
)	
print(rt.group$plot)

rt.group.by.cue = aov_plot(
	.data = rt
	, .dv = .(mean)
	, .id = .(id)
	, .within.variables = .(cue)
	, .between.variables = .(group)
	, .x = .(cue)
	, .lines = FALSE
	, .split = .(group)
	, .x.lab = 'Cue'
	, .y.lab = 'RT (ms)'
	, .split.lab = 'Group'
)	
print(rt.group.by.cue$plot)

rt.group.by.cue.by.flanker = aov_plot(
	.data = rt
	, .dv = .(mean)
	, .id = .(id)
	, .within.variables = .(cue,flanker)
	, .between.variables = .(group)
	, .x = .(flanker)
	, .bar.width = .5
	, .split = .(cue)
	, .col = .(group)
	, .x.lab = 'Flanker'
	, .y.lab = 'RT (ms)'
	, .split.lab = 'Cue'
)	
print(rt.group.by.cue.by.flanker$plot)
