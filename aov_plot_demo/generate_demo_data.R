generate_demo_data = function(){
	set.seed(1)
	a=expand.grid(
		cue = factor(1:4,labels=c('No Cue','Center Cue','Double Cue','Spatial Cue'))
		,flanker = factor(1:3,labels=c('Neutral Flanker','Congruent Flanker','Incongruent Flanker'))
		,location = factor(c('up','down'))
		,direction = factor(c('left','right'))
		,block = c(1:6)
		,id = factor(c(1:20))
	)
	x = length(a[,1])
	a$trial = rep(c(1:(x/(6*20))),6*20)
	a$group = factor(rep(c('Treatment','Control'),each = x/2))
	a$rt = rnorm(x,500,50)
	a=data.frame(id=a$id,group=a$group,block=a$block,trial=a$trial,cue=a$cue,flanker=a$flanker,location=a$location,direction=a$direction,rt=a$rt)

	subset = a$cue=='Center Cue' & a$flanker=='Incongruent Flanker' & a$group=='Control'
	a$rt[subset] = a$rt[subset] - 20
	subset = a$cue=='Center Cue' & a$flanker!='Incongruent Flanker' & a$group=='Control'
	a$rt[subset] = a$rt[subset] - 50
	subset = a$cue=='Center Cue' & a$group=='Treatment'
	a$rt[subset] = a$rt[subset] - 50


	subset = a$cue=='Double Cue' & a$flanker=='Incongruent Flanker' & a$group=='Control'
	a$rt[subset] = a$rt[subset] - 25
	subset = a$cue=='Double Cue' & a$flanker!='Incongruent Flanker' & a$group=='Control'
	a$rt[subset] = a$rt[subset] - 55
	subset = a$cue=='Double Cue' & a$group=='Treatment'
	a$rt[subset] = a$rt[subset] - 55

	subset = a$cue=='Spatial Cue'
	a$rt[subset] = a$rt[subset] - 90
	
	subset = a$flanker %in% c('Congruent Flanker','Neutral Flanker')
	a$rt[subset] = a$rt[subset] - 70
	
	return(a)
}