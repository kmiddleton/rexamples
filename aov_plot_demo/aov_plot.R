#load the ggplot2 library
library(ggplot2)

# The aov_plot() function below facilitates the generation of multiple plots of data from a factorial ANOVA.
# Usage:
# 	.data -> data frame, specifies where to find the data to be plotted. Example -> my_data
# 	.dv -> .() object, specifies the dependent variable. Example-> .(my_dv)
# 	.id ->  .() object, specifies the unit of observation. Example -> .(Ss)
# 	.within.variables -> .() object, specifies the within-Ss variables. Example -> .(condition,time)
# 	.between.variables -> .() object, specifies the between-Ss variables. Example -> .(gender,race)
# 	.x -> .() object, specifies the variable to be plotted on the x-axis. Example -> .(time)
# 	.lines -> logical, specifies whether to join points with lines.
# 	.bars -> logical, specifies whether to plot error bars (defaults to computing Fisher's Least Significant Difference bars)
# 	.bar.width -> numeric, specifies a non-default width of the error bars. Example -> .5
# 	.FLSD -> numeric, specifies a non-default size of the error bars. Example -> 10.5
# 	.split -> .() object, specifies a variable by which to split the data into different shapes/colors (and linetypes, if .line==TRUE). Example -> .(condition)
# 	.row -> .() object, specifies a variable to facet into rows. Example -> .(time)
# 	.col -> .() object, specifies a variable to facet into columns. Example -> .(gender)
# 	.to.numeric -> .() object, specifies variables that should be converted to numeric for plotting. Example -> .(age)
# 	.x.lab -> character string, specifies the x-axis label
# 	.y.lab -> character string, specifies the y-axis label
# 	.split.lab -> character string, specifies the plotted label of the .split variable
# Returns a named list containg:
#		$plot -> a ggplot object
#		$FLSD -> record of the size of the error bars used in the ggplot object
#		$aov -> the ANOVA pertinent to the plotted effect
aov_plot = function (
	.data
	, .dv
	, .id
	, .within.variables = NULL
	, .between.variables = NULL
	, .x
	, .lines = TRUE
	, .bars = TRUE
	, .bar.width = NULL
	, .FLSD = NULL
	, .split = NULL
	, .row = NULL
	, .col = NULL
	, .to.numeric = NULL
	, .x.lab = NULL
	, .y.lab = NULL
	, .split.lab = NULL
){
	if(is.null(.within.variables) & is.null(.between.variables)){
		stop('is.null(.within.variables) & is.null(.between.variables)\nYou must specify at least one variable.')
	}
	.variables = structure(as.list(c(.(dummy),.between.variables)),class = 'quoted')
	N = ddply(
		cbind(.data,dummy = rep(1,length(.data[,1])))
		,.variables
		,function(x){
			to.return = length(unique(x$id))
			names(to.return) = 'N'
			return(to.return)
		}
	)
	N = mean(N[,length(N)])
	.variables = structure(as.list(c(.id,.between.variables,.within.variables)),class = 'quoted')
	.data <- ddply(
		.data
		,.variables
		,function(x){
			mean(x[,names(x) == as.character(.dv)])
		}
	)
	aov.formula = paste(
		'V1~'
		,paste(as.character(.between.variables),collapse = '*')
		,ifelse(is.null(.between.variables),'',ifelse(is.null(.within.variables),'','*'))
		,paste(as.character(.within.variables),collapse = '*')
		,ifelse(
			is.null(.within.variables)
			,''
			,paste(
				'+Error('
				,as.character(.id)
				,'/('
				,paste(as.character(.within.variables),collapse = '*')
				,'))'
				,sep = ''
			)
		)
		,sep = ''
	)
	.aov = summary(aov(
		formula(aov.formula)
		,data = .data
	))
	if(is.null(.FLSD)){
		if(is.null(.within.variables)){
			DFE = .aov[[1]]$D[1+length(.between.variables)]
			MSE = .aov[[1]]$M[1+length(.between.variables)]
		}else{
			DFE = .aov[[length(.aov)]][[1]]$D[2+length(.between.variables)]
			MSE = .aov[[length(.aov)]][[1]]$M[2+length(.between.variables)]
		}
		Tcrit = qt(0.975,DFE)
		CI = Tcrit * sqrt(MSE/N)
		.FLSD = sqrt(2) * CI
	}	
	.variables = structure(as.list(c(.between.variables,.within.variables)),class = 'quoted')
	.data <- ddply(
		.data
		,.variables
		,function(x){
			mean(x$V1)
		}
	)
	.data$.ymin = .data$V1-.FLSD/2
	.data$.ymax = .data$V1+.FLSD/2
	for(i in .to.numeric){
		.data[,names(.data) == i] = as.numeric(as.character(.data[,names(.data) == i]))
	}
	names(.data)[names(.data) == .x] = '.x'
	if(!is.null(.split)){
		names(.data)[names(.data) == .split] = '.split'
	}
	if(!is.null(.row)){
		names(.data)[names(.data) == .row] = '.row'
	}
	if(!is.null(.col)){
		names(.data)[names(.data) == .col] = '.col'
	}
	p = ggplot(
		data = .data
		,aes(
			y = V1
			,x = .x
		)
	)
	if(!is.null(.split)){
		p = p+geom_point(
			aes(
				colour = .split
				,shape = .split
			)
		)
		if(!is.null(.split.lab)){
			p = p+labs(colour = .split.lab,shape = .split.lab)
		}
		if(.lines){
			p = p+geom_line(
				aes(
					colour = .split
					,linetype = .split
					,x = as.numeric(.x)
				)
			)
			if(!is.null(.split.lab)){
				p = p+labs(linetype = .split.lab)
			}
		}
		if(.bars){
			p = p+geom_errorbar(
				aes(
					colour = .split
					,ymin = .ymin
					,ymax = .ymax
				)
				,linetype = 1
				,legend = FALSE
				,width = .bar.width
			)
		}
	}else{
		p = p+geom_point()
		if(.lines){
			p = p+geom_line(aes(x = as.numeric(.x)))
		}
		if(.bars){
			p = p+geom_errorbar(
				aes(
					ymin = .ymin
					,ymax = .ymax
				)
				,linetype = 1
				,legend = FALSE
				,width = .bar.width
			)
		}
	}
	if(!is.null(.row)){
		if(!is.null(.col)){
			p = p+facet_grid(.row~.col)
		}else{
			p = p+facet_grid(.row~.)
		}
	}else{
		if(!is.null(.col)){
			p = p+facet_grid(.~.col)
		}
	}
	if(!is.null(.x.lab)){
		p = p+labs(x = paste('\n',.x.lab,sep = ''))
	}
	if(!is.null(.y.lab)){
		p = p+labs(y = paste(.y.lab,'\n',sep = ''))
	}
	return(list(plot=p,FLSD=.FLSD,aov=.aov))
}