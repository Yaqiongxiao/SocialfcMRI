# function to plot interaction scatter plots

FC_scatterplot <- function(dat, x,y, xlab, term) { 
	
	# DESCRIPTION
	# This function plots scatter plots
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# x = data to plot in the x-axis, e.g., Age, RT
	# y = data to plot in the y-axis, i.e., network connectivity 
	# (within-mentalizing, reward, or between-network)
	# xlab = title of x-axis
	# term = interation term, e.g., RT, Age
	# OUTPUT
	# scatter plots showing interaction effects
		
	p <- ggplot(dat,aes_string(x =x, y = y, col = "social", fill = "social")) + 
		geom_point(shape = 19, alpha = .7) + 
		geom_smooth(method = "lm", 
			    alpha = .3, fullrange = T) +
		labs(x= xlab, y="Mean connectivity") + 
		guides(fill = "none") + 
		scale_color_manual(values = c("#e66101", "#5e3c99"),
				   name="Social\nInteraction",
				   breaks=c("P", "C"),
				   labels=c("Peer", "Character")) +
		scale_fill_manual(values = c("#e66101", "#5e3c99")) +
		theme(axis.text = element_text(size=18,face="bold"),
		      axis.title = element_text(size=20,face="bold")) + 
		theme(panel.background = element_blank(),
		      axis.line = element_line(colour = "black")) + 
		theme(legend.title=element_blank())
	
	if (term == "Age") {
		p <- p+coord_cartesian(ylim=c(0,1.6)) +
			scale_y_continuous(breaks = seq(0,1.6,0.4))	
	} else if (term == "RT") {
		p <- p + coord_cartesian(xlim = c(1.4,3), ylim=c(0,1.8)) +
			scale_x_continuous(breaks = seq(1.4,3,0.4)) +
			scale_y_continuous(breaks = seq(0,1.8,0.6))
	}

	return(p)
} 
