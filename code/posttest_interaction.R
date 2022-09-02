# function to plot interaction scatter plots

posttest_interaction <- function(dat, posttest,ytitle,filename) {
	# DESCRIPTION
	# This function plots scatter plots for posttest data and age
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# posttest = posttest item, e.g., attention, like, wantsee
	# ytitle = title of y-axis, e.g., Paid Attention, Want to See 
	# filename = the name of plot
	# OUTPUT
	# scatter plots showing interaction effects of posttest item and age
	
	p <- ggplot(dat, aes_string(x= "Age",y= posttest, col="social")) +
		geom_point(shape = 19, alpha = .9,
			   position = position_jitter(width = 0.1,height = .1)) +
		geom_smooth(aes(fill = social),method = "lm", 
			    alpha = 0.3, fullrange = T) + 
		labs(y = "Rating",x = "Age (years)") + 
		guides(fill = "none", color = "none") + 
		scale_color_manual(values = c("#e66101", "#5e3c99") ) +  # c("#D012E7","#7ec738")
		scale_fill_manual(values = c("#e66101", "#5e3c99")) +
		#  scale_color_manual(values = c(rgb(204/255,0,204/255),rgb(0,0,204/255))) + 
		theme(axis.text = element_text(size=18,face="bold"),
		      axis.title = element_text(size=20,face="bold")) + 
		theme(panel.background = element_blank(),
		      axis.line = element_line(colour = "black")) 
	#ggsave(paste0("results/",filename,".tiff"),width = 7, height = 5,
	  #     units = c("in"), dpi = 200)
	return(p)
}
