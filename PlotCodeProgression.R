#Stacked bar plots for code progression analyses
bar_colors = c('white', 'grey', 'darkslategray3', 'grey18')

#1. Using all UMLS codes
codes_all = cbind(c(0.647,0.352,0,0),
                  c(0.434,0.336,0.228,0),
                  c(0.110,0.368,0.397,0.122))

codes_31 = cbind(c(0.751,0.248,0,0),
                c(0.596,0.259,0.144,0),
                c(0.202,0.381,0.346,0.070))
#graph parameters
#par(mfrow=c(1,2), mar=c(4,4,2,1), oma=c(0,0,3,1))
par(mar=c(4,4,1,1),oma=c(0,0,3,1))
layout(rbind(c(1,1,2,2), c(3,3,3,3)), widths=c(1,1,1,1), heights=c(2.25,0.5))
#bar params
bar_densities = c(0, 5, 5, 10)
bar_angles = c(0,45,-45,90)
#horizontal labels for bar
x_label = c("2", "3","4")
legend_text = c("1st report", "2nd report", "3rd report", "4th report")
#1st plot, all UMLS codes
chart = barplot(codes_all*100,xpd=T,names.arg=x_label,
                #density = bar_densities,
                col=bar_colors,
                angle = bar_angles,
                ylim=c(0,109),
                xlab="Reports per case", 
                ylab="Unique findings (%)", 
                main='(a) All possible findings')

first_per = c()
for (value in codes_all[1,]){
  new_value = paste(as.character(round(100*value)),'%',sep='')
  first_per = c(first_per,c(new_value))
}
text(chart, 100*codes_all[1,]/2, labels=first_per)
#2nd plot, influenza-specific UMLS codes
chart2 = barplot(codes_31*100,xpd=T,
                names.arg=x_label,
                #density = bar_densities,
                col=bar_colors,
                angle = bar_angles,
                ylim=c(0,109),xlab="Reports per case", ylab=NULL, main='(b) 31 expert-defined findings')

first_per = c()
for (value in codes_31[1,]){
  new_value = paste(as.character(round(100*value)),'%',sep='')
  first_per = c(first_per,c(new_value))
}
text(chart2, 100*codes_31[1,]/2, labels=first_per)
mtext("Unique findings per ED report", side=3, line=1, outer=TRUE, cex=1, font=2)

#ghost plot: only for legend
barplot(0,0,axes=FALSE)
#legend('center', legend=legend_text, density=c(0,10,10,15), angle=bar_angles, 
#       cex=1.5, xjust=1, yjust=0.5, bty="n", horiz=TRUE, fill=T)
legend('center', legend=legend_text, col=bar_colors, 
       cex=1.5, xjust=1, yjust=0.5, bty="n", horiz=TRUE, fill=bar_colors)
