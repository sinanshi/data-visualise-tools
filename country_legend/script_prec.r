library("lpjutil")
library("ggplot2")
library(rgdal)

setwd("/home/mfader/_AndereProjekte/USA/plotsSinan/country_legend/country_legend/")

#load("norm_kcal_cap_nec_sus.RData")

#source("/home/mfader/_AndereProjekte/USA/plotsSinan/country_legend/country_legend/script_prec.r")

#------------------------------------
# classify a vector into its pre-set 
# breaks. 
#------------------------------------
classify.map<-function(map, bracket){
    map_new<-array(NA, length(map))
    for(i in 1:length(bracket)-1){
        this_class<- map <= bracket[i+1]
        map_new[this_class]<-i
        map[this_class]<-max(bracket)*10
    }
    return(map_new)
}


#------------------------------------
# settings 
#------------------------------------
legend.breaks <-
    c("0-20","20-40","40-60","60-80",
      "80-100","100-120","120-140","140-160","160-200") 
#myPalette <-colorRampPalette(c("red4","red","orange","grey","yellow","green","cyan","blue"))
                       
#myPalette <-colorRampPalette(c("magenta4", "red3", "red", "salmon3","orange3", "darkorange", "yellow","green","green4"))

myPalette <-
    colorRampPalette(c("green4","green","yellow","darkorange","orange3","salmon3","red","red3","magenta4"))

breaks<-c(0,20,40,60,80,100,120,140,160,200)
#data<-norm.kcal.cap.nec.sus
#------------------------------------
# convert to a classified map (vector)
# `class_map`
#------------------------------------
# from cow.bin country code starts from 0 and here we are going to match the
# data from norm_kcal_cap_nec_sus, i.e. we start from 1
lpj_country<-read.input.yearband("cow_mg_2006_full.bin",band=1, year=1901,
                                 data.size=2) + 1 
read.input.grid("grid.bin")
country_index<-match(lpj_country, data[,1])
stopifnot(all(!is.na(country_index)))
value<-data[,3]
value_vector<-value[country_index]
class_map<-classify.map(value_vector, breaks)


#---------------------------------------
# plotting
#---------------------------------------
theme_opts <- list(theme(
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         # panel.background=element_blank(),
                         panel.background = element_rect(color = "black",fill=NA, size = .5, linetype = "solid"),
                         #panel.border=element_rect(color = "black",fill=NA, size = 0.4, linetype = "solid"),
                         plot.background = element_blank(),
                         plot.background=element_blank(),
                         panel.border=element_rect(color = "black",fill=NA, size = .5, linetype = "solid"),
                         legend.position="bottom",
                         #panel.border = element_blank(),
                         #axis.line = element_blank(),
                         #axis.text.x = element_blank(),
                         #axis.text.y = element_blank(),
                         #axis.ticks = element_blank(),
                         #panel.margin = unit(5,"lines"),
                         #panel.margin = unit(0,"null"),
                         #plot.margin = rep(unit(0,"null"),4),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text()))

#myPalette <-colorRampPalette(c("red4","red","orange","grey","yellow","green","cyan","blue"))

#myPalette <-colorRampPalette(c("magenta4", "red3", "red", "salmon3","orange3", "darkorange", "yellow","green","green4"))

myPalette <-
    colorRampPalette(c("green4","green","yellow","darkorange","orange3","salmon3","red","red3","magenta4"))
    
p=data.frame(lon,lat,"val"=class_map)
#lines_proj<-graticules(40,20)
#lines<-lines2dataframe(lines_proj)
boundary<-c(min(p$lon),max(p$lon),min(p$lat),max(p$lat))
poly<-readOGR("admin_countries/110m",
              layer="ne_110m_admin_0_countries")
poly<-fortify(poly)

plot.title = paste("Var Coeff Precipitation")
plot.subtitle = "Parameter: daily average tas - Time: 2006-07-01"



gpl<-ggplot(p,aes(lon,lat))+geom_raster(aes(fill=val))+
    scale_fill_gradientn(colours=myPalette(1000),
                         na.value="white",
                         breaks=seq(1,9,length.out=9),
                         labels=legend.breaks,
                         guide= guide_legend(
                                             #guide=guide_colorbar(
                                             title=expression("[%]"), title.position="left",#guide_legend
                                             draw.ulim = T, draw.llim =T ,
                                             barwidth = 22, barheight=1.3)
                         )+
theme_opts+
coord_equal()+
ggtitle(bquote(atop(.(plot.title))))+
scale_x_continuous(limits = c(min(p$lon), max(p$lon)), expand = c(0, 0))+#, breaks=as.vector(text$x[,1]), labels=text$x[,2])+
scale_y_continuous(limits = c(min(p$lat), max(p$lat)), expand = c(0, 0))+#,breaks=as.vector(text$y[,1]), labels=text$y[,2])+
geom_path(data=poly,aes(long,as.numeric(lat),group=group),color="black",size=0.45)

#gpl