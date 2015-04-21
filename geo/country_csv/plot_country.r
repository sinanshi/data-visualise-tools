source("../../../LPJU/LPJmL_RUtil/R/read.input.r")
source("../../../LPJU/LPJmL_RUtil/R/map.r")

library("RColorBrewer")
library("fields")
library("ggplot2")

#myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
myPalette <- colorRampPalette(rev(c("lightgoldenrod","yellow","darkolivegreen1","greenyellow","green","green4")))



read.input.grid("grid.bin")
cow=read.input.yearband(filename="cow_mg_2006_full.bin",band=1,year=1901,data.size=2)

rwru=read.csv("RWRU.csv",sep=",")
data=data.frame(lpj_cow=rwru$LPJ.running.Code..from.lu.time.series.,name=rwru$km3,val=rwru$val)


data$lpj_cow[data$lpj_cow == 0] <- NA
data <- data[!is.na(data$lpj_cow),]
data$lpj_cow=data$lpj_cow-1 # convert to LPJ code
country.standard<-read.csv("countries.csv",sep="\t")

country.index<-sort(unique(cow))

val<-array(NA, length(cow))

for(i in country.index)
{
    if( length(which(data$lpj_cow == i)) == 0){
        cat("country not found in the input table: ",as.character(country.standard$Country[country.standard$Index==i]),"\n")
    }
    else
    {
        val[cow==i] <- data$val[data$lpj_cow==i]
    }
}


library(rgdal)
library(raster)
source("../../../../visotmed/R/header.r")
#------
#plot
#------
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



p=data.frame(lon,lat,val)
lines_proj<-graticules(40,20)
lines<-lines2dataframe(lines_proj)

boundary<-c(min(p$lon),max(p$lon),min(p$lat),max(p$lat))
text<-gratText(lines_proj,boundary)

poly<-readOGR("/Users/sinan/workspace/OT-Med/visotmed/data/admin_countries/110m/",layer="ne_110m_admin_0_countries")
poly<-fortify(poly)

plot.title = paste("Land Availability per Capita (Unused)")
#plot.subtitle = "Parameter: daily average tas - Time: 2006-07-01"


gpl<-ggplot(p,aes(lon,lat))+geom_raster(aes(fill=val))+
scale_fill_gradientn(colours=myPalette(1000), na.value="white",breaks=seq(1,0,length.out=10),
		     	labels=round(seq(1,0,length.out=10),digits=1),
		     guide= guide_legend(
		     #guide=guide_colorbar(
		     title=expression("[Ha/Capita/yr]"), title.position="left",#guide_legend
			     draw.ulim = T, draw.llim =T ,
			     barwidth = 22, barheight=1.3))+
 theme_opts+
 coord_equal()+
 ggtitle(bquote(atop(.(plot.title))))+
 scale_x_continuous(limits = c(min(p$lon), max(p$lon)), expand = c(0, 0), breaks=as.vector(text$x[,1]), labels=text$x[,2])+
 scale_y_continuous(limits = c(min(p$lat), max(p$lat)), expand = c(0, 0),breaks=as.vector(text$y[,1]), labels=text$y[,2])+
geom_path(data=poly,aes(long,as.numeric(lat),group=group),color="black",size=0.45)+
geom_path(data=lines,aes(lon,as.numeric(lat),group=group),color="black",size=0.15,linetype=2)#graticules



