cluster.plot <-
function(x, center = 2, cor.m = "euclidean",clust.m = "average",xlab = "time in min",ylab = "relative intensity",kmeans.m = T, colorblind.set = T, x.time = c(1,2,3,4)){
temp.data <- x	
na.treat <- "median"
if(na.treat == "median"){
	
	temp.data <- apply(temp.data,1,function(x){
		x <- as.numeric(x)
		x[is.na(x)] <- median(x,na.rm =T)
		return(x)
		
	} )
	temp.data <- t(temp.data)
	
	
}else{
if(is.numeric(na.treat)){
	temp.data[is.na(temp.data )] <- na.treat
}
}



color.blind <- list(	

		yellow = rgb(240,228,66,max = 255),
		orange = rgb(230,159,0,max = 255),
		vermillion = rgb(213,94,0,max = 255),
		bluishgreen = rgb(0,158,115,max = 255),
		reddishpurple = rgb(204,121,167,max = 255), 		skyblue = rgb(86,180,233,max = 255),
		blue = rgb(0,133,178,max = 255)

)

if(length(x.time) != dim(temp.data)[2]){
	x.time <- c(1:dim(temp.data)[2])
	
}
pdf("clusters.pdf")

	temp.clust 	<- dist(temp.data,method = cor.m)
	if(!kmeans.m ){
		clust.temp 	<- hclust(temp.clust,method = "average")	
	
		groups.data <- cutree(clust.temp,center)
		
		
	}else{
		clust.temp 	<- hclust(temp.clust,method = "average")	

		groups.data <- kmeans(temp.clust,center,iter.max = 100)$cluster
	}
	
	clust.temp$labels <- rep("",length(clust.temp$labels))
	plot(clust.temp)
for(i in 1:length(unique(groups.data))){

	temp.i <- groups.data == i
	temp.i.data <- temp.data[temp.i,]
	if(is.vector(temp.i.data)){
		temp.i.data <- t(as.matrix(temp.i.data))
	}
	
	temp.mean <- apply(temp.i.data,2,function(x){x<-as.numeric(x);x<- median(x,na.rm = TRUE)})

	sum.test <- apply(temp.i.data,1,function(x){sum(abs(x - temp.mean),na.rm = TRUE)})
	sum.test <- round(sum.test*10)
	order.test 	<- rev(order(sum.test))
	sum.test	<- (sum.test)
	range.test 	<- max(sum.test)
#ramp <- colorRampPalette(c("red","orange","yellow","green","cyan","blue","purple"))
#test.ramp <- ramp(range.test)


if(colorblind.set){
				test.ramp		<- rev(colorRampPalette(c(unlist(color.blind)[c(7,6,4,3,2,1)]))(max(unique(sum.test))))
			}else{
test.ramp <- rainbow(max(unique(sum.test)),v = 0.88,alpha = 0.7,end = 0.9)
				
		}


	#ramp <- colorRampPalette(c(colors()[338],colors()[276]))
	#test.ramp <- ramp(length(unique(sum.test)))
main.string <- paste("Cluster: ",i,"; Proteins: ",dim(temp.i.data)[1])

	
	matplot( t(matrix(x.time,nrow = dim(temp.i.data)[1],ncol = dim(temp.i.data)[2],byrow = T)),t(temp.i.data),type = "n",col = test.ramp,xlab = "time in s", ylab = ylab,main = main.string)
	grid()
	matpoints( t(matrix(x.time,nrow = dim(temp.i.data)[1],ncol = dim(temp.i.data)[2],byrow = T)),t(temp.i.data),type = "l",col = test.ramp,xlab = "time in s", ylab = ylab)
	points(x.time,temp.mean,col = "red",type = "l",lwd = 3 )
	
	
	
}

graphics.off()
write.csv(cbind(x, groups.data),"cluster.plot.csv")
hz.show.path("clusters.pdf")
}
