library (tidyverse)
library (gridExtra)

graph1 = ggplot(data=mpg, mapping=aes(x=displ,y=hwy))+
	geom_point(color = "black", size = 5)+
	geom_smooth (color="blue")


graph2 = ggplot(data=mpg, mapping=aes(x=displ,y=hwy, color=drv))+
	geom_point()+
	geom_smooth ()


graph3 = ggplot(data=mpg, mapping=aes(x=displ,y=hwy))+
	geom_point(mapping=aes(color=drv))+
	geom_smooth (color="blue")


graph4 = ggplot(data=mpg, mapping=aes(x=displ,y=hwy, color=drv))+
	geom_point()+
	geom_smooth(mapping = aes(linetype = drv), color="blue")


graph5 = ggplot(data=mpg, mapping=aes(x=displ,y=hwy, color=drv))+
	geom_point(stroke = 5)

	
grid.arrange(graph1,graph2,graph3,graph4,graph5)


