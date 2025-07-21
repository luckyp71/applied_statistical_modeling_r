require(dplyr)
require(ggplot2)
require(plotly)
require(datasets)

normdis <- rnorm(n=100, mean=30, sd=3)

df <- as.data.frame(normdis)
names(df) <- c('data')

v <- df%>%
  ggplot(aes(x=data))+
  geom_histogram(aes(fill=I('orange'),
                     text=paste0(
                       'x: ', round(..x.., 2),
                       '<br>Frequency: ', ..count..
                     )
                     ), binwidth = 1)+
  theme_minimal()


ggplotly(v, tooltip='text')
