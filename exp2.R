require(datasets)
require(dply)
require(ggplot2)
require(plotly)

names(iris)
names(iris) <- tolower(gsub('\\.', '_', names(iris)))
names(iris)

#graphics.off()

# histogram
ggplot(iris, aes(x=sepal_length))+
  geom_histogram(aes(fill=species))+
  labs(
    x='Sepal Length',
    y='Frequency',
    title='Sepal Length by Species',
    fill='Species Name'
  )
  theme_minimal()

# boxplot
ggplot(iris, aes(x=species, y=sepal_length))+
  geom_boxplot(aes(color=species))+
  labs(
    x='Species',
    y='Sepal Length'
  )+
  theme_minimal()
  
# relation between 2 quantitative variables
v <- ggplot(iris, aes(x=sepal_length, y=sepal_width))+
  geom_point(aes(color=species, size=1.5))+
  theme_minimal()

ggplotly(v)


# Ploting categorical or count variables

str(mtcars)
View(mtcars)

v <- ggplot(mtcars, aes(x=gear))+
  geom_bar(aes(fill = 'orange'))+
  labs(
    x='Gear',
    y='Frequency',
    title='Gear Frequency'
  )+
  theme_minimal()

ggplotly(v)

# Facet
v <- ggplot(iris, aes(x=sepal_length, y=sepal_width))+
  geom_point(aes(color=species, size=1))+
#  facet_grid(. ~ species)+
  facet_grid(species ~ .)+
  geom_smooth(method = 'lm')+
  labs(
    x='Sepal Length',
    y='Sepal Width'
  )+
  theme_minimal()+
  theme(legend.position = "none")

ggplotly(v)



