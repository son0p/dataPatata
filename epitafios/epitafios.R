install.packages('tm')
require('tm')

# usando el paquete tm
epitafios <- read.csv2("data_epitafios.csv")
corpus <- VCorpus(VectorSource((epitafios)))
dtm <- DocumentTermMatrix(corpus)
findFreqTerms(dtm,5)

#------- experimentos ----------
epitafios <- read.csv2("data_epitafios.csv")
#epitafios <- c("mi gran amigo", "mi hermano querido", "mi mi mi")
x <- (epitafios[1])
x

epitafios

# no logro que separe las palabras
palabras <- strsplit(as.character(x), " ")

palabras
attributes(palabras)
