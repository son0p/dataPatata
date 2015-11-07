epitafios <- read.csv2("data_epitafios.csv")
x <- (epitafios[1])
x
# no logro que separe las palabras
palabras <- strsplit(x, " ")

palabras
attributes(palabras)
