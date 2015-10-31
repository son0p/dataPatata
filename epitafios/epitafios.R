epitafios <- read.csv2("data_epitafios.csv")
x <- (epitafios[1])
palabras <- strsplit(x, " ")
palabras
attributes(palabras)
