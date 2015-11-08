#########################################################
# Fecha: 05/11/2015
# Descripcion: Optimizacion del numero de topicos k de la revista 
#                Associated Press (AP),
#                AP web: "http://www.ap.org"
#              Clasificacion de documentos de la revista 
#                Journal of Statistical Software (JSS),
#                JSS web: "http://www.jstatsoft.org"
# Conferencia: VII Jornadas de Usuarios de R
# Autor: Sergio Contador Pachon
# Email: scontador@ucm.es
#########################################################

####################################
## Instalar paquetes si es necesario:
# install.packages("topicmodels")
# install.packages("tm")                     
# install.packages("slam")
# install.packages("SnowballC")
# install.packages("XML")
####################################

# DOCUMENTOS JSS

# Cargar librerias necesarias
library(topicmodels)
library(tm)
library(slam)
library(SnowballC)
library(XML)

# Obtener documentos revista JSS si es necesario (acceder a "datacube")
# install.packages("corpus.JSS.papers",
#                  repos = "http://datacube.wu.ac.at/", 
#                  type = "source") 

# Cargar documentos
data("JSS_papers", package = "corpus.JSS.papers")

# Visualizar numero de documentos 
dim(JSS_papers)[1]

# Omitir documentos incorporados despues del 05/08/2015
## (lo hacemos para reducir el tamaño de la colección)
JSS_papers <- JSS_papers[JSS_papers[,"date"] < "2010-08-05",]

# Omitir datos que no sean ASCII 
JSS_papers <- JSS_papers[sapply(JSS_papers[, "description"], 
                                Encoding) == "unknown",]

# Visualizar nuevo numero de documentos 
dim(JSS_papers)[1]

# Crear el corpus de los abstractos de los documentos 
remove_HTML_markup <- function(s) {
        doc <- htmlTreeParse(s, asText = TRUE, trim = FALSE)
        xmlValue(xmlRoot(doc))
}

corpus <- Corpus(VectorSource(sapply(JSS_papers[, "description"],
                                     remove_HTML_markup)))

# Cambiar comparador alfabético al idioma en el que estan escritos los textos
## ( en este caso se cambian al ingles)
Sys.getlocale()
Sys.setlocale("LC_COLLATE", "C")
Sys.getlocale()

# Crear matriz DTM
## x es una lista con valores de los parametros de control
x <- list(stemming = TRUE, tolower = TRUE, stopwords = TRUE, 
          minWordLength = 3, removeNumbers = TRUE, 
          removePunctuation = TRUE, stripWhitespace = TRUE)
JSS_dtm <- DocumentTermMatrix(corpus, control = x)

# Conocer algunos valores estadisticos de los datos como la mediana
# range: min y max numero de 1 palabra en los documentos 
range(col_sums(JSS_dtm))  
summary(col_sums(JSS_dtm))

# Crear "term frequent" e "inverse document frequent" (tf-idf).
## tf-idf permite omitir terminos con baja frecuencia (tf) 
## tf-idf permite omitir terminos que occurren en muchos documentos (idf)
term_tfidf <-
        tapply(JSS_dtm$v/row_sums(JSS_dtm)[JSS_dtm$i], JSS_dtm$j, mean) * 
        log2(nDocs(JSS_dtm)/col_sums(JSS_dtm > 0))

# Conocer la mediana
summary(term_tfidf)

# Aplicar  tf-idf a los documentos de JSS.
## El valor de la mediana obtenido para "term_tfidf" se toma como referencia
## para el valor umbral de tf-idf
JSS_dtm <- JSS_dtm[, term_tfidf >= 0.1]
JSS_dtm <- JSS_dtm[row_sums(JSS_dtm) > 0,]

# Conocer algunos valores estadisticos de los nuevos datos como la mediana
summary(col_sums(JSS_dtm))

# Modelar los documentos con LDA_VEM "Topic Modeling":
### LDA_VEM
### LDA_VEM_fixed
### LDA_Gibbs
### CTM_VEM
## Suponer que tenemos 30 topicos
k <- 30

## Necesario para reproducibilidad de los resultados
SEED <- 2010 

## Modelo LDA_VEM 
x <- list(seed = SEED)
LDA_VEM <- LDA(JSS_dtm, k = k, control = x) 

## Modelo LDA_VEM_fixed 
x <- list(estimate.alpha = FALSE, seed = SEED)
LDA_VEM_fixed <- LDA(JSS_dtm, k = k, control = x) 

## Modelo LDA_Gibbs 
x <- list(seed = SEED, burnin = 1000, thin = 100, iter = 1000)
LDA_Gibbs <- LDA(JSS_dtm, k = k, method = "Gibbs", control = x) 

## Modelo CTM_VEM 
x <- list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))
CTM_VEM <- CTM(JSS_dtm, k = k, control = x) 

## Listar los 4 modelos
jss_TM <- list(LDA_VEM = LDA_VEM, LDA_VEM_fixed = LDA_VEM_fixed, 
               LDA_Gibbs = LDA_Gibbs, CTM_VEM = CTM_VEM)

## Visualizar valores del hiperparametro alfa para los modelos
## LDA_VEM y LDA_VEM_fixed
sapply(jss_TM[c(1,2)], slot, "alpha")

## Visualizar valores de la entropia (desorden) para los 4 modelos 
## obtenidos del cálculo de la probabilidad a posteriori
sapply(jss_TM, function(x) mean(apply(posterior(x)$topics, 1, 
                                      function(z) - sum(z * log(z)))))

# Visualizar los 6 primeros terminos de 5 topicos obtenidos con el modelo LDA_VEM
Terms <- terms(jss_TM[["LDA_VEM"]], 6)
Terms[, 1:5]

# Visualizar los 6 primeros terminos de 5 topicos obtenidos con el modelo CTM_VEM
Terms <- terms(jss_TM[["CTM_VEM"]], 6)
Terms[, 1:5]

# Clasificar Volumen 24 revista JSS con el modelo LDA_VEM
(topics_v24 <- topics(jss_TM[["LDA_VEM"]])[grep("/v24/", JSS_papers[, "identifier"])])

## Visualizar topicos obtenidos para los 9 documentos que forman el volumen 24
terms(jss_TM[["LDA_VEM"]], 6)[, topics_v24]

## Clasificacion de los documentos por el tema principal
terms(jss_TM[["LDA_VEM"]], 6)[ 1, topics_v24]

## Clasificacion del volumen por el tema principal
most_frequent_v24 <- which.max(tabulate(topics_v24))
terms(jss_TM[["LDA_VEM"]], 6)[ 1, most_frequent_v24]




# DOCUMENTOS AP
# Cargar librerias necesarias
library(topicmodels)
library(tm)
library(slam)
library(SnowballC)
library(XML)


## Crear Directorios 
## (modificar para su utilizacion en otra maquina)
dir.Principal <- paste(getwd(), "/R/Programas/TopicModel", sep = "")
if (!file.exists(dir.Principal)){
        dir.create(file.path(dir.Principal))}

dir <-paste(dir.Principal,"/AP_TopicModel", sep = "")
if (!file.exists(dir)){
        dir.create(file.path(dir))}

dir <-paste(dir,"/results", sep = "")
if (!file.exists(dir)){
        dir.create(file.path(dir))}

# Cargar documentos
data("AssociatedPress", package = "topicmodels")

# Visualizar numero de documentos 
dim(AssociatedPress)[1]

# Cambiar comparador alfabetico al idioma en el que estan escritos los textos
## ( en este caso se cambian al ingles)
Sys.getlocale()
Sys.setlocale("LC_COLLATE", "C")
Sys.getlocale()

## Crear Modelo Usando "6-fold cross-validation"
set.seed(0908)
SEED <- 20080809
(topics <- c(10, 20, 30, 40, 50, 100, 200))
(folds <- c(1, 2, 3, 4, 5, 6)) 
D <- nrow(AssociatedPress)
folding <- sample(rep(seq_len(length(folds)), ceiling(D))[seq_len(D)])
system.time(
        for (k in topics){
                for (chain in folds){          
                        x <- list(seed = SEED)
                        training <- LDA(AssociatedPress[folding != chain,], k = k, control = x)
                        x <- list(estimate.beta = FALSE, seed = SEED)
                        testing <- LDA(AssociatedPress[folding == chain,], model = training, control = x)
                        FILE <- paste(dir, "/LDA_VEM_", k, "_", chain, ".rda", sep = "")
                        save(training, testing, file = FILE)
                        print(paste("Model: LDA_VEM",";", " Topics:", k, ";", " Fold:", chain, sep = ""))
                }
        }
)

## Resultados "6-fold cross-validation"
set.seed(0908)
SEED <- 20080809
(topics <- c(10, 20, 30, 40, 50, 100, 200))
(folds <- c(1, 2, 3, 4, 5, 6)) 
data("AssociatedPress", package = "topicmodels")
D <- nrow(AssociatedPress)
folding <- sample(rep(seq_len(length(folds)), ceiling(D))[seq_len(D)])
AP_test <- list()
system.time(
        for (method in c("LDA_VEM")){
                AP_test[[method]] <- matrix(NA, nrow = length(topics), ncol = 10, dimnames = list(topics, seq_len(10)))
                for (fold in seq_len(10)){
                        for (i in seq_along(topics)){
                                T <- topics[i]
                                FILE <- paste(method, "_", T, "_", fold, ".rda", sep = "")
                                load(file.path(dir, FILE))
                                AP_test[[method]][paste(T),fold] <- perplexity(testing, AssociatedPress[folding == fold,], use_theta = FALSE)
                                print(paste("Model: LDA_VEM",";", " Topics:", T, ";", " Fold:", fold, sep = ""))
                        }
                }
        }
)

AP_test
save(AP_test, file = "AP.rda")

# Plot "perplexity"
AP_test.frame <- as.data.frame(AP_test)
plot(topics, AP_test.frame[ , 1], xlim = c(0, 200), ylim = c(0, 10000), main = "LDA_VEM model", xlab = "Number of Topics", ylab = "Perplexity", type = "b", col = "blue", lwd = 3)
lines(topics, AP_test.frame[, 2], type = "b", col = "green", lwd = 3) 
lines(topics, AP_test.frame[, 3], type = "b", col = "yellow", lwd = 3) 
lines(topics, AP_test.frame[, 4], type = "b", col = "salmon", lwd = 3) 
lines(topics, AP_test.frame[, 5], type = "b", col = "brown", lwd = 3) 
lines(topics, AP_test.frame[, 6], type = "b", col = "magenta", lwd = 3) 
lines(topics, AP_test.frame[, 7], type = "b", col = "red", lwd = 3) 
lines(topics, AP_test.frame[, 8], type = "b", col = "black", lwd = 3) 
lines(topics, AP_test.frame[, 9], type = "b", col = "orange", lwd = 3) 
lines(topics, AP_test.frame[, 10], type = "b", col = "gold", lwd = 3) 

legend("topright", pch = 1, col = c("blue", "green", "yellow", "black", "brown", "magenta"), legend = c("fold 1", "fold 2", "fold 3", "fold 4", "fold 5", "fold 6"))
