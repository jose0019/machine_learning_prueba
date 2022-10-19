#'## PRÁCTICA HERRAMIENTAS DE PROGRAMACIÓN


#'### **Librerias**
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(purrr)


#'**Ejercicio 1**
df_original<-read_csv("mktmix.csv")

df_mmm<- df_original %>% 
  clean_names()

#vemos como quedan los nombres de las columnas
names(df_mmm)


#'**Ejercicio 2**
#con la siguiente función tenemos una visión de la estructura del dataframe
str(df_mmm)

#vemos el número de columnas del data frame
ncol(df_mmm) 

#vemos el número de filas del data frame
nrow(df_mmm) 

#vemos que la clase de base_price es numeric
class(df_mmm$base_price)
#comprobación para ver si base_price es de tipo numeric
is.numeric(df_mmm$base_price)

#vemos que la clase de discount es numeric
class(df_mmm$discount)
#comprobación para ver si discount es de tipo numeric
is.numeric(df_mmm$discount)

#con esta función vemos un resumen del data frame por columnas
glimpse(df_mmm)
#la información que contienen base_price y discount son números reales ya que
#su clase es numeric y base_price contendrá los precios originales de los 
#productos. Discount mostrará los descuentos de los productos


#'**Ejercicio 3**
df_mmm<-df_mmm %>% 
  mutate(newspaper_inserts=if_else(is.na(df_mmm$newspaper_inserts),0,1))

#comprobamos que ha pasado a ser numeric
class(df_mmm$newspaper_inserts) 


#'**Ejercicio 4**
#vemos los valores distintos de la columna website_campaign
df_mmm %>% 
  distinct(website_campaign)
#hay 3 valores distintos en la columna website_campaign (sin contar NA)

#cambio el valor de los NA de la columna website_campaign por el valor a
df_mmm$website_campaign[is.na(df_mmm$website_campaign)] <- "a"

#creo las nuevas columnas para las distintas categorias
df_mmm<-df_mmm %>% 
  mutate(Facebook=if_else(website_campaign=="Facebook", 1,0)) %>% 
  mutate(Twitter=if_else(website_campaign=="Twitter",1,0)) %>% 
  mutate(WebsiteCampaign=if_else(website_campaign=="Website Campaign",1,0)) 

#compruebo que se han modificado los valores mediante la función comentada
#View(df_mmm)


#'**Ejercicio 5**
#calculo las semanas de campaña de Facebook
df_mmm %>% 
  filter(Facebook==1) %>% 
  count()
#4 semanas de campaña Facebook

#Calculo las semanas de campaña de Twitter
df_mmm %>% 
  filter(Twitter==1) %>% 
  count()
#4 semanas de campaña Twitter


#'**Ejercicio 6**
df_mmm %>% 
  filter(tv<50) %>% 
  select(tv)
#la respuesta son 3 semanas de inversión de menos de 50 grp


#'**Ejercicio 7**
#se considera que no hay inversion cuando los valores de radio son NA y 0

#mediante el siguiente código remplazamos los valores NA de radio por 0
df_mmm$radio[is.na(df_mmm$radio)] <- 0

#el siguiente código incluye la columna RadioEdit que son los valores de la
#columna radio pero con valor 1 si hay inversión en radio (es decir radio>0)
#y valor 0 si no hay inversión en radio (es decir radio=0)
df_mmm<-df_mmm %>% 
  mutate(RadioEdit=if_else(radio==0,0,1))

#mediante el siguiente código agrupamos por la nueva columna (RadioEdit) y 
#mostramos un resumen mediante la función summarise en función de la media de
#inversión en tv para los dos valores de la columna RadioEdit
df_mmm %>%
  group_by(RadioEdit) %>%
  summarise(media_tv=mean(tv))


#'**Ejercicio 8**
ggplot(df_mmm) +
    geom_line(aes(x=1:104, y=new_vol_sales))


#'**Ejercicio 9**
ggplot(df_mmm)+
  geom_histogram(aes(x=new_vol_sales))

ggplot(df_mmm)+
  geom_boxplot(aes(y=new_vol_sales))
#a la vista de los gráficos la mediana sería 20000 y la media sería 
#muy parecida a la mediana
  

#'**Ejercicio 10**
#para este ejercicio he cogido el df_mmm del ejercicio 1 ya que en ejercicios
#posteriores dimos valor 0 a los NA de la columna radio
df_mmm<- df_original %>% 
  clean_names()

df_media<-df_mmm %>% 
  select(tv,radio,stout)

df_media<-df_media %>% 
  pivot_longer(everything())

ggplot(df_media)+
  geom_line(aes(x=1:312, y=value))+
  facet_grid(name ~ ., scales = "free_y")
#el gráfico nos muestra los diferentes valores de las columnas radio, stout y tv
#es decir, esta representando la cantidad de inversión en radio, stout y tv.
#Del gráfico podemos destacar la inestable inversión que hay debido a los
#frecuentes mínimos y máximos


#'**Ejercicio 11**
ggplot(df_mmm, aes(y=new_vol_sales, x=in_store))+
  geom_point()+
  geom_smooth()
#como vemos en el gráfico a medida que aumenta el valor de new_vol_sales se 
#incrementa el valor de in_store. La variable new_vol_sales es el volumen de 
#ventas y in_store es el stock disponible en las tiendas para vender el producto
#por lo que es lógico pensar que ambas variables están correlacionadas y a 
#medida que aumenta el stock aumentará el volumen de ventas, ya que hay más 
#oferta disponible para aumentar las ventas.


#'**Ejercicio 12**
ggplot(df_mmm)+
  geom_point(aes(y=new_vol_sales, x=in_store, colour=as.factor(newspaper_inserts)))

ggplot(df_mmm)+
  geom_point(aes(y=new_vol_sales, x=in_store, colour=tv))


#'**Ejercicio 13**
df_mmm %>% 
  mutate(discount_yesno=if_else(discount>0,1,0)) %>% 
  group_by(discount_yesno) %>% 
  summarise(media_baseprice=mean(base_price)) %>% 
  print() %>% 
  ggplot()+
  geom_col(aes(x=discount_yesno, y=media_baseprice))


#'**Ejercicio 14**
ajuste<-function(vector){
  df_aux<-df_mmm %>% 
    select(all_of(vector), new_vol_sales)
  my_model<-lm(new_vol_sales ~ ., data=df_aux)
  p<-summary(my_model)$adj.r.squared
  return(p)
}
#llamamos a la función con el siguiente vector de entrada
ajuste(c("tv","radio"))


#'**Ejercicio 15**
#creo una lista con los vectores
lista_vectores<- list(c("base_price","radio","tv","stout"),
                      c("base_price","in_store","discount","radio","tv","stout"),
                      c("in_store","discount"))

#llamamos a la función para cada uno de los vectores
map_dbl(lista_vectores, ajuste)                     
#el mejor modelo es el que tiene el mayor número de variables, ya que el r
#cuadrado ajustado es mayor por lo que tendrá menor error al explicar el modelo.

