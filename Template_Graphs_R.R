# TEMPLATE FOR GRAPHS IN R -> Take as example file "EXTRA_Exercise_1.2c).R"


#SETUP-------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(kableExtra)

#1 Definition old Parameters---------------

B <- 1                                                    
a <- 1/3
o <- 0.1
s <- 0.3
n1 <- 0.05


#2 Definition new Parameters-----------------

n2 <- 0


# MODELLING: Capital------------

# old steady-state k0*
k0 <- (B^(1/(1-a))) * (s/(n1+o))^(1/(1-a))

# new steady-state k*
ks <- (B^(1/(1-a))) * (s/(n2+o))^(1/(1-a))

# evolution kt 1:100
kt_column <- data.frame()
kt_column["1", "kt"] <- (1/(1+n1)) * (s*B*((k0)^a) + ((1-o) * k0)) # here we wrote literally the formula as it is written in the book
for (i in 2:100) {
  kt_column[i, "kt"] <- (s*B*((kt_column[i-1, "kt"])^a) + ((1-o) * kt_column[i-1, "kt"]))
}

View(kt_column)


#MODELLING: GDP---------------------

# old steady state y0* 
y0 <- B*(k0)^a

# new steady-state y* 
ys <- B*(ks)^a

# evolution yt 1:100
yt_column <- data.frame()
for (i in 1:100) {
  yt_column[i,"yt"] <- B*(kt_column[i, "kt"])^a
}

View(yt_column)


#MODELLING: Consumption-------------------

# old steady state c0*
c0 <- B^(1/(1-a)) * (1-s) * (s/(n1+o))^(a/(1-a))

# new steady state c*
cs <- B^(1/(1-a)) * (1-s) * (s/(n2+o))^(a/(1-a))

# evolution ct 1:100
ct_column <- data.frame()
for (i in 1:100) {
  ct_column[1, "ct"] <- (1-s) * y0
  ct_column[i, "ct"] <- (1-s) * yt_column[i, "yt"]
}

View(ct_column)  


#MODELLING: Real Wage--------

# evolution of wt 1:100
wt_column <- data.frame()
for (i in 1:100) {
  wt_column[1,"wt"] <- (1-a) * B * (k0)^a
  wt_column[i, "wt"] <- (1-a) * B * (kt_column[i, "kt"])^a
}

View(wt_column)


#MODELLING: Rental Rate and Interest Rate-----------
 
# old steady state r0* and p0*
r0 <- a*B*(k0)^(a-1)
p0 <- r0 - o

# new steady state rs* and ps*
rs <- a*B*(ks)^(a-1)
ps <- rs - o

# evolution of rt & pt 1:100
rt_column <- data.frame()
pt_column <- data.frame()
for (i in 1:100) {
  rt_column[1,"rt"] <- a*B*(k0)^(a-1)
  rt_column[i, "rt"] <- a*B*(kt_column[i,"kt"])^(a-1)
}
for (i in 2:100) {  
  pt_column[1,"pt"] <- rt_column[1,"rt"] - o
  pt_column[i,"pt"] <- rt_column[i-1,"rt"] - o
}

View(rt_column)
View(pt_column)


#MODELLING: GDP Growth------------

# evolution of yt growth
ytgrowth_column <- data.frame()
for (i in 2:100) {
  ytgrowth_column[1,"yt_growth%"] <- 0
  ytgrowth_column[i,"yt_growth%"] <- ((yt_column[i,"yt"] - yt_column[i-1,"yt"]) / yt_column[i-1,"yt"]*100)
}

View(ytgrowth_column)


#Table with all columns together---------

library(tidyverse)
Data<-(tibble(kt_column,yt_column, ct_column, wt_column,
              rt_column, pt_column, ytgrowth_column))
View(Data)

 
#Data Visualisation -> Graphs---------

library(ggplot2)
Time <- 1:100
Graph <- ggplot() +
  geom_line(data = yt_column, aes(x = Time, y = yt_column[,"yt"])) +
  geom_line(data = ct_column, aes(x = Time, y = ct_column[,"ct"]), color = "blue") +
  geom_line(data = wt_column, aes(x = Time, y = wt_column[,"wt"]), color = "green") +
  geom_line(data = pt_column, aes(x = Time, y = pt_column[,"pt"]), color = "red") +
  geom_line(data = ytgrowth_column, aes(x = Time, y = ytgrowth_column[,"yt_growth%"]), color = "yellow")+
  ylab("yt,  ct,  wt,  pt,  yt_growth") +
  xlab("Time")

Graph


#Saving Graph in pdf or jpeg---------------

ggsave("Graph.pdf", path = "Bachelor")   # you basically define the format of the saving file by its name 
ggsave("Graph.jpeg")


#Latex: Importing Tables & Tibbles to LaTex-------------

Data_Latex <- kable(Data,
      row.names = FALSE,
      digits = 2,
      caption = "Name",
      format = 'latex', booktabs = TRUE
)

Data_Latex

