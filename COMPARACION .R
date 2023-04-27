library(haven)
library(survey)
library(tidyverse)
library(haven)
library(magrittr)
options(scipen = 999)
library(dplyr)
library(ggplot2) 
library(readxl)
library(gmodels)
library(Hmisc)
library(ggthemes)
library(convey)


## CASEN

Casen_2017 <- read_dta("BASES/Casen 2017.dta")

DCCASEN <- svydesign(id = ~varunit, strata = ~varstrat, weights =
~expr, nest = TRUE, data = Casen_2017)
options(survey.lonely.psu="remove")
DCCASEN <- convey_prep(DCCASEN)

# ESI 2021
load("BASES/esi-2021---personas.rdata")
ESI_2021 <- base

DCESI <- svydesign(id = ~conglomerado, strata = ~estrato, 
weights = ~fact_cal_esi, nest = TRUE, data = ESI_2021) 
options(survey.lonely.psu="remove")
DCESI <- convey_prep(DCESI)


# Coeficiente Gini (expandido)
CGINI <- subset(DCCASEN, !is.na(yoprcor ))
ginicasen <- as_tibble(svygini(~yoprcor, design = CGINI))
names(ginicasen) <- c("gini", "se")
ginicasen %<>% mutate(Fuente = "Casen")

EGINI <- subset(DCESI, !is.na(ing_t_p ) & ocup_ref==1)
giniesi <- as_tibble(svygini(~ing_t_p, design = EGINI))
names(giniesi) <- c("gini", "se")
giniesi %<>% mutate(Fuente = "ESI")

gini <- rbind(ginicasen, giniesi)


# Kernel density no expandido
knexp <- ggplot() +
geom_density(data = Casen_2017, aes(log(yoprcor),fill = "Casen"), alpha = .2) + 
geom_density(data = base, aes(log(ing_t_p),fill = "ESI"), alpha = .2) +
scale_fill_manual(name = "Fuente: ", values = c("Casen" = "blue", "ESI" = "green")) +
theme_minimal() + theme(legend.position = "bottom", aspect.ratio = .6) +
labs(x = "\nLogaritmo del ingreso", y = "Densidad\n", 
 title = "Distribución del logaritmo de ingresos por fuente" ) +
coord_cartesian(ylim = c(0, 1.350), expand = F, clip = "off") +
theme(legend.position = c(0.5,-.3), legend.direction="horizontal",legend.text = element_text(size=10)) +
geom_vline(aes(xintercept = log(mean(base$ing_t_p[base$ocup_ref==1],na.rm=TRUE))),
 linetype="dashed",color="green") +
geom_vline(aes(xintercept = log(mean(Casen_2017$yoprcor,na.rm=TRUE))),
 linetype="dashed",color="blue")+
theme(aspect.ratio = .6,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 4, 5, 3), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2)) 


# Kernel density expandido
kexp <- ggplot() +
geom_density(data = Casen_2017, aes(log(yoprcor), weight = expr, fill = "Casen"), alpha = .2) + 
geom_density(data = base[(!is.na(base$ing_t_p ) & base$ocup_ref==1),], aes(log(ing_t_p), weight = fact_cal_esi, fill = "ESI"), alpha = .2) +
scale_fill_manual(name = "Fuente: ", values = c("Casen" = "Blue", "ESI" = "green")) +
theme_minimal() + theme(legend.position = "bottom", aspect.ratio = .6) +
labs(x = "\nLogaritmo del ingreso", y = "Densidad\n", 
 title = "Distribución del logaritmo de ingresos por fuente" ) +
coord_cartesian(ylim = c(0, 1.350), expand = F, clip = "off") +
theme(legend.position = c(0.5,-.3), legend.direction="horizontal",legend.text = element_text(size=10)) +
geom_vline(aes(xintercept = log(svymean(~ing_t_p, EGINI))),
 linetype="dashed",color="green") +
geom_vline(aes(xintercept = log(svymean(~yoprcor, CGINI))),
 linetype="dashed",color="blue")+
theme(aspect.ratio = .6,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 4, 5, 3), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2)) 




#########################
# Curva de Lorenz (expandido)
par(mfrow=c(1,2))
svylorenz(~yoprcor, CGINI, seq(0,1,.005))
title("A: Curva Lorenz en Casen")
svylorenz( ~ing_t_p , EGINI, seq(0,1,.005))
title("B: Curva Lorenz en ESI")

lorenzcasen <- svylorenz( ~yoprcor , CGINI, seq(0,1,.005))
lorenzesi <- svylorenz( ~ing_t_p , EGINI, seq(0,1,.005))

qcasen <- t(lorenzcasen$quantiles)
qesi <- t(lorenzesi$quantiles)
percentiles <- as_tibble(rownames(qcasen))
qlorenz <- cbind(percentiles,as_tibble(qcasen))
qlorenz <- cbind(qlorenz,as_tibble(qesi))

glorenz <- ggplot(qlorenz, aes(as.numeric(value)*100)) + 
geom_line(aes(y = yoprcor*100, colour = "Casen")) + 
geom_line(aes(y = ing_t_p*100, colour = "ESI")) +
labs(x = "\nPorcentaje de población acumulada", y = "Porcentaje de ingreso total\n", 
 title = "Curva Lorenz de ingresos por fuente" ) +
coord_cartesian(ylim = c(0, 100), expand = FALSE, clip = "off") +
theme(legend.position = c(0.5,-.25), legend.direction="horizontal",legend.text = element_text(size=10)) +
labs(colour="Fuente: ") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
breaks=seq(0,100,10)) +
scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
breaks=seq(0,100,10)) +
theme(aspect.ratio = .6,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 4, 5, 3), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2)) +
geom_abline(intercept = 0, slope = 1, colour = "red", linewidth = .2)

glorenz 



estimacion <- svyby(~yoprcor, by = ~sexo, na.rm.all = FALSE,
subset(DCCASEN, yoprcor > .1 ), svymean,
covmat = T, vartype = "se")
contraste <- svycontrast(estimacion, quote(`1` - `2`))
contraste %<>%
as_tibble() %>%
mutate("p.value" = pt(abs(c(contraste[1]/SE(contraste))), df = 60,
lower.tail = FALSE)*2)





# Curva de distribución de probabilidad acumulada
Casen_2017%<>% filter(yoprcor >0)
Casen_2017 <- Casen_2017[order(Casen_2017$yoprcor), ]# Won't change anything since it was created sorted
Casen_2017$cum.pct <- with(Casen_2017, cumsum(expr) / sum(expr))
ggplot(Casen_2017, aes(log(yoprcor), cum.pct)) + geom_line()




# Curva de distribución de probabilidad acumulada
base%<>% filter(!is.na(ing_t_p ) & ocup_ref==1)
base <- base[order(base$ing_t_p), ]# Won't change anything since it was created sorted
base$cum.pct <- with(base, cumsum(fact_cal_esi) / sum(fact_cal_esi))
ggplot(base, aes(log(ing_t_p), cum.pct)) + geom_line()

pacum <- ggplot() + 
geom_line(data=base, aes(x=log(ing_t_p), cum.pct*100, color='ESI')) + 
geom_line(data=Casen_2017, aes(x=log(yoprcor), cum.pct*100, color='Casen')) +
labs(x = "\nLogaritmo del ingreso", y = "Porcentaje de población acumulada\n", 
 title = "Curva de distribución de probabilidad acumulada" ) +
coord_cartesian(ylim = c(0, 100), expand = FALSE, clip = "off") +
theme(legend.position = c(0.5,-.25), legend.direction="horizontal",legend.text = element_text(size=10)) +
labs(colour="Fuente: ") +
scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
breaks=seq(0,100,10)) +
scale_x_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
 breaks=seq(7,18,1)) +
theme(aspect.ratio = .6,
plot.title=element_text(size=12,face="bold"),
plot.subtitle=element_text(size=10),
axis.title=element_text(size=8, vjust = -6),
plot.caption = element_text(vjust = -45, face = "italic"),
plot.margin = unit(c(1, 4, 5, 3), "lines"),
plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2)) 


## Indicadores

kuesi <- sapply(base[,c("ing_t_p")], weigths= base$fact_cal_esi, Kurt,na.rm=TRUE	
)
kucasen  <- sapply(Casen_2017[,c("yoprcor")], weigths= Casen_2017$expr, Kurt,na.rm=TRUE)
skcasen  <- sapply(Casen_2017[,c("yoprcor")], weigths= Casen_2017$expr, Skew,na.rm=TRUE)
skesi <- sapply(base[,c("ing_t_p")], weigths= base$fact_cal_esi, Skew,na.rm=TRUE	
)

pe <-svymean(~ing_t_p,EGINI)
ipe <-confint( svymean(~ing_t_p,EGINI))
pc <- svymean(~yoprcor,CGINI)
ipc <- confint(svymean(~yoprcor,CGINI))
               

pere <- svyquantile(~ing_t_p,EGINI, c(0,.25,.5,.75,.95,1))
perc <- svyquantile(~yoprcor,CGINI, c(0,.25,.5,.75,.95,1))


i <- c(kucasen, kuesi, skcasen, skesi)


gini$indicador <- "Coeficiente Gini"
names(gini)[1] <- "Estimación"

k <- as_tibble(i[1:2])
k$se <- 0
k$Fuente <- c("Casen", "ESI")
k$indicador <- "Kurtosis"
names(k)[1] <- "Estimación"


s <- as_tibble(i[3:4])
s$se <- 0
s$Fuente <- c("Casen", "ESI")
s$indicador <- "Skewness"
names(s)[1] <- "Estimación"

pe <- as_tibble(pe)
pe$indicador <- "Promedio"
pe$Fuente <- "ESI"
names(pe)[c(1,2)] <- c("Estimación", "se")
pe$se <- as.numeric(pe$se)

pc <- as_tibble(pc)
pc$indicador <- "Promedio"
pc$Fuente <- "Casen"
names(pc)[c(1,2)] <- c("Estimación", "se")
pc$se <- as.numeric(pc$se)



perc <- as.data.frame(perc$yoprcor)
perc$indicador <- c("Mínimo", "p25", "Mediana", "p75", "p95", "Máximo")
perc$Fuente <- "Casen"
names(perc)[c(1)] <- c("Estimación")


pere <- as.data.frame(pere$ing_t_p)
pere$indicador <- c("Mínimo", "p25", "Mediana", "p75", "p95", "Máximo")
pere$Fuente <- "ESI"
names(pere)[c(1)] <- c("Estimación")


resumen <- bind_rows(s,k,pe,pc, pere[,-c(2,3)], perc[,-c(2,3)])

p <-    ggplot(data = resumen[-c(1:4,12,18),], aes(y = Estimación, x = indicador, fill = factor(Fuente) )) +
  geom_bar(stat = "identity", position=position_dodge())+
  labs( title = "Indicadores de ingreso según fuente" ) +
  coord_cartesian(ylim = c(0, 2020000), expand = FALSE, clip = "off") +
  theme(legend.position = c(0.5,-.25), legend.direction="horizontal",legend.text = element_text(size=10)) +
  labs(fill="Fuente: ") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     breaks=seq(0,2000000,200000)) +
  theme(aspect.ratio = .6,
        plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=10),
        axis.title=element_text(size=8, vjust = -6),
        plot.caption = element_text(vjust = -45, face = "italic"),
        plot.margin = unit(c(1, 4, 5, 3), "lines"),
        plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
        panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
        panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2)) 

p1 <-    ggplot(data = resumen[c(1:2),], aes(y = Estimación, x = indicador, fill = factor(Fuente) )) +
  geom_bar(stat = "identity", width = 0.5,
           position=position_dodge(width = 1))+
  expand_limits(x = c(0.5,1.5), y = 0) +
  labs( title = "Indicador Skewness de ingreso según fuente" ) +
  coord_cartesian(ylim = c(0, 16), expand = FALSE, clip = "off") +
  theme(legend.position = c(0.5,-.2), legend.direction="horizontal",legend.text = element_text(size=10)) +
  labs(fill="Fuente: ") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     breaks=seq(0,20,2)) +
  theme(aspect.ratio = .9,
        plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=10),
        axis.title=element_text(size=8, vjust = -6),
        plot.caption = element_text(vjust = -45, face = "italic"),
        plot.margin = unit(c(1, 4, 5, 3), "lines"),
        plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
        panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
        panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2)) 


p2 <-    ggplot(data = resumen[c(3:4),], aes(y = Estimación, x = indicador, fill = factor(Fuente) )) +
  geom_bar(stat = "identity", width = 0.5,
           position=position_dodge(width = 1))+
  expand_limits(x = c(0.5,1.5), y = 0) +
  labs( title = "Indicador Kurtosis de ingreso según fuente" ) +
  coord_cartesian(ylim = c(0, 520), expand = FALSE, clip = "off") +
  theme(legend.position = c(0.5,-.2), legend.direction="horizontal",legend.text = element_text(size=10)) +
  labs(fill="Fuente: ") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE),
                     breaks=seq(0,500,50)) +
  theme(aspect.ratio = .9,
        plot.title=element_text(size=12,face="bold"),
        plot.subtitle=element_text(size=10),
        axis.title=element_text(size=8, vjust = -6),
        plot.caption = element_text(vjust = -45, face = "italic"),
        plot.margin = unit(c(1, 4, 5, 3), "lines"),
        plot.background = element_rect(fill = "#F9FAEA", colour="white"), 
        panel.background = element_rect(fill = "#F5F8F0", colour="#6A9CE8"),
        panel.grid.major = element_line(colour = "#036125", linetype = "dotted"),
        panel.grid.minor = element_line(colour = "#FDFEEE", linewidth = .2)) 
