# Trabalho 1 Métodos Aplicados - Carolyne e Camila -----
# Carregando pacotes -----
library(pacman)
pacman::p_load(tidyverse, hms, scales, ggplot2, lubridate, dplyr, readr, gridExtra,
               copula, fBasics, StableEstim, stabledist, PerformanceAnalytics,
               extRemes,ismev,evmix,extremis,DT,kableExtra,VGAM,evd,fExtremes,
               graphics, ggExtra, patchwork)

load("~/Estudos/Métodos Aplicados/Dados/Trabalho CÓDIGOS.RData")

# --------------------------------------------------
# Lendo e limpando dados -------
#Definindo Diretório
setwd("~/Estudos/Métodos Aplicados/Dados")

#Lendo os bancos
fb <- read.csv("~/Estudos/Métodos Aplicados/Dados/FB.CSV")
head(fb)
tw <- read.csv("~/Estudos/Métodos Aplicados/Dados/TWTR.CSV")
head(tw)

# --------------------------------------------------
# Graficos Iniciais (hist e linha) ----
a <- ggplot(fb, aes(x=High)) + geom_histogram(fill="#00688B",bins=30, color = "black")+
  labs(x="Facebook", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
b <- ggplot(fb, aes(x=as.Date(Date), y=High, group=1)) +
  geom_line(size=1, colour="#00688B") +
  labs(x="Data", y="Valores máximos Facebook") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")

c <- ggplot(tw, aes(x=High)) + geom_histogram(fill="#00688B",bins=30, color = "black")+
  labs(x="Twitter", y="Frequência") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
d <- ggplot(tw, aes(x=as.Date(Date), y=High, group=1)) +
  geom_line(size=1, colour="#00688B") +
  labs(x="Data", y="Valores máximos Twitter") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels = "%Y")

grid.arrange(a, b, c, d)
ggsave("grafinicial.png", arrangeGrob(a, b, c, d), device = "png",
       width = 8)
dev.off()


# --------------------------------------------------
# Retorno Facebook -----
fb$retorno<-log(fb$High/dplyr::lag(fb$High))
head(fb$retorno)

(fb<-fb[is.finite(fb$retorno), ])
(ret<-fb$retorno)

ggplot(fb, aes(x=as.Date(Date), y=retorno, group=1)) +
  geom_line(size=0.4, colour="#00688B") +
  labs(x="Data", y="Valores máximos Facebook") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")

ggplot(fb, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diário Facebook", y="Densidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

media<-mean(ret)
varianc<-var(ret)
dp<-sqrt(varianc)
setNames(c(media,varianc,dp),c("Média "," Variância "," Desvio Padrão"))
rn<-rnorm(10000,media,dp)
summary(ret)

shapiro.test(fb$retorno)

##Estiamacao da estavel (Acrescentar!)
## alpha=forma, beta=assimetria, gamma=escala, delta=locação.
## Ajuste dos parametros metodo os quantis
(st1<-stableFit(ret, type= "q",doplot = TRUE))

##Ajuste dos parametros via MLE
#est<-Estim(EstimMethod="ML",ret)
(st2<-stableFit(ret, "mle",doplot = TRUE))
(par.est<-st2@fit$estimate)
alpha.est<-par.est[1] ; beta.est<-par.est[2]
gamma.est<-par.est[3] ; delta.est<-par.est[4]


ggplot(fb, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diário Facebook", y="Densidade") +
  stat_function(fun = dnorm, args = list(mean = media, sd = dp), 
                                         size = 1, color = "#000000") +
  stat_function(fun = dstable, args = list(alpha=1.5410000000, beta = -0.0240000000, 
                                           gamma =0.0081798711, delta = 0.0007224951), 
                                           size = 1, color = "#e36414") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


##calculo do VAR (Historico e Normal) Acrescentar!
library("PerformanceAnalytics")##pacote uasado

p<-c(0.90,0.95,0.975,0.99,0.999) 
## VaR historico
V.hist<-sapply(p, PerformanceAnalytics::VaR, R=ret, method="historical")
## VaR normal     
V.normal<-sapply(p, PerformanceAnalytics::VaR, R=ret, method="gaussian")
## VaR alpha-estavel
V.alpha<-sapply(p, qstable,alpha= alpha.est,beta= beta.est,gamma= gamma.est,delta= delta.est,pm=0)

## Apresentação dos VaRs obtidos
df_info<-data.frame(p,V.hist,V.normal,V.alpha)
names(df_info)<-c("Nível de Confiança","VaR Histórico","VaR Normal","VaR Alpha-Estável")

kable(df_info) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# --------------------------------------------------
# Retorno Twitter -----
tw$retorno<-log(tw$High/dplyr::lag(tw$High))
head(tw$retorno)

(tw<-tw[is.finite(tw$retorno), ])
(ret<-tw$retorno)

ggplot(tw, aes(x=as.Date(Date), y=retorno, group=1)) +
  geom_line(size=0.4, colour="#00688B") +
  labs(x="Data", y="Valores máximos Retorno Twitter") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")
ggplot(tw, aes(x=retorno)) + geom_histogram(colour="white",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diário Twitter", y="Densidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

media<-mean(ret)
varianc<-var(ret)
dp<-sqrt(varianc)
setNames(c(media,varianc,dp),c("Média "," Variância "," Desvio Padrão"))
rn<-rnorm(10000,media,dp)
summary(ret)

shapiro.test(tw$retorno)

##Estiamacao da estavel (Acrescentar!)
## alpha=forma, beta=assimetria, gamma=escala, delta=locação.
## Ajuste dos parametros metodo os quantis
(st1<-stableFit(ret, "q",doplot = TRUE))

##Ajuste dos parametros via MLE
#est<-Estim(EstimMethod="ML",ret)
(st2<-stableFit(ret, "mle",doplot = TRUE))
(par.est<-st2@fit$estimate)
alpha.est<-par.est[1] ; beta.est<-par.est[2]
gamma.est<-par.est[3] ; delta.est<-par.est[4]

ggplot(tw, aes(x=retorno)) + geom_histogram(colour="white",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diário Twitter", y="Densidade") +
  stat_function(fun = dnorm, args = list(mean = media, sd = dp), 
                size = 1, color = "#000000") +
  stat_function(fun = dstable, args = list(alpha=1.459000000, beta = 0.203000000, 
                                           gamma =0.013428890, delta = -0.001858993), 
                size = 1, color = "#e71d36") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


##calculo do VAR (Historico e Normal) Acrescentar!
library("PerformanceAnalytics")##pacote uasado

p<-c(0.90,0.95,0.975,0.99,0.999) 
## VaR historico
V.hist<-sapply(p, PerformanceAnalytics::VaR, R=ret, method="historical")
## VaR normal     
V.normal<-sapply(p, PerformanceAnalytics::VaR, R=ret, method="gaussian")
## VaR alpha-estavel
V.alpha<-sapply(p, qstable,alpha= alpha.est,beta= beta.est,gamma= gamma.est,delta= delta.est,pm=0)

## Apresentação dos VaRs obtidos
df_info<-data.frame(p,V.hist,V.normal,V.alpha)
names(df_info)<-c("Nível de Confiança","VaR Histórico","VaR Normal","VaR Alpha-Estável")

kable(df_info) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# --------------------------------------------------
# Gráficos Retornos ------
a <- ggplot(fb, aes(x=as.Date(Date), y=retorno, group=1)) +
  geom_line(size=0.4, colour="#00688B") +
  labs(x="Data", y="Valores máximos Retorno Facebook") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")

b <- ggplot(fb, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diário Facebook", y="Densidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

c <- ggplot(tw, aes(x=as.Date(Date), y=retorno, group=1)) +
  geom_line(size=0.4, colour="#00688B") +
  labs(x="Data", y="Valores máximos Retorno Twitter") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")

d <- ggplot(tw, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diário Twitter", y="Densidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

grid.arrange(a, b, c, d)
ggsave("grafinicialret.png", arrangeGrob(a, b, c, d), device = "png",
       width = 8)
dev.off()



a <- ggplot(tw, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diário Twitter", y="Densidade") +
  stat_function(fun = dnorm, args = list(mean = media, sd = dp), 
                size = 1, color = "#000000") +
  stat_function(fun = dstable, args = list(alpha=1.459000000, beta = 0.203000000, 
                                           gamma =0.013428890, delta = -0.001858993), 
                size = 1, color = "#e36414") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 

b <- ggplot(fb, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                                 fill="#00688B",bins =50)+
  labs(x="Log Retorno Diário Facebook", y="Densidade") +
  stat_function(fun = dnorm, args = list(mean = media, sd = dp), 
                size = 1, color = "#000000") +
  stat_function(fun = dstable, args = list(alpha=1.5410000000, beta = -0.0240000000, 
                                           gamma =0.0081798711, delta = 0.0007224951), 
                size = 1, color = "#e36414") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))
grid.arrange(b, a)
ggsave("histalfaret.png", arrangeGrob(b, a), device = "png",
       width = 8)
dev.off()

# --------------------------------------------------
# Bloco Facebook------
ret<-fb$retorno
High <- ret
N<-length(High)
result <- data.frame() 
for (k in 1:10) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
    for (i in 1:tau){   
      m[i]<-max(ret[j:(j+n-1)])    
      j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}
result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")
kable(result) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))



N<-length(ret)  ; n<-2
tau<-floor(N/n)
M<-numeric(tau) ; j<-1

for (i in 1:tau){
  M[i]<-max(ret[j:(j+n-1)])
  j<-j+n }

M<-M[-1]
par(mfrow=c(1,2))
{hist(M, prob=T, col = "#00688B", ylim = c(0,40), ylab = "Densidade")
  lines(density(m))}
plot(M, type="l",main="Some Extremes")
dev.off()

p<-c((1:tau)/(tau+1)) ; ginv<- -log(-log(p))
qqplot(M,ginv,xlab="Quantis empíricos",ylab="Quantis da Gumbel",main="qqplot") ; grid()

#Estimandoh
library(evd)
library(VGAM)
library(fExtremes)
## R MLE - GEV
fitmv = gevFit(M, type ="mle")
fitmv
par(mfrow = c(2, 2))
summary(fitmv)
## R PWM - GEV
fitpwm = gevFit(M, type ="pwm")
fitpwm
par(mfrow = c(2, 4))
summary(fitpwm)
dev.off()

#Gráficos
x<-M
hist(x,prob=T,ylim=c(0,35),main='',cex.axis=1.5, cex.lab=1.5, cex=1.5, font.axis=2,lwd=2, col = "#00688B")
curve(dgev(x, xi = -0.033180969, mu = 0.002676859, beta = 0.012677361),col='black', lwd=2, add=TRUE)
curve(dgev(x, xi = 0.045238260, mu = 0.002420717, beta= 0.010798933),col='#e36414',lwd=2,add=TRUE)
legend('topright',legend=c('MLE','PWM'),col=c('black','#e36414'),lwd=2)

fit <- fevd(m,type="GEV")
fit #positive shape estimate but fairly large standard error
plot(fit) #The fit looks reasonable
ci(fit,type="parameter") #As expected the 95% confidence interval includes negative values
return.level(fit,do.ci=T)
# --------------------------------------------------
# Bloco Twitter------
ret<-tw$retorno
High <- ret
N<-length(High)
result <- data.frame() 
for (k in 1:10) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(ret[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}
result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")
kable(result) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))



N<-length(ret)  ; n<-2
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(ret[j:(j+n-1)])
  j<-j+n }

MI<-MI[-1]
par(mfrow=c(1,2))
{hist(MI, prob=T,col = "#00688B", ylim = c(0,22), ylab = "Densidade")
  lines(density(MI))}
plot(MI, type="l",main="Some Extremes")
dev.off()

p<-c((1:tau)/(tau+1)) ; ginv<- -log(-log(p))
qqplot(MI,ginv,xlab="Quantis empíricos",ylab="Quantis da Gumbel",main="qqplot") ; grid()


#Estimandoh
library(evd)
library(VGAM)
library(fExtremes)
## R MLE - GEV
fitmv = gevFit(MI, type ="mle")
fitmv
par(mfrow = c(2, 2))
summary(fitmv)
## R PWM - GEV
fitpwm = gevFit(MI, type ="pwm")
fitpwm
par(mfrow = c(2, 4))
summary(fitpwm)
dev.off()

#Gráficos
x<-MI
hist(x,prob=T,ylim=c(0,22),main='',cex.axis=1.5, cex.lab=1.5, cex=1.5, font.axis=2,lwd=2, col = "#00688B")
curve(dgev(x, xi = -0.06887585, mu = 0.00344904, beta = 0.02431638),col='black', lwd=2, add=TRUE)
curve(dgev(x, xi =0.028444735, mu = 0.002867182, beta= 0.019107333 ),col='#e36414',lwd=2,add=TRUE)
legend('topright',legend=c('MLE','PWM'),col=c('black','#e36414'),lwd=2)

fit <- fevd(m,type="GEV")
fit #positive shape estimate but fairly large standard error
plot(fit) #The fit looks reasonable
ci(fit,type="parameter") #As expected the 95% confidence interval includes negative values
return.level(fit,do.ci=T)
# --------------------------------------------------
# Trabalho 2 ----
# --------------------------------------------------
# Dispersão e Histograma ----

#M = Facebook
#MI = Twitter

MI <- MI[1:982]
ggplot(mapping = aes(x = M, y = MI)) +
  stat_density2d(binwidth = 25,color = "#00688B") +
  geom_point(size = 1.6, shape = 20) +
  labs(x="Blocos máximos Facebook", y="Blocos máximos Twitter") +
  theme_bw()
ggsave("t2dispdens.png", width = 158, height = 93, units = "mm")


df <- data.frame(x = M, y = MI)
ggplot(df, aes(x, y)) + geom_point() + 
  theme_classic() +
  labs(x="Blocos máximos Facebook", y="Blocos máximos Twitter")

## FDA fornecida
pgev <- function(x, mu, sigma, qsi) {
  if (qsi != 0) {
    return( exp(-(1 + qsi*(x-mu)/sigma)^(-1/qsi)) )
  } else {
    return( exp(-exp(- (x-mu)/sigma)) )
  }
}

## Inversa da FDA (Quantil)
qgev <- function(q, mu, sigma, qsi) {
  if (qsi != 0) {
    return(mu - (sigma/qsi) * ( 1 - (-log(q))^(-qsi)) )
  } else {
    return(mu - sigma*log(-log(q))  )
  }
}

## Derivada da FDA (Densidade)
dgev <- function(x, mu, sigma, qsi) {
  if (qsi != 0) {
    return ( exp(-(1 + qsi*(x-mu)/sigma)^(-1/qsi)) *
               (1/qsi*(1 + qsi*(x-mu)/sigma)^(-1/qsi-1) ) * qsi/sigma )
  } else {
    return ( exp(-exp(- (x-mu)/sigma)) * (1/sigma)*(exp(- (x-mu)/sigma)) )
  }
}


# Gerar uma amostra de tamanho 100 de uma v.a. GEV
# Grafico Facebook M
qsi1 =-0.03307
sigma1=0.01268
mu1 = 0.00266

set.seed(123)
yM <- runif (100 , min = 0 , max =1)
amostraM <- qgev(yM, mu1, sigma1, qsi1)

par(mfrow = c(1,2))
hist(amostraM , probability = T , border = " black ",
         col = "#00688B", main ="", xlab =" Amostra Facebook", ylab = " Densidade ")
curve(dgev(x,qsi =-0.03307,sigma=0.01268,mu = 0.00266), add = T , col = "black")

#Gráfico Twitter MI
qsi2 =-0.06887
sigma2= 0.02432
mu2 =  0.00345

set.seed(123)
yMI <- runif (100 , min = 0 , max =1)
amostraMI <- qgev(yMI, mu2, sigma2, qsi2)

hist(amostraMI , probability = T , border = "black",
     col = "#00688B", main ="", xlab =" Amostra Twitter ", ylab = " Densidade ") 
curve(dgev(x,qsi =-0.06887,sigma= 0.02432,mu =  0.00345), add = T , col = "black")
dev.off()

#Gráfico Dispersão e histograma
plot1 <- ggplot(df, aes(x, y)) + geom_point() + 
  theme_classic() +
  labs(x="Blocos máximos Facebook", y="Blocos máximos Twitter")

am <- data.frame(amostraM,i =rep("i",100))
dens1 <- ggplot(am, aes(x=amostraM)) + 
  geom_histogram(colour="white", fill="#00688B",bins=12)+
  labs(x="", y="") +
  theme_bw()

ami <- data.frame(amostraMI,i=rep("i",100))
dens2 <- ggplot(ami, aes(x=amostraMI)) + 
  geom_histogram(colour="white", fill="#00688B",bins=12)+
  labs(x="", y="") +
  theme_bw() +
  coord_flip()

dens1 + plot_spacer() + plot1 + dens2 + 
  plot_layout(
    ncol = 2, 
    nrow = 2, 
    widths = c(4, 1),
    heights = c(1, 4)
  ) 
ggsave("t2disphist.png", width = 158, height = 93, units = "mm")

# --------------------------------------------------
# Valor inicial ----

#Correlação
qsi1 =-0.03307
sigma1=0.01268
mu1 = 0.00266
qsi2 =-0.06887
sigma2= 0.02432
mu2 =  0.00345

#Estimativas
a.0<-sin(cor(M,MI, method = "kendal")*pi/2) #Correlação de kendal
a.0
start<- c(a.0)
udat<- cbind(pgev(M, qsi =-0.03307, sigma=0.01268, mu = 0.00266),
             pgev(MI,qsi =-0.06887, sigma=0.02432, mu = 0.00345))


myCop.clayton<- claytonCopula(dim=2)
myCop.gumbel<-gumbelCopula(dim=2)
myCop.frank<- frankCopula(dim=2)


fit.if1 <-fitCopula(myCop.clayton,udat, start=a.0)
fit.if1
fit.if2 <-fitCopula(myCop.gumbel,udat, start=a.0)
fit.if2
fit.if3 <-fitCopula(myCop.frank,udat, start=a.0)
fit.if3

library(copula)
cc<- claytonCopula(0.3275587)
sample1<- rCopula(10000, cc)
gu<- gumbelCopula(1.057)
sample2<- rCopula(10000,gu)
fr<- frankCopula(1.305)
sample3<- rCopula(10000,fr)

#Aic e BIC
aicCalyton <- -2*fit.if1@loglik+2
bicCalyton <- -2*fit.if1@loglik+log(10000)

aicGumbel <- -2*fit.if2@loglik+2
bicGumbel <- -2*fit.if2@loglik+log(10000)

aicFrank <- -2*fit.if3@loglik+2
bicFrank <- -2*fit.if3@loglik+log(10000)

#Adequação do ajuste
aicCalyton
bicCalyton
aicFrank
bicFrank
aicGumbel
bicGumbel

# --------------------------------------------------
# Escolhemos aquela cópula com menor AIC ----

n<- 1000
fr<- frankCopula(1.305)
sample<- rCopula(1000,fr)
plot(sample, xlab="U", ylab="V", pch = ".", cex = 1.5)

qsi1 =-0.03307
sigma1=0.01268
mu1 = 0.00266
qsi2 =-0.06887
sigma2= 0.02432
mu2 =  0.00345

x<- numeric(n)
y<- numeric(n)

for (i in 1:n ){
  x[i] <- mu1 + sigma1*( ((-log(sample[i,1]))^(-qsi1) - 1 )/ qsi1 )
  y[i] <- mu2 + sigma2*( ((-log(sample[i,2]))^(-qsi2) - 1 )/ qsi2 )
}

par(mfrow=c(1,2))
plot(sample, xlab="U", ylab="V", pch = ". ", cex = 3,col="#00688B", main="Cópula Frank")
plot(x, y, ylab="Y", pch = ". ", cex = 3, col="black", main="Frank marginal GEV " )
dev.off()

ggplot(mapping = aes(x = x, y = y)) +
  stat_density2d() + geom_point()

plot(M, MI, pch = ". ", cex = 5 , col="#00688B", xlab=" " , ylab=" " )
points(x, y, ylab="Y", pch = ". ", cex = 5, col="black")
legend("topright", legend=c("Dados simulados", "Dados reais"), col=c("black","#00688B"), pch = 15)

ggplot() +
  stat_density2d(mapping = aes(x = x, y = y), color = "black") +
  stat_density2d(mapping = aes(x = M, y = MI), color = "blue") +
  theme_classic() +
  labs(x="", y="")
ggsave("t2dens.png", width = 158, height = 93, units = "mm")

tau(fr)
rho(fr)
lambda(fr)

# --------------------------------------------------
# VAR ----
# VAR BIvARIADO tipo Expected Shortfall condicional 
#  VAR NO OCTANTE INFERIOR

#Primeira componente do VaR bivariado com cópula e marginais GEV
#(Se X~F_1=GEV_1-> primeira componente do VaR)

alpha= 0.95# alpha%
theta= 1.305 # valor estimado do parámetro da cópula
#mu # paraetro de locação da marginal GEV
#sigma # parametro de escala da marginal GEV
#qsi parametro de forma da marginal GEV


#Gerar amostra de tamanho n=50 de uma va S~Beta(1,1)=U[0,1]

S<-runif(50, min=0, max=1)
S
# Para cópula Frank 
# Definir o gerador da copula vphi(alpha)=vphi -> S*vphi(alpha)=S*vphi=t
vphi<- (-log(( exp(-theta*alpha)-1)/(exp(-theta)-1 )))
t<- S*vphi
t
#Definir a inversa de vphi(t) -> vphi^{-1}(S*vphi(alpha))=inv(t)=inv

inv<- (-theta^(-1))*log(1+exp(-t)*(exp(-theta) -1 ) )
inv

#Definir quantile F^(-1) da GEV -> F^{-1}(vphi^{-1}(S*vphi(alpha)))=F^{-1}(inv)=qgev
qev1<- mu1-(sigma1/qsi1)*(1-(-log(inv)))^(-qsi1)
qev1
qev2<- mu2-(sigma2/qsi2)*(1-(-log(inv)))^(-qsi2)
qev2

#Media E(F^{-1}(vphi^{-1}(S*vphi(alpha))))=VaR_1 componnete 1
(VAR <- c(mean(qev1), mean(qev2)))

#97,5
alpha= 0.975
theta= 1.305 
S<-runif(50, min=0, max=1)
vphi<- (-log(( exp(-theta*alpha)-1)/(exp(-theta)-1 )))
t<- S*vphi
inv<- (-theta^(-1))*log(1+exp(-t)*(exp(-theta) -1 ) )
qev1<- mu1-(sigma1/qsi1)*(1-(-log(inv)))^(-qsi1)
qev2<- mu2-(sigma2/qsi2)*(1-(-log(inv)))^(-qsi2)
(VAR <- c(mean(qev1), mean(qev2)))

#99
alpha= 0.99
theta= 1.305 
S<-runif(50, min=0, max=1)
vphi<- (-log(( exp(-theta*alpha)-1)/(exp(-theta)-1 )))
t<- S*vphi
inv<- (-theta^(-1))*log(1+exp(-t)*(exp(-theta) -1 ) )
qev1<- mu1-(sigma1/qsi1)*(1-(-log(inv)))^(-qsi1)
qev2<- mu2-(sigma2/qsi2)*(1-(-log(inv)))^(-qsi2)
(VAR <- c(mean(qev1), mean(qev2)))


# --------------------------------------------------
#### EXTRA ####
# Cópulas Arquimedianas ----

#Cópula de Clayton
cc <- claytonCopula(2)
sample <- rCopula(10000,cc)
#Scatterplot
plot(sample, xlab="U", ylab="V", pch = ".", cex = 1.5)
#contour
contour(cc, dCopula)
#Density
persp(cc,dCopula, xlab="u", ylab="v", zlab="c(u,v)")

#Cópula de Frank
fr <- frankCopula(5)
sample <- rCopula(10000,fr)
#Scatterplot
plot(sample, xlab="U", ylab="V", pch = ".", cex = 1.5)
#contour
contour(fr, dCopula)
#Density
persp(fr,dCopula, xlab="u", ylab="v", zlab="C(u,v)", shade=.0001)

#Cópula de Gumbel
gu <- gumbelCopula(4)
sample <- rCopula(10000,gu)
#Scatterplot
plot(sample, xlab="U", ylab="V", pch = ".", cex = 1.5)
#contour
contour(gu, dCopula)
#Density
persp(gu,dCopula, xlab="u", ylab="v", zlab="C(u,v)")
