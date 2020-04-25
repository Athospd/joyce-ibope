#########################################################################################
# Análise de dados categorizados - Lista 4                                              #
#                                                                                       #
# Código da reprodução do artigo citado no enunciado da lista                           #
#                                                                                       #
# Athos Damiani                                                                         #
# Daniel de Paula                                                                       #
# Fernanda Kmohan                                                                       #
# Leonardo de Faria                                                                     #
# Julio Trecenti                                                                        #
#########################################################################################

#carregando pacotes utilizados
require(ggplot2)
require(reshape)
require(stringr)
require(Hmisc)
require(nlme)
require(lme4)
require(Matrix)


#carregando dados
#OBS: o arquivo Singer&Curi2006.xls precisa ser convertido para .csv.
dados <- read.csv2("Singer&Curi2006.csv")
dados$Baseline.Salinity.Group  <- str_replace_all(dados$Baseline.Salinity.Group, ",","\\.")
dados$Baseline.Salinity.Group  <- as.numeric(str_replace_all(dados$Baseline.Salinity.Group, "%",""))

# Reformatando o banco de dados para o formato longo, no qual cada linha corresponde a um animal-condição
dados.l.com.NA <- melt(dados, measure.vars = 3:12, variable_name = "Teste")
dados.l.com.NA$grupo <- factor(str_extract(dados.l.com.NA$Teste,"[A-z]+"))
dados.l.com.NA$Teste  <- as.numeric(str_extract(dados.l.com.NA$Teste,"0.7|1.4|1.9|2.4|3.4"))

# Criando a variável "dif" que é a diferença entre a salinidade do teste e a salinidade de controle
dados.l.com.NA$dif  <- with(dados.l.com.NA, Teste - Baseline.Salinity.Group)

#Planilha de dados sem os dados omissos
dados.l <- data.frame(subset(dados.l.com.NA, !is.na(value)))

#########################################################################################
#########################################################################################
# Seçao 3.1
#########################################################################################
#########################################################################################

#Figura 1 ###############################################################################
# Gráfico de perfis individuais agrupados por salinidade basal para ECAP resposta #######

# Baseline 1,9%
ggplot(subset(dados.l, Baseline.Salinity.Group == 1.9 & grupo == "Response"), aes(x = Teste, y = value, group = Animal)) + 
  geom_line() +
  geom_point(size=2.5, pch=4) +
  stat_summary(aes(group=1), fun.y=mean, geom="line", col=I("black"), size=1.5) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", col=I("black"), size=4) +
  labs(y = "ECAP resposta (mV)", x = "Salinidade de teste (%)") +
  opts(panel.background=theme_blank())

# Baseline 2,4%
ggplot(subset(dados.l, Baseline.Salinity.Group == 2.4 & grupo == "Response"), aes(x = Teste, y = value, group = Animal)) + 
  geom_line() +
  geom_point(size=2.5, pch=4) +
  stat_summary(aes(group=1), fun.y=mean, geom="line", col=I("black"), size=1.5) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", col=I("black"), size=4) +
  labs(y = "ECAP resposta (mV)", x = "Salinidade de teste (%)") +
  opts(panel.background=theme_blank())

# Baseline 3,4%
ggplot(subset(dados.l, Baseline.Salinity.Group == 3.4 & grupo == "Response"), aes(x = Teste, y = value, group = Animal)) + 
  geom_line() +
  geom_point(size=2.5, pch=4) +
  stat_summary(aes(group=1), fun.y=mean, geom="line", col=I("black"), size=1.5) +
  stat_summary(aes(group=1), fun.y=mean, geom="point", col=I("black"), size=4) +
  labs(y = "ECAP resposta (mV)", x = "Salinidade de teste (%)") +
  opts(panel.background=theme_blank())
#########################################################################################

#Figura 2 ###############################################################################
# Gráfico de perfis médios por diferença de sanilidade ##################################

# Médias de ECAP resposta
dados.l[dados.l$dif==0,"grupo"] <- "Response"
ggplot(subset(dados.l, grupo == "Response"), aes(x = dif, y = value)) +
  stat_summary(aes(group=factor(Baseline.Salinity.Group), linetype = factor(Baseline.Salinity.Group)), fun.y=mean, geom="line", size = 1) +
  stat_summary(aes(group=factor(Baseline.Salinity.Group)), fun.y=mean, geom="point",pch=15) +
  labs(y = "Médias de ECAP resposta (mV)", x = "diferença de salinidade (%)", linetype = "Grupo") +
  scale_linetype_discrete(labels = c('1,9%','2,4%','3,4%')) +
  opts(panel.background=theme_blank())

#Figura 3 ###############################################################################
# Médias de ECAP controle
dados.l[dados.l$dif==0,"grupo"] <- "Control"
ggplot(subset(dados.l, grupo == "Control"), aes(x = dif, y = value)) +
  stat_summary(aes(group=factor(Baseline.Salinity.Group), linetype = factor(Baseline.Salinity.Group)), fun.y=mean, geom="line", size = 1) +
  stat_summary(aes(group=factor(Baseline.Salinity.Group)), fun.y=mean, geom="point",pch=15) +
  labs(y = "Médias de ECAP resposta (mV)", x = "diferença de salinidade (%)", linetype = "Grupo") +
  scale_linetype_discrete(labels = c('1,9%','2,4%','3,4%')) +
  opts(panel.background=theme_blank())

#########################################################################################

#########################################################################################
#########################################################################################
# Seçao 3.2
#########################################################################################
#########################################################################################

#Figura 4 ###############################################################################
# Gráfico de proporção de dados omissos por diferença de sanilidade #####################
ggplot(subset(dados.l.com.NA, grupo == "Response"), aes(x = dif, y = ifelse(is.na(value),1,0))) +
  stat_summary(aes(group=factor(Baseline.Salinity.Group), linetype = factor(Baseline.Salinity.Group)), fun.y=mean, geom="line", size = 1) +
  stat_summary(aes(group=factor(Baseline.Salinity.Group)), fun.y=mean, geom="point", size=3,pch=15) +
  labs(y = "% de valores omissos", x = "diferença de salinidade (%)", linetype = "Grupo") +
  scale_linetype_discrete(labels = c('1,9%','2,4%','3,4%')) +
  opts(panel.background=theme_blank())
#########################################################################################

# Regressão linear relacionando ECAP resposta e controle ################################

# Estruturando uma planilha que contenha colunas de valores observados correspondentes à 
# resposta (Response) e ao controle (Control). Esses valores serão a variável dependente
# e a covariável que "varia no tempo", respectivamente.
dados.l.cast <- cast(dados.l,  Animal + Baseline.Salinity.Group + dif + Teste ~ grupo, fun.aggregate=mean, fill=NA)

# Aqui faz valer a suposição de que o valor da covariável "Control" quando da coleta na 
# salinidade basal é igual ao valor observado da "Response".
dados.l.cast[is.na(dados.l.cast$Control),"Control"] <- dados.l.cast[is.na(dados.l.cast$Control),"Response"]

#BSG.dif.categ são as categorias compostas pelas combinações de níveis das variáveis Baseline.Salinity.Group e dif.
dados.l.cast$BSG.dif.categ <- with(dados.l.cast,factor(paste(Baseline.Salinity.Group,dif)))

# Ancova Para Tudo
aux.data <- dados.l.cast[!is.na(dados.l.cast$Control),]
aux.data$base.teste <- factor(paste(aux.data$Baseline.Salinity.Group,aux.data$Teste))

ancova.geral <- lm(Response ~ factor(Baseline.Salinity.Group):factor(Teste) + factor(Baseline.Salinity.Group):factor(Teste):Control - 1, data=aux.data)
round(ancova.geral$coef,2)

#Os 'residuos.ancova' serão utilizados para fazer a tabela 2
aux.data$residuo.ancova[as.numeric(attr(ancova.geral$res,"names"))] <- ancova.geral$res

slope <- ancova.geral$coef[(1:15)+15]
EP <- c()
EP[!is.na(slope)] <- summary(ancova.geral)$coef[(1:12)+12,2]
slope <- round(matrix(slope,byrow=TRUE,5,3),2)
EP <- round(matrix(EP,byrow=TRUE,5,3),2)

# Tabela 1 (com os coeficientes angulares +/- EP das regressoes lineares acima) #########

tabela1 <- matrix(NA,5,3)
rownames(tabela1) <- paste(levels(factor(dados.l$Teste)),"%",sep="")
colnames(tabela1) <- paste(levels(factor(dados.l$Baseline.Salinity.Group)),"%",sep="")

for(i in 1:5){
  for(j in 1:3){
    tabela1[i,j] <- paste(paste(format(slope[i,j],digits=2),format(EP[i,j],digits=2),sep="-+"))
  }
}
tabela1[c(3,9,15)] <- "--"

#########################################################################################

#########################################################################################
#########################################################################################
# Seçao 3.3
#########################################################################################
#########################################################################################

# Tabela 2 (var-cov dos resíduos da ANCOVA) #############################################

# residuos é uma lista de tamanho 3, uma para cada grupo de salinidade basal. Cada uma
# dessas 3 entradas contém uma tabela com 4 colunas, uma para cada possível valor da 
# diferença de salinidade (a diferença zero não entra), na qual cada uma dessas colunas
# guarda os resíduos da regressão linear entre o ECAP resposta e controle dentro do
# subconjunto definido pelo nível de grupo de salinidade basal e diferença de salinidade.

res <- with(aux.data, tapply(residuo.ancova, list(Baseline.Salinity.Group, dif), function(x) x))

res <- cast(aux.data,  Animal ~ Baseline.Salinity.Group + dif, value = 'residuo.ancova', fun.aggregate=mean, fill=NA)

tabela2 <- list()
tabela2.cov <- list()
tabela2.cor <- list()
for(i in 1:3){
  tabela2.cov[[i]] <- cov(res[,2:6+(i-1)*5],use="p")
  tabela2.cor[[i]] <- cor(res[,2:6+(i-1)*5],use="p")
}
tabela2.cov <- lapply(tabela2.cov,function(x){matrix(x[!is.na(x)],4)})
tabela2.cor <- lapply(tabela2.cor,function(x){matrix(x[!is.na(x)],4)})

for(i in 1:3){
  tabela2[[i]] <- as.matrix(band(matrix(1,4,4),0,3)*tabela2.cov[[i]] + band(matrix(1,4,4),-3,-1)*tabela2.cor[[i]])
}
names(tabela2) <- c("1.9%","2.4%","3.4%")
tabela2

#########################################################################################

#########################################################################################
#########################################################################################
# Seçao 4.1
#########################################################################################
#########################################################################################

# Ajustando o modelo para a matriz de covariância #######################################
#Retirando observações omissas (no par "controle-resposta" simultaneamente)
dados.l.cast <- dados.l.cast[!(is.na(dados.l.cast$Response) & is.na(dados.l.cast$Control)),]

#Criando as duas variáveis auxiliares utilizadas na modelagem.
#'dif_neg' é o min(diferença_de_salinidade, 0). No artigo, é o delta-.
dados.l.cast$dif_neg <- pmin(dados.l.cast$dif,0)

#'dif_pos' é o max(diferença_de_salinidade, 0). No artigo, é o delta+
dados.l.cast$dif_pos <- pmax(dados.l.cast$dif,0)

#Apenas transformando 'Baseline.Salinity.Group' de numérico para fator.
dados.l.cast$Baseline.Salinity.Group <- factor(dados.l.cast$Baseline.Salinity.Group)

dados.l.cast[is.na(dados.l.cast$Control),"Control"] <- dados.l.cast[is.na(dados.l.cast$Control),"Response"]
dados.l.cast[is.na(dados.l.cast$Response),"Response"] <- dados.l.cast[is.na(dados.l.cast$Response),"Control"]

#n_j, j=1,2,3. quantidade de animais dos grupos 1.9, 2.4 e 3.4, respectivamente.
n <- with(dados.l.cast, tapply(Animal,list(Baseline.Salinity.Group),function(x) length(unique(x))))

#t_ij, j=1,2,3, i = 1,...,n_j. t[[j]][i] = quantidade de salinidades testadas no animal i do grupo j.
t <- with(dados.l.cast, 
          apply(tapply(Response, list(Animal,Baseline.Salinity.Group), length), 
                2, function(x) x[!is.na(x)]))

#Cbarra_j, j=1,2,3. Medida média do controle.
Cbarra <- with(dados.l.cast,
               tapply(Control,Baseline.Salinity.Group,mean))

#W_ij = C_ij - Cbarra_j
dados.l.cast$W <- with(dados.l.cast, Control - Cbarra[Baseline.Salinity.Group])


# Jeito pedestre: Constrói-se a matriz Z de planejamento 
Z <- model.matrix(~Baseline.Salinity.Group - 1, data=dados.l.cast)
Z <- lapply(dados.l.cast[,c("dif_pos","dif_neg")],function(x) x*Z)
Z <- data.frame(Z$dif_pos,Z$dif_neg)
Z <- Z[,-3]
names(Z)[1:2] <- paste("BSG",c(1.9,2.4),"pos",sep=".")
names(Z)[3:5] <- paste("BSG",c(1.9,2.4,3.4),"neg",sep=".")
Z <- Z[,c(3,1,4,2,5)]
dados.l.cast <- data.frame(dados.l.cast,Z)

#Modelo de efeitos aleatórios (4.2)-(4.5)
modelo.EA <- lmer(Response ~ BSG.dif.categ +  BSG.dif.categ:W + 
  (BSG.1.9.neg - 1|Animal) +
  (BSG.1.9.pos - 1|Animal) +
  (BSG.2.4.neg - 1|Animal) +
  (BSG.2.4.pos - 1|Animal) +
  (BSG.3.4.neg - 1|Animal) - 1, data = dados.l.cast)

#Extraindo as variâncias dos efeitos aleatórios e o sigma2 estimados por REML pelo modelo EA
var.cov <- t(t(as.numeric(summary(modelo.EA)@REmat[,3])))
colnames(var.cov) <- "estimativas"
rownames(var.cov) <- c(summary(modelo.EA)@REmat[-6,2],"sigma2")

#########################################################################################

# Tabela 3 (matrizes de var-cov ajustadas) ##############################################
R <- diag(5)*var.cov[6,]
Sigmai <- list()
# Grupo 1.9%
Z1 <- rbind(c(-1.2, 0.0),
            c(-0.5, 0.0),
            c( 0.0, 0.0),
            c( 0.0, 0.5),
            c( 0.0, 1.5))
G1 <- diag(var.cov[1:2,])
Sigmai[[1]] <- Z1%*%G1%*%t(Z1) + R

# Grupo 2.4%
Z2 <- rbind(c(-1.7, 0.0),
            c(-1.0, 0.0),
            c(-0.5, 0.0),
            c( 0.0, 0.0),
            c( 0.0, 1.0))
G2 <- diag(var.cov[3:4,])
Sigmai[[2]] <- Z2%*%G2%*%t(Z2) + R

# Grupo 3.4%
Z3 <- rbind(c(-2.7),
            c(-2.0),
            c(-1.5),
            c(-1.0),
            c( 0.0))
G3 <- var.cov[5,]
Sigmai[[3]] <- Z3%*%G3%*%t(Z3) + R

(tabela3 <- lapply(Sigmai,round,2))
#########################################################################################

# Modelo heterocedástico independente (4.6)
# A variável 'dif.ordinal.categ' denota a ordem de diferença de salinidade relativa a cada grupo
# de salinidade basal. Por exemplo, a menor diferença (a mais negativa) de cada grupo recebe o 
#rótulo '1'. A segunda menor diferença recebe o rótulo '2' e assim por diante. 
dados.l.cast$dif.ordinal.categ <- dados.l.cast$BSG.dif.categ
levels(dados.l.cast$dif.ordinal.categ) <- c(c(2,1,3,4,5),c(3,2,1,4,5),c(4,3,2,1,5))

modelo.HI <- gls(Response ~ BSG.dif.categ + BSG.dif.categ:W - 1, 
                 data=dados.l.cast, 
                 weights = varIdent(form= ~1|dif.ordinal.categ))
modelo.HI

# Modelo homocedástico com estrutura de correlação espacial (4.7)
# ERRADO -------------------------------------------------------------------------#
modelo.CE <- lme(Response ~ BSG.dif.categ + BSG.dif.categ:W - 1, 
                 data=dados.l.cast, 
                 random = ~ 1|Animal,
                 correlation = corExp(form = ~ Teste + Baseline.Salinity.Group|Animal, metric="manhattan"))

# Tabela 4 (Comparação entre matrizes de covariâncias de três modelos) ##################

tabela4 <- matrix(NA,3,6)
rownames(tabela4) <- c("Efeitos aleatórios","Indep. e Heterocedástico", "Correlação exponencial")
colnames(tabela4) <- c("Num param","AIC","1.9%","2.4%","3.4%","tot")
tabela4[,"Num param"] <- c(6,5,6)

# Diferença absoluta média entre os valores preditos e as covariâncias dos residuos da ANCOVA
# Modelo de efeitos aleatórios
for(i in 1:3){
  modelado <- Sigmai[[i]][-(i+2),][,-(i+2)]
  aux <- ceiling(modelado/max(modelado))
  amostral <- aux*tabela2.cov[[i]]
  tabela4["Efeitos aleatórios",i+2] <- sum(abs(amostral - modelado))/sum(aux)
}

#********************** tabela 4 incompleta!!!


#########################################################################################
#########################################################################################
# Seçao 4.2
#########################################################################################
#########################################################################################

# Ajustando modelos para parâmetros de localidade #######################################

# Ajuste do modelo 4.8
modelo.4.8 <- lmer(Response ~ BSG.dif.categ + W + dif_neg:W:factor(Baseline.Salinity.Group) +
  (BSG.1.9.neg - 1|Animal) +
  (BSG.1.9.pos - 1|Animal) +
  (BSG.2.4.neg - 1|Animal) +
  (BSG.2.4.pos - 1|Animal) +
  (BSG.3.4.neg - 1|Animal) - 1, data = dados.l.cast)


# Ajuste do modelo 4.9 (gerador da tabela 5)
modelo.4.9 <- lmer(Response ~  W + factor(Baseline.Salinity.Group) + (I(dif) + I(dif^2) + I(dif^3) + dif_neg:W):factor(Baseline.Salinity.Group) + 
  (BSG.1.9.neg - 1|Animal) +
  (BSG.1.9.pos - 1|Animal) +
  (BSG.2.4.neg - 1|Animal) +
  (BSG.2.4.pos - 1|Animal) +
  (BSG.3.4.neg - 1|Animal) - 1, data = dados.l.cast)

# Tabela 5
sumario.modelo.4.9 <- summary(modelo.4.9)@coefs
sumario.modelo.4.9[,3] <- round(pf((sumario.modelo.4.9[,3])^2,1,222-16, lower.tail=FALSE),4)
sumario.modelo.4.9[,-3] <- round(sumario.modelo.4.9[,-3],2)
sumario.modelo.4.9 <- sumario.modelo.4.9[c(3*(0:3)+2,3*(0:3)+3,3*(0:3)+4,14:16,1),]

summary(modelo.4.9)@REmat

#########################################################################################
# Ajuste do modelo 4.10 (gerador da tabela 6)
#Montando a matriz de planejamento X do modelo 4.10 (retirando a coluna de zeros referente ao 'psi+' do grupo 3.4)
X.4.10 <- model.matrix(Response ~  W + factor(Baseline.Salinity.Group) + (dif_neg + dif_pos + dif_neg:W):factor(Baseline.Salinity.Group)-1, data=dados.l.cast)[,-10]
colnames(X.4.10) <- c("W","BSG1.9","BSG2.4","BSG3.4","BSG1.9.neg","BSG2.4.neg","BSG3.4.neg","BSG1.9.pos","BSG2.4.pos","BSG1.9.neg.W","BSG2.4.neg.W","BSG3.4.neg.W")
dados.l.cast <- data.frame(dados.l.cast,X.4.10)
modelo.4.10 <- lmer(Response ~ 
  BSG1.9.neg +
  BSG1.9 +
  BSG1.9.pos +
  BSG2.4.neg +
  BSG2.4 +
  BSG2.4.pos +
  BSG3.4.neg +
  BSG3.4 +
  BSG1.9.neg.W +
  BSG2.4.neg.W +
  BSG3.4.neg.W +
  W +
  (BSG.1.9.neg - 1|Animal) +
  (BSG.1.9.pos - 1|Animal) +
  (BSG.2.4.neg - 1|Animal) +
  (BSG.2.4.pos - 1|Animal) +
  (BSG.3.4.neg - 1|Animal) - 1, data = dados.l.cast)

#tabela 6
sumario.modelo.4.10 <- summary(modelo.4.10)@coefs
sumario.modelo.4.10[,3] <- round(pf((sumario.modelo.4.10[,3])^2,1,222-141, lower.tail=FALSE),4)
sumario.modelo.4.10[,-3] <- round(sumario.modelo.4.10[,-3],2)

summary(modelo.4.10)@REmat

#########################################################################################
#Figura 5 - Resíduos estandardizados do modelo 4.9 vs valores preditos de ECAP
#Extraindo as variâncias dos efeitos aleatórios e o sigma2 estimados por REML pelo modelo 4.9
var.cov.4.9 <- t(t(as.numeric(summary(modelo.4.9)@REmat[,3])))
colnames(var.cov.4.9) <- "estimativas"
rownames(var.cov.4.9) <- c(summary(modelo.4.9)@REmat[-6,2],"sigma2")

X <- model.matrix(modelo.4.9)

#Resíduo condicional estandardizado 
e.hat.st <- modelo.4.9@resid/(sqrt(var.cov.4.9[6])*sqrt(1-diag(X%*%solve(t(X)%*%(X))%*%t(X))))

#Plot da figura 5
par(mar=c(4,4,.1,.1))
plot(fitted(modelo.4.9),e.hat.st,ylab='Resíduos estandardizados (mV)', xlab='valores preditos para o ECAP (mV)',las=1,xlim=c(0,20))
abline(h=c(-2,0,2),lty=c(2,1,2))

#########################################################################################
#Figura 6 - Resíduos estandardizados do modelo 4.10 vs valores preditos de ECAP
#Extraindo as variâncias dos efeitos aleatórios e o sigma2 estimados por REML pelo modelo 4.10
var.cov.4.10 <- t(t(as.numeric(summary(modelo.4.10)@REmat[,3])))
colnames(var.cov.4.10) <- "estimativas"
rownames(var.cov.4.10) <- c(summary(modelo.4.10)@REmat[-6,2],"sigma2")

X <- model.matrix(modelo.4.10)

#Resíduo condicional estandardizado 
e.hat.st <- modelo.4.10@resid/(sqrt(var.cov.4.10[6])*sqrt(1-diag(X%*%solve(t(X)%*%(X))%*%t(X))))

#Plot da figura 5
par(mar=c(4,4,.1,.1))
plot(fitted(modelo.4.10),e.hat.st,ylab='Resíduos estandardizados (mV)', xlab='valores preditos para o ECAP (mV)',las=1,xlim=c(0,20))
abline(h=c(-2,0,2),lty=c(2,1,2))

#########################################################################################
#Figura 7 - Curvas ajustadas pelos modelos 4.9 e 4.10 para o ECAP resposta médio
mod.EA.coef <- data.frame(coef = fixef(modelo.EA)[1:15], 
                          dif = with(dados.l.cast, tapply(dif,BSG.dif.categ,mean)),
                          Baseline.Salinity.Group = levels(dados.l.cast$Baseline.Salinity.Group)[rep(1:3,each=5)])

#funções das curvas ajustadas pelo modelo 4.9
mod.4.9.coef <- fixef(modelo.4.9)
#BSG = 1 para grupo 1.9
#BSG = 2 para grupo 2.4
#BSG = 3 para grupo 3.4
mod.4.9.fun <- function(x,BSG,xmin,xmax){
  ifelse(x > xmax | x < xmin, NA, 
         mod.4.9.coef[1+BSG] + mod.4.9.coef[4+BSG]*x + mod.4.9.coef[7+BSG]*x^2 + mod.4.9.coef[10+BSG]*x^3)
}

#funções das curvas ajustadas pelo modelo 4.10
mod.4.10.coef <- fixef(modelo.4.10)
#BSG = 1 para grupo 1.9
#BSG = 2 para grupo 2.4
#BSG = 3 para grupo 3.4
mod.4.10.fun <- function(x,BSG,xmin,xmax){
  #Função indicadora
  ifelse(x > xmax | x < xmin, NA, 
        mod.4.10.coef[2+3*(BSG-1)] + ifelse(x < 0, mod.4.10.coef[1+3*(BSG-1)], mod.4.10.coef[3+3*(BSG-1)])*x)
}

# Médias de ECAP resposta
ggplot(dados.l.cast, aes(x = dif, y = Response, linetype=Baseline.Salinity.Group)) +
  stat_function(fun = mod.4.10.fun, args=c(1,-1.2,1.5), geom="line", n=1000, aes(linetype="1.9%")) +
  stat_function(fun = mod.4.10.fun, args=c(2,-1.7,1.0), geom="line", n=1000, aes(linetype="2.4%")) +
  stat_function(fun = mod.4.10.fun, args=c(3,-2.7,0.0), geom="line", n=1000, aes(linetype="3.4%")) +
  scale_linetype_manual("Modelo 4.10",values=c("1.9%"=2,"2.4%"=5,"3.4%"=4),breaks=c("1.9%","2.4%","3.4%")) + 
  stat_function(fun = mod.4.9.fun, args=c(1,-1.2,1.5), geom="line", lty=1, aes(size="1.9%b")) +
  stat_function(fun = mod.4.9.fun, args=c(2,-1.7,1.0), geom="line", lty=1, aes(size="2.4%b")) +
  stat_function(fun = mod.4.9.fun, args=c(3,-2.7,0.0), geom="line", lty=1, aes(size="3.4%b")) +
  scale_size_manual("Modelo 4.9",values=c("1.9%b"=.5,"2.4%b"=1,"3.4%b"=1.5),breaks=c("1.9%b","2.4%b","3.4%b"), 
                    labels=c("1.9%","2.4%","3.4%")) +
  geom_point(data=mod.EA.coef,aes(x=dif,y=coef, shape=Baseline.Salinity.Group),size=4, fill="white") +
  scale_shape_manual("Modelo 4.2",values=c(22,21,24),labels=c("1.9%","2.4%","3.4%")) +
  labs(y = "Médias de ECAP resposta (mV)", x = "diferença de salinidade (%)", linetype = "Grupo") +
  ylim(0,13) +
  opts(panel.background=theme_blank(), 
       axis.text.x= theme_text(size=13), 
       axis.text.y= theme_text(size=13),
       axis.title.x=theme_text(size=17),
       axis.title.y=theme_text(size=17,angle=90),
       legend.text = theme_text(size=15),
       legend.title = theme_text(size=15, face='bold'),
       legend.position = c(.7,.3),
       legend.box = "horizontal")




#OBSERVAÇÃO: As tarefas que não estão feitas são:
#Ajuste do modelo (4.7) com modelo para a matriz de covariãncias do tipo espacial.
#Os três testes de Wald feitos no artigo.