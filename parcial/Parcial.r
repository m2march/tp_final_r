# Parcial: Ciencia de Datos en R  (_10/6/2017_) [Martin A. Miguel | 181/09]

dataset_hongos_clasificados = 'http://www.ic.fcen.uba.ar/~anab/hongos_clasificados.txt'
dataset_hongos_alturas = 'http://www.ic.fcen.uba.ar/~anab/alturas.txt'
dataset_mediciones = 'http://www.ic.fcen.uba.ar/~anab/mediciones.txt'

## Ejercicio 1

hongos.X.I.mu = 4
hongos.X.I.sigma = sqrt(1)
hongos.X.I.sigmaSQ = 1
hongos.X.I.d = function(x) { dnorm(x, hongos.X.I.mu, hongos.X.I.sigma)}
hongos.X.I.p = 0.4
hongos.X.II.mu = 8
hongos.X.II.sigma = sqrt(4)
hongos.X.II.sigmaSQ = 4
hongos.X.II.d = function(x) { dnorm(x, hongos.X.II.mu, hongos.X.II.sigma)}
hongos.X.II.p = 0.6

### Ej 1.1

xs = seq(-3, 25, length.out=300)
ylim = c(0, 0.5)
plot(xs, hongos.X.I.d(xs), type='l', main='Densidad altura', ylab='densidad', col='orange')
lines(xs, hongos.X.II.d(xs), type='l', col='purple')
legend('topright', legend=c('Tipo I', 'Tipo II'), lty=rep(1, 3), lwd=rep(1, 3), col=c('orange', 'purple'))

### Ej 1.2

pMenor3_siTipoI = pnorm(3, hongos.X.I.mu, hongos.X.I.sigma)
sprintf('P(x <= 3 | Tipo 1) = %f', pMenor3_siTipoI)

### Ej 1.3

pMenor3 = pnorm(3, hongos.X.I.mu, hongos.X.I.sigma) * hongos.X.I.p + pnorm(3, hongos.X.II.mu, hongos.X.II.sigma) * hongos.X.II.p
sprintf('P(x <= 3) = %f', pMenor3)

### Ej 1.4

pTipoI_siXMenor3 = (pnorm(3, hongos.X.I.mu, hongos.X.I.sigma) * hongos.X.I.p) / pMenor3
sprintf('P(Tipo 1 | x <= 3) = %f', pTipoI_siXMenor3)

### Ej 1.5

dalturaX = function(t) {
    return (hongos.X.I.d(t) * hongos.X.I.p + hongos.X.II.d(t) * hongos.X.II.p)
}
xs = seq(-5, 25, length.out=300)
plot(xs, dalturaX(xs), main='Densidad X (altura de los hongos)', ylab='densidad', xlab='t', type='l')

### Ej 1.6

ralturaX = function(n) {
    choices = sample(x=c(1, 2), size=n, replace=T)
    mus = c(hongos.X.I.mu, hongos.X.II.mu)[choices]
    sigmas = c(hongos.X.I.sigma, hongos.X.II.sigma)[choices]
    ret = sapply(1:n, function(n_idx) {rnorm(1, mus[n_idx], sigmas[n_idx])})
}

### Ej 1.7

set.seed(123)
hist(ralturaX(1000), freq=F, ylim=c(0, 0.2), main='1000 simulaciones ralturaX', xlab='altura', ylab='densidad')
xs = seq(-5, 25, length.out=1000)
lines(xs, dalturaX(xs), col='blue')

### Ej 1.8

class.optim.variedad = function(t) {
    # ret == 1 si clasifica a tipo 1 o 2 si clasifica a tipo 2
    prob_tI = hongos.X.I.d(t) * hongos.X.I.p
    prob_tII = hongos.X.II.d(t) * hongos.X.II.p
    return ((prob_tII > prob_tI) + 1)
}
hongos.data = read.table(dataset_hongos_clasificados, header=T)
head(hongos.data)
hongos.data.n = length(hongos.data[,1])
preds = sapply(1:hongos.data.n, function(n_idx) {class.optim.variedad(hongos.data[n_idx, 1])})
expected = hongos.data[,2]
hongos.class.mean_error = mean(preds != expected)
sprintf('Error empírico medio: %f', hongos.class.mean_error)

### Ej 1.9

hongos.W.I.p = 0.35
hongos.W.I.mu = hongos.X.I.mu
hongos.W.I.sigma = hongos.X.I.sigma
hongos.W.III.p = 0.65
hongos.W.III.mu = 10
hongos.W.III.sigma = sqrt(4)
dalturaW = function(t) {
    (dnorm(t, hongos.W.I.mu, hongos.W.I.sigma) * hongos.W.I.p + 
     dnorm(t, hongos.W.III.mu, hongos.W.III.sigma) * hongos.W.III.p)
}
xs = seq(1, 16, length.out=200)
plot(xs, dalturaW(xs), main='Densidad W', xlab='t', ylab='densidad', type='l')
#lines(xs, dnorm(xs, hongos.W.I.mu, hongos.W.I.sigma) * hongos.W.I.p, col='orange')
#lines(xs, dnorm(xs, hongos.W.III.mu, hongos.W.III.sigma) * hongos.W.III.p, col='cyan')

### Ej 1.10

hongos.alturas = read.table(dataset_hongos_alturas, header=T)[,1]
xs = seq(min(hongos.alturas), max(hongos.alturas), length.out=200)
par(mfrow=c(1, 2))
hist(hongos.alturas, freq=F, main='Alturas vs densidad X', ylab='densidad', xlab='alturas')
lines(xs, dalturaX(xs), col='darkred')
hist(hongos.alturas, freq=F, main='Alturas vs densidad W', ylab='densidad', xlab='alturas')
lines(xs, dalturaW(xs), col='darkblue')

bosque.log_like.X = sum(sapply(hongos.alturas, dalturaX))
bosque.log_like.W = sum(sapply(hongos.alturas, dalturaW))
sprintf('Log-likelihood X: %f', bosque.log_like.X)
sprintf('Log-likelihood W: %f', bosque.log_like.W)
'Tanto visualmente como en log-like, es más razonable que el bosque se comporte como X'

## Ejercicio 2

### Ej 2.1

est.mediana = median
est.media = mean

### Ej 2.2
# El eje de simetría de X ~ N(0, 1) es $\eta$ = 0

#### Ej 2.2.a

ns = c(30, 50, 100, 500)
Nrep = 1000
est.mediana.sim = lapply(1:length(ns), function(n_idx) {
    n = ns[n_idx]
    mediana.ma = sapply(1:Nrep, function(rep_idx) {
        est.mediana(rnorm(n, 0, 1))
    })
    mediana.ma
})
est.media.sim = lapply(1:length(ns), function(n_idx) {
    n = ns[n_idx]
    media.ma = sapply(1:Nrep, function(rep_idx) {
        est.media(rnorm(n, 0, 1))
    })
    media.ma
})

par(mfrow=c(4, 2))
xlims = c(min(unlist(est.mediana.sim)), max(unlist(est.mediana.sim)))
for (r_idx in 1:4) {
    n = ns[r_idx]
    mediana.title = sprintf('Distribución est.mediana (n=%d)', n)
    hist(est.mediana.sim[[r_idx]], freq=F, ylab='Densidad', xlab='Mediana', main=mediana.title, xlim=xlims)
    media.title = sprintf('Distribución est.media (n=%d)', n)
    hist(est.media.sim[[r_idx]], freq=F, ylab='Densidad', xlab='Media', main=media.title, xlim=xlims)
}

# El estimador $\hat{\eta_n}$ (est.mediana) parecería ser normal, aunque tiene
# más problemas en converger que $\tilde{\eta_n}$ (est.media).
# $\tilde{\eta_n}$ se distribuye de forma normal desde la teoría.


### Ej 2.2.b

est.mse.sim = matrix(nrow=2, ncol=length(ns))
est.mse.sim = data.frame(est.mse.sim)
rownames(est.mse.sim) = c('est.mediana', 'est.media')
colnames(est.mse.sim) = sapply(ns, function(n) {sprintf('n=%d', n)})

for (c_idx in 1:length(ns)) {  
    # Aprovecha que el valor esperado es 0 toma los valores estimados
    # de mediana y media como el error.
    est.mse.sim[1, c_idx] = mean(est.mediana.sim[[c_idx]] ** 2)
    est.mse.sim[2, c_idx] = mean(est.media.sim[[c_idx]] ** 2)
}
est.mse.sim


### Ej 2.3

#### Ej 2.3.a
# El eje de simetría para $X \tilde{} C(0, 1)$ es $\eta$ = 0

ns = c(30, 50, 100, 500)
Nrep = 1000
est.mediana.sim = lapply(1:length(ns), function(n_idx) {
    n = ns[n_idx]
    mediana.ma = sapply(1:Nrep, function(rep_idx) {
        est.mediana(rcauchy(n))
    })
    mediana.ma
})
est.media.sim = lapply(1:length(ns), function(n_idx) {
    n = ns[n_idx]
    media.ma = sapply(1:Nrep, function(rep_idx) {
        est.media(rcauchy(n))
    })
    media.ma
})

par(mfrow=c(4, 2))
xlims = c(min(unlist(est.mediana.sim)), max(unlist(est.mediana.sim)))
for (r_idx in 1:4) {
    n = ns[r_idx]
    mediana.title = sprintf('Distribución est.mediana (n=%d)', n)
    hist(est.mediana.sim[[r_idx]], freq=F, ylab='Densidad', xlab='Mediana', main=mediana.title, xlim=xlims)
    media.title = sprintf('Distribución est.media (n=%d)', n)
    hist(est.media.sim[[r_idx]], freq=F, ylab='Densidad', xlab='Media', main=media.title, xlim=xlims)
}

# La distribución de $\hat{\eta_n}$ aparenta ser normal. 
# La distribución de $\tilde{\eta_n}$ no aparenta ser normal y de hecho $\tilde{\eta_n} \tilde{} C(0,1)$.

est.mse.sim = matrix(nrow=2, ncol=length(ns))
est.mse.sim = data.frame(est.mse.sim)
rownames(est.mse.sim) = c('est.mediana', 'est.media')
colnames(est.mse.sim) = sapply(ns, function(n) {sprintf('n=%d', n)})

for (c_idx in 1:length(ns)) {  
    # Aprovecha que el valor esperado es 0 toma los valores estimados
    # de mediana y media como el error.
    est.mse.sim[1, c_idx] = mean(est.mediana.sim[[c_idx]] ** 2)
    est.mse.sim[2, c_idx] = mean(est.media.sim[[c_idx]] ** 2)
}
est.mse.sim

# Sería más razonable utilizar la mediana como estimador. El comportamiento de
# $\tilde{\eta_n}$ se debe a que la distribución de Cauchy no tiene esperanza.

## Ej 2.4

mediciones = read.table(dataset_mediciones, header=T)
head(mediciones)
mediciones = mediciones[,1]

plot(ecdf(mediciones),main="LA empírica",xlab=" ",ylab=" ")
curve(pnorm(x,mean(mediciones),sd(mediciones)),add=T,col="blue",lwd=2)
curve(pcauchy(x,location=median(mediciones)),add=T,col="red",lwd=2)
legend('topleft', legend=c('empirica', 'norm', 'cauchy'), 
       lty=rep(1, 3), lwd=rep(1, 3), col=c('black', 'blue', 'red'))

#### Ej 2.4.a
# Los datos parecerían estar distribuídos como una variable aleatoria $X \tilde{} C(0,1)$.

#### Ej 2.4.b
# El estimador propuesto es $\hat{\eta_n} = med(\mathbf{x})$

est.eta = median
est.eta.v = est.eta(mediciones)
sprintf('La estimación dada es %f', est.eta.v)

#### Ej 2.4.c

alpha = 0.05
z = qnorm(1 - alpha/2)
Nboot = 1000
est.eta.se.boot = sd(sapply(1:Nboot, function(boot_idx) {
    est.eta(sample(x=mediciones, size=length(mediciones), replace=T))
}))
est.eta.int = c(est.eta.v - z * est.eta.se.boot, est.eta.v + z * est.eta.se.boot)
sprintf('El intervalo bootstrap normal para \\hat{\\eta_n}=(%f, %f)', est.eta.int[1], est.eta.int[2])
