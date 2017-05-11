# Ej 1
p <- 1/6  # Probabilidad de que gane Juan
Nrep <- 1000

print("Ej 1")
print(c("Juan gana", sum(runif(Nrep) < p)), quote=F)

# Ej 2
unajugada <- function(p) {
    if (runif(1) < p) {
        return("Juan")
    } else { 
        return("Pedro")
    }
}
print(unajugada(p))
print(unajugada(p))
print(unajugada(p))
print(unajugada(p))

# Ej 3

ps = c(0.2, 0.5, 0.8)
print(c("p, ", "est_p"), quote=F)
for (p in ps) {
   print(c(p, sum(runif(100) < p) / 100.), quote=F)
}

# Ej 4
juanJuega <- function(n, N, p) { # n (monedas Juan), N (monedas total), p (probabilidad de ganar de juan)
    m <- N - n
    while (n > 0 && m > 0) {
        if (unajugada(p) == "Juan") {
            n <- n + 1
            m <- m - 1
        } else {
            n <- n - 1
            m <- m + 1
        }
    }
    if (n == 0) {
        return("Pierde")
    } else {
        return("Gana")
    }
}

print(juanJuega(3, 6, 0.2))
print(juanJuega(3, 6, 0.2))
print(juanJuega(3, 6, 0.2))
print(juanJuega(3, 6, 0.2))
print(juanJuega(3, 4, 0.9))

# Ej 5
simulaNPartidas <- function(n, N, p, Nrep) {
    partidas = c()
    for (i in 1:Nrep) {
        partidas = c(partidas, juanJuega(n, N, p))
    }
    return(sum(partidas == "Gana"))
}
print(simulaNPartidas(3, 5, 1/6, 1000) / 1000)
print(simulaNPartidas(3, 5, 1/6, 1000) / 1000)


# Ej 6
MCprobaJuanGana <- function(n, N, p, Nrep) {
    return(simulaNPartidas(n, N, p, Nrep) / Nrep)
}
print(MCprobaJuanGana(3, 5, 1/6, 1000))
print(MCprobaJuanGana(3, 5, 1/6, 1000))


# Ej 7
# Ej 8
N <- 5
p <- 1/6
xs <- 1:N
ys <- sapply(xs, function(x) {return(MCprobaJuanGana(x, N, p, 1000))})

png('MCprobaJuanGana-ej8.png')
plot(xs, ys, type='b', main='Ej 8', 
     sub='Probabilidad de que Juan gane dependiendo de sus monedas',
     xlab='Cantidad de monedas de Juan (n)',
     ylab='Probabilidad de que Juan gane')
dev.off()


# Ej 9
ys <- sapply(xs, function(x) {return(MCprobaJuanGana(x, N, 0.5, 1000))})

png('MCprobaJuanGana-ej9.png')
plot(xs, ys, type='b', main='Ej 9', 
     sub='Probabilidad de que Juan gane dependiendo de sus monedas',
     xlab='Cantidad de monedas de Juan (n)',
     ylab='Probabilidad de que Juan gane')
dev.off()

# Ej 10
ys <- sapply(xs, function(x) {return(MCprobaJuanGana(x, N, 0.8, 1000))})

png('MCprobaJuanGana-ej10.png')
plot(xs, ys, type='b', main='Ej 10', 
     sub='Probabilidad de que Juan gane dependiendo de sus monedas',
     xlab='Cantidad de monedas de Juan (n)',
     ylab='Probabilidad de que Juan gane')
dev.off()

# Ej 12
N <- 10
xs <- 1:N
ys <- sapply(xs, function(x) {return(MCprobaJuanGana(x, N, 0.5, 1000))})

png('MCprobaJuanGana-ej10.png')
plot(xs, ys, type='b', main='Ej 10', 
     sub='Probabilidad de que Juan gane dependiendo de sus monedas',
     xlab='Cantidad de monedas de Juan (n)',
     ylab='Probabilidad de que Juan gane')
dev.off()

# Ej 13
N <- 50
xs <- 1:N
ys <- sapply(xs, function(x) {return(MCprobaJuanGana(x, N, 0.5, 1000))})

png('MCprobaJuanGana-ej13.png')
plot(xs, ys, type='b', main='Ej 13', 
     sub='Probabilidad de que Juan gane dependiendo de sus monedas',
     xlab='Cantidad de monedas de Juan (n)',
     ylab='Probabilidad de que Juan gane')
dev.off()

# Ej 14
