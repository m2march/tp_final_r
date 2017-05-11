# 1
#rm(list=ls())

# 2
auto <- read.table('auto.txt', header=T)

# 3
head(auto)

tail(auto)

# 4
# fix(auto)

# 5
names(auto)
length(names(auto))

length(row.names(auto))

# 6
sum(is.na(auto)) == 0

# 7
attach(auto)

# 8
for (c in auto) {
    print(class(c))
}

# 9
png('ej9.png')
plot(cylinders, mpg)
dev.off()

# 10 
png('ej10a.png')
plot(origin, mpg)
dev.off()

origin.f <- as.factor(origin)
png('ej10b.png')
plot(origin.f, mpg)
dev.off()

# 11
png('ej11.png')
plot(origin.f, mpg, boxcol='blue', horizontal=T,xlab='mpg', ylab='origin')
dev.off()

# 12
bins = seq(min(mpg), max(mpg), length=15)
png('ej12.png')
hist(mpg, breaks=bins)
dev.off()

# 13
m = mean(mpg)
s = sd(mpg)
xs = seq(min(mpg), max(mpg), length=100)
ys = dnorm(xs, mean=m, sd=s)
png('ej13.png')
hist(mpg, breaks=bins, freq=F, main='Histograma de Densidad de mpg')
lines(xs, ys, col='darkgreen')
dev.off()

# 14 
png('ej14.png')
par(mfrow=c(3, 1))
for (o in 1:3) { # Consulta: ¿Se puede evitar el for?
    hist(mpg[origin == o], breaks=bins, freq=F)
}
dev.off()

# 15
png('ej15.png')
counts.origin <- table(origin)
pie(counts.origin, labels=c('Americano', 'Europeo', 'Japones'))
dev.off()
