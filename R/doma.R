### Vypocty pravdepodobnosti


## Diskretni rozdeleni
## Binomicke rozdeleni: n - pocet pokusu, p - pst uspechu
# pbinom(k,n,p) - distribucni funkce (vice vysledku) P(x <= k)
# dbinom(k,n,p) - pravdepodobnostni funcke (jeden vysledek) P(x = k)
#Priklad 8 

#a
dbinom(2, 20, 0.02)
#b
dbinom(0, 20, 0.02)
#c
pbinom(2, 20, 0.02)

dbinom(13, 15, 0.90)

n <- 15
p <- 0.93
k <- 12

sum(dbinom(3:5,100,0.035))

pgeom(4,0.20)

sum(dbinom(2:0,20,0.02))
pbinom(2,20,0.02)
1 - pbinom(1, 100 ,0.035)
pbinom(5, 100 ,0.035) - pbinom(2, 100, 0.035)

probability_a <- dbinom(k, size = n, prob = p)
print(probability_a)

n <- 10
p <- 0.93
k <- 8

probability_b <- 1 - pbinom(k - 1, size = n, prob = p)
print(probability_b)