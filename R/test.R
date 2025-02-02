load("cars.RData")
delka <- cars$Length

d <- cars$DriveTrain
c <- cars$Cylinders

plot(c ~ d)

hist(delka)
cena <- cars$Price
typ <- cars$Type
velikostM <- cars$EngineSize
max(cena)
mean(cena)
cor(cena, velikostM, method = "pearson")
prop.table(table(typ)) * 100 

1 - pbinom(14, 25, 0.40)
n <- 25     # Počet pokusů
p <- 0.40   # Pravděpodobnost úspěchu
k <- 14     # Největší počet úspěchů, který nechceme zahrnout

P_X_ge_15 <- 1 - pbinom(k, size = n, prob = p)

library(dplyr)
pohon <- cars$DriveTrain
pohon <- cars %>% filter(DriveTrain == "Rear")

library(DescTools)
library(TeachingDemos)
motor <- cars$EngineSize
hist(motor)
PlotQQ(motor, pch = 19)
Skew(motor)
Kurt(motor)
shapiro.test(motor)
#p = 0.0001 < alfa zamitame plati H1 nema normalni rozdelení
rpm <- cars$RPM
typ <- cars$Type
oneway.test(rpm ~ typ, var.equal = TRUE)

prop.table(table(pohon, naprava[3]))* 100

cena <- cars$Price
hp <- cars$Horsepower

zavisla <- cena
nezavisla <- hp

plot(zavisla ~ nezavisla)
cor(zavisla, nezavisla)
#silna kladna rostouci prima zavislost´

abline(lm(zavisla ~ nezavisla),col=2,lwd=2)
# grafem prolozim primku
(Model1 <- lm(zavisla ~ nezavisla))
# odhad regresnich koeficientu - popis primky
# porodni hmotnost = -7905.8 + 224.8*porodni delka
#   je dulezite, ktera promenna je na x-ove a ktera na y-ove ose
# s narustem porodni delky o 1cm naroste porodni hmotnost v promeru o 224.8 g
summary(Model1)
# souhrn vystupu
# v casti Coefficients najdeme odhady regresnich koeficientu (Estimate)
#   a dale test o jejich nulovosti (kdyz je posledni hodnota v radku
#   Pr(>|t|) mensi nez 0.05, pak se koeficient vyznamne lisi od nuly,
#   a za posledni hodnotou se objevi alespon jedna hvezdicka)
# v souhrnu Multiple R-squared je procento variability zavisle promenne 
#   vysvetlene modelem: z variability porodni hmotnosti se vysvetlilo 62.5%
confint(Model1)
# 95% intervaly spolehlivosti pro regresni koeficienty
# ani jeden z nich neobsahuje nulu

## Jak zavisi hmotnost v pul roce na porodni hmotnosti?
zavisla <- Kojeni$hmotnost
nezavisla <- Kojeni$porHmotnost