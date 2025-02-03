
load("Kojeni.RData")
load("Cars.RData")
# Kvalitativní (Kategoriální - slovní)

#   Nominální (Slova bez pořadí)
  porodnice <- Kojeni$Porodnice
  # absolutní četnost
  (ac <- table(porodnice))
  # relativní četnost
  (rc <- round(prop.table(ac) * 100, 2))
  
  cbind("Absolutní četnost" = ac, "Relativní četnost" = rc)
  
  hoch <- Kojeni$hoch
  Porodnice <- Kojeni$Porodnice
  
  Por.poh<-ifelse(hoch==1&Porodnice=="Praha","Hoch z Prahy","Hoch z venkova")
  Por.poh<-ifelse(hoch==0&Porodnice=="Praha","Divka z Prahy",Por.poh)
  Por.poh<-ifelse(hoch==0&Porodnice=="okresní","Divka z venkova",Por.poh)
  
  #BARPLOT
  barplot(table(Por.poh),col=2:5,main="Sloupcovy graf")
  
  #KOLAČOVY GRAF
  pie(table(Por.poh),col=2:5,main="Kolacovy graf")

  #Popisky
  leg<-sort(unique(Por.poh))
  rc<-round(prop.table(table(Por.poh))*100,2)
  for(i in 1:length(leg)){
    leg[i]<-paste(leg[i],"(",rc[i],"%)")
  }
  pie(table(Por.poh),col=2:5,main="Kolacovy graf",labels=leg)
  
    
#   Ordinální (Slova s pořadím)
  
  Vzdelani <- Kojeni$Vzdelani
  Vzdelani <- ordered(Vzdelani, levels = c("základní", "maturita", "VŠ") )
  table(Vzdelani)
  
  # absolutni cetnosti
  (ac<-table(Vzdel))
  # relativni cetnosti
  (rc<-round(prop.table(table(Vzdel)),2))
  cbind("absolutni"=ac,"relativni"=rc)
  
  #  kumulativni absolutni cetnosti
  (kac<-cumsum(ac))
  # kumulativni relativni cetnosti
  (krc<-cumsum(rc))
  cbind("n(i)"=ac,"N(i)"=kac,"p(i)"=rc,"P(i)"=krc)
  
  barplot(ac, col = 2:4, main = "Sloupcový graf")

# Kvantitativní (Numerické) 
  library(DescTools)
  Price <- cars$Price
  prom <- Price
  n <- length(prom)
  Mean <- mean(prom)
  Huber <- HuberM(prom)
  sumary <- summary(prom)
  SD <- sd(prom)
  IQR <- IQR(prom)
  MAD <- MAD(prom)
  CoefVar <- CoefVar(prom)
  Skew <- Skew(prom)
  Kurt <- Kurt(prom)
  
  data.frame(prom = "Price" ,cbind("n" = n, 
                                    "Mean" = Mean, 
                                    "Huber" = Huber,
                                    "Min" = sumary[1], 
                                    "First Qu" = sumary[2], 
                                    "Median" = sumary[3], 
                                    "Third Qu" = sumary[4], 
                                    "Max" = sumary[5]),
                                    "SD" = SD,
                                    "IQR" = IQR,
                                    "MAD" = MAD,
                                    "CoefVar" = CoefVar,
                                    "Skew" = Skew,
                                    "Kurt" = Kurt
              )
  
#   Diskrétní (int)
  
    valce <- cars$Cylinders
    
    (ac <- table(valce))
    #valce bez rotary
    valce2 <- droplevels(valce[valce != "rotary"])
    (ac<-table(valce2))
    (kac<-cumsum(ac))
    (rc<-round(prop.table(table(valce2)),2))
    (krc<-cumsum(rc))
    data.frame(cbind("n(i)"=ac,"N(i)"=kac,"f(i)"=rc,"F(i)"=krc))
    
    # Frekvencni polygon  
    x.val<-as.numeric(as.character(names(ac)))
    ac.n<-as.numeric(ac)
    # zmena typu promenne kvuli popiskum na y-ove ose v grafu
    plot(x.val,ac.n,type="h",lwd=3,col="darkgreen",main="Frekvencni polygon",
         xlab="Pocet valcu",ylab="Absolutni cetnosti")
    x.val2<-min(x.val):max(x.val)
    ac2<-rep(0,length(x.val2))
    for(i in 1:length(x.val2)){
      for(j in 1:length(x.val)){
        
        if(x.val2[i]==x.val[j]){ac2[i]<-ac[j]}}
    
      }
    lines(x.val2,ac2,col="red")
    
    hp <- Cars93$Horsepower
    hist(hp,col="skyblue",border="darkblue",main="Histogram",
         xlab="Sila vozu",ylab="Absolutni cetnosti")
    hist(hp,col="skyblue",border="darkblue",main="Histogram",
         xlab="Sila vozu",ylab="Absolutni cetnosti",breaks=10)
    # pomoci parametru "breaks" je mozne nastavit i jemnejsi deleni, 
    #   ale pro tabulku frekvencniho rozdeleni asi neni treba
    hist(hp,plot=F)
    deleni<-hist(hp,plot=F)$breaks
    popis<-as.character(deleni[-1])
    for(i in 1:length(popis)){
      popis[i]<-paste("(",deleni[i],",",deleni[i+1],"]")
    }
    ac<-hist(hp,plot=F)$counts
    kac<-cumsum(ac)
    rc<-round(ac/sum(ac),3)
    krc<-cumsum(rc)
    names(ac) <- popis
    data.frame("n(i)"=ac,"N(i)"=kac,"f(i)"=rc,"F(i)"=krc)     
    
#   Spojité (float)
    
    # Krabicovy graf
    boxplot(hp,col="yellow",border="orange3",main="Krabicovy graf",
            ylab="Sila vozu")
    # samostatne body ukazuji odlehla pozorovani
    boxplot(hp,col="yellow",border="orange3",main="Krabicovy graf",
            ylab="Sila vozu",range = 3)
    # graf bez odlehlych pozorovani
    
    #histogram
    hist(hp, col="skyblue",border="darkblue",main="Histogram",ylab="Hustota",
         xlab="Síla vozu (hp)",breaks=15,freq = F)
    (jadro <- density(hp))
    lines(jadro,col=2, lwd = 3)
    summary(hp)
    library(DescTools)
    # knihovna s uzitecnymi prikazy na popisne statistiky
    
    # Jaka je variabilita delky vozu
    delka <- Cars93$Length
    var(delka)
    sd(delka)
    # Zakladni charakteristiky variability odvozene od prumeru 
    # Vyberovy rozptyl a smerodatna odchylka
    # Jake maji jednotky, jak je interpretovat?
    IQR(delka)
    MAD(delka)
    # Robustni charakteristiky variability mene citlive na odlehla pozorovani
    # Mezikvartilove rozpeti a medianova absolutni odchylka okolo medianu
    # Jake maji jednotky, jak je interpretovat?
    CoefVar(delka)
    # Variacni koeficient
    # Jake ma jednotky a jak ho interpretovat?
    
    # Standardizovane veliciny - z.skory
    z.val<-scale(delka)
    # Jak jsou velke? Co popisuji? K cemu se pouzivaji?
    
    # Caharakteristiky tvaru rozdeleni
    hist(delka, col = "tomato", main = "Histogram", ylab = "Absolutní četnosti", xlab = "Síla vozu")
    #hist(delka, col = "tomato", main = "Histogram", ylab = "Husota", xlab = "Síla vozu", freq = FALSE)
    boxplot(delka)
    mean(delka)
    median(delka)
    Skew(delka)
    Kurt(delka)
    