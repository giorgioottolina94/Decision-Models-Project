#Ferrario Marco
#Ottolina Giorgio

library(dplyr)
library(ggplot2)

presents <- read.csv('C:/Users/marco/Downloads/presents.csv') # importo il dataset

set.seed(1234) # imposto il seed per i campioni

names(presents) <- c("Id","x","y","z") 

summary(presents) # si può vedere come le dimensioni dei pacchetti vadano tutte dai 2 ai 250 per lato

presents <- presents[order(-presents$Id),] # i pacchi con id basso vanno inseriti per ultimi

xSleigh <- 1000 # dimesioni lati slitta
ySleigh <- 1000
zSleigh <- Inf  

presents$Volume <- apply(presents[,2:4], 1, prod) # calcolo il volume di ogni pacco (che ovviamente rimane costante in qualsiasi modo sia orientato )

sample <- presents[sample(nrow(presents), 15000), ] # creo un campione per analizzare meglio i dati

ggplot(sample) + geom_point(aes(x,y) , color = 'black') # grafici per controllare l'orientamento
ggplot(sample) + geom_point(aes(x,z) , color = 'black')
ggplot(sample) + geom_point(aes(y,z) , color = 'black')

presents[,2:4] <- t(apply(presents[,2:4],1, sort)) # ruoto i pacchi nello stesso senso (con il lato più lungo relativo alla stessa coordinata)




# funzione per dividere in gruppi in base alle dimensioni

calcolaGruppi <- function(ds) {
  
  quantiles <- quantile(ds$Volume, probs=c(0,0.333,0.666,1))
  
  piccoli <- ds[ds$Volume <= quantiles[2],]
  medi <- ds[ds$Volume > quantiles[2] & ds$Volume <= quantiles[3],]
  grandi <- ds[ds$Volume > quantiles[3],]
  
  return(list(piccoli,medi,grandi))
}








# combinazioni rotazione pacchi

#names(presents) <- c("Id","x","z","y","Volume") #1236146
#names(presents) <- c("Id","z","x","y","Volume") #1525841
#names(presents) <- c("Id","z","y","x","Volume") #1442467
#names(presents) <- c("Id","y","z","x","Volume") #1196627

names(presents) <- c("Id","y","x","z","Volume") #1050789


asseX <- 1  # tiene conto dell'avanzamento sull'asse x
asseY <- 1  # tiene conto dell'avanzamento sull'asse y
asseZ <- 1  # tiene conto dell'avanzamento sull'asse z
y <- 1      # tiene conto della massima y raggiunta nel riempimento dell'asse x
z <- 1      # tiene conto della massima z raggiunta nel riempimento dell'asse x - y


# creo una matrice e un vettore dove salvare le coordinate dei pacchi e gli id
mn <- c("PresentId","x1","y1","z1","x2","y2","z2","x3","y3","z3","x4","y4","z4","x5","y5","z5","x6","y6","z6","x7","y7","z7","x8","y8","z8")
vertici <- matrix(nrow = 0, ncol = 25, dimnames = list(NULL , mn))
Id_posizionato <- c()

repeat {   
  
  gruppo <-  na.omit(presents[c(1:500),]) # divido il gruppo in grandi,medi,piccoli
  
  gruppi <- calcolaGruppi(gruppo)
  small_group <- gruppi[[1]]
  medium_group <- gruppi[[2]]
  big_group <- gruppi[[3]]
  
  
  pacchi <- small_group # assegnazione gruppo di pacchi
  
  i <- 0
  
  # apro 2 cicli,uno per l'asse y e uno per l'asse x (più interno)
  
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){     
    while ((asseX + pacchi[i+1,]$x <= 334) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {                
      i <- i + 1               # indice del pacco
      pacco <- pacchi[i,]      #seleziona il pacco in posizione i
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id) # tengo traccia dell'ordine degli id
      asseX <- asseX + pacco$x                          # avanzo sull'asse x 
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   # tengo conto della massima y raggiunta 
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}   # tengo conto della massima z raggiunta
    }
    asseX <- 1   # quando l'asse x è esaurito, riparti da 1
    y <- asseY   # aggiorna la y di partenza con la massima raggiunta riempendo l'asse x
  }
  altezza_strato_1 <- asseZ # tengo traccia della massima z raggiunta nell'asse x-y
  
  
  asseX <- 334       # nuovo gruppo 
  y <- asseY <- 1    # l'asse y ritorna di valore 1 nel nuovo gruppo 
  asseZ <- 1         # azzero il contatore lungo la z
  pacchi <- medium_group
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 667) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {          
      i <- i + 1    
      pacco <- pacchi[i,]  
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}
    }
    asseX <- 334
    y <- asseY   
  }
  altezza_strato_2 <- asseZ
  
  
  asseX <- 667
  y <- asseY <- 1
  asseZ <- 1         
  pacchi <- big_group
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 1000) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {                       
      i <- i + 1
      pacco <- pacchi[i,]    
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}   
    }
    asseX <- 667
    y <- asseY  
  }
  altezza_strato_3 <- asseZ
  
  
  
  
  
  presents <- presents %>% filter(!(Id %in% Id_posizionato)) # toglie dal dataset i pacchi posizionati
  
  if (nrow(Id_posizionato) == 1000000) { break }
  
 
  gruppo <-  na.omit(presents[c(1:500),]) 
  
  gruppi <- calcolaGruppi(gruppo)
  small_group <- gruppi[[1]]
  medium_group <- gruppi[[2]]
  big_group <- gruppi[[3]]
  
  
  asseX <- 1
  y <- asseY <- 1    
  z <- altezza_strato_1
  
  pacchi <- big_group 
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 334) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {  
      i <- i + 1     
      pacco <- pacchi[i,]      
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x       
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}   
    }
    asseX <- 1       
    y <- asseY  
  }
  altezza_strato_1_1 <- asseZ
  
  
  
  
  
  asseX <- 334    
  y <- asseY <- 1   
  asseZ <- altezza_strato_2         
  pacchi <- medium_group
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 667) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {          
      i <- i + 1    
      pacco <- pacchi[i,]  
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}
    }
    asseX <- 334
    y <- asseY   
  }
  altezza_strato_2_2 <- asseZ
  
  
  asseX <- 667
  y <- asseY <- 1
  asseZ <- altezza_strato_3         
  pacchi <- small_group
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 1000) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {                       
      i <- i + 1
      pacco <- pacchi[i,]    
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}   
    }
    asseX <- 667
    y <- asseY  
  }
  altezza_strato_3_3 <- asseZ
  
  
  z <- max(altezza_strato_1_1, altezza_strato_2_2, altezza_strato_3_3)
  
 
  presents <- presents %>% filter(!(Id %in% Id_posizionato))
  
  
  if (nrow(Id_posizionato) == 1000000) { break }
  

  
  print(nrow(Id_posizionato)) # tengo conto delle righe per monitorare l'esecuzione
  
}

final_z <- max(z, altezza_strato_1, altezza_strato_2, altezza_strato_3) # trovo l'altezza raggiunta 


print(paste("Altezza finle raggiunta:",final_z))


# calcolo la seconda metrica

Id_posizionato <- as.matrix(rev(Id_posizionato))

c <- (1:1000000)

Id_posizionato <- cbind(Id_posizionato,c)


differenza <- apply(Id_posizionato, 1, function(x) abs(x[2]-x[1]) )

score_id <- sum(differenza)


final_score = 2*final_z + score_id


write.csv(vertici,file = "coordinate.csv")








#----------------------------------------------------------------------------------------------


# prove con campioni piccoli per le dimensioni

quantiles <- quantile(presents$Volume, probs=c(0,0.333,0.666,1)) 

small_presents <- presents[presents$Volume <= quantiles[2],]
medium_presents <- presents[presents$Volume > quantiles[2] & presents$Volume <= quantiles[3],]
big_presents <- presents[presents$Volume > quantiles[3],]

small_sample <- small_presents[sample(nrow(small_presents), 5000), ]
medium_sample <- medium_presents[sample(nrow(medium_presents), 5000), ]
big_sample <- big_presents[sample(nrow(big_presents), 5000), ]

sample <- rbind(small_sample,medium_sample,big_sample)

sample <- sample[order(-sample$Id),]



asseX <- 1  
asseY <- 1  
asseZ <- 1  
y <- 1 
z <- 1 


mn <- c("PresentId","x1","y1","z1","x2","y2","z2","x3","y3","z3","x4","y4","z4","x5","y5","z5","x6","y6","z6","x7","y7","z7","x8","y8","z8")
vertici <- matrix(nrow = 0, ncol = 25, dimnames = list(NULL , mn))
Id_posizionato <- c()

repeat {   
  
  gruppo <-  na.omit(sample[c(1:6000),])
  
  gruppi <- calcolaGruppi(gruppo)
  small_sample <- gruppi[[1]]
  medium_sample <- gruppi[[2]]
  big_sample <- gruppi[[3]]
  
  
  pacchi <- small_sample 
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){           
    while ((asseX + pacchi[i+1,]$x <= 334) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {     
      i <- i + 1     
      pacco <- pacchi[i,]      
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x      
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}   
    }
    asseX <- 1       
    y <- asseY   
  }
  altezza_strato_1 <- asseZ 
  
  
  asseX <- 334   
  y <- asseY <- 1    
  asseZ <- 1          
  pacchi <- medium_sample
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 667) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {          
      i <- i + 1    
      pacco <- pacchi[i,]  
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}
    }
    asseX <- 334
    y <- asseY   
  }
  altezza_strato_2 <- asseZ
  
  
  asseX <- 667
  y <- asseY <- 1
  asseZ <- 1         
  pacchi <- big_sample
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 1000) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {                       
      i <- i + 1
      pacco <- pacchi[i,]    
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}   
    }
    asseX <- 667
    y <- asseY  
  }
  altezza_strato_3 <- asseZ
  
  
  
  
  
  sample <- sample %>% filter(!(Id %in% vertici[,1]))
  
  if (nrow(vertici) == 15000) { break }
  
 
  gruppo <-  na.omit(sample[c(1:6000),])
  
  gruppi <- calcolaGruppi(gruppo)
  small_sample <- gruppi[[1]]
  medium_sample <- gruppi[[2]]
  big_sample <- gruppi[[3]]
  
  
  asseX <- 1
  y <- asseY <- 1    
  z <- altezza_strato_1
  
  pacchi <- big_sample
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 334) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) { 
      i <- i + 1     
      pacco <- pacchi[i,]     
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x      
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}  
    }
    asseX <- 1      
    y <- asseY   
  }
  altezza_strato_1_1 <- asseZ
  
  
  
  
  
  asseX <- 334    
  y <- asseY <- 1    
  asseZ <- altezza_strato_2         
  pacchi <- medium_sample
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 667) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {          
      i <- i + 1    
      pacco <- pacchi[i,]  
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}
    }
    asseX <- 334
    y <- asseY  
  }
  altezza_strato_2_2 <- asseZ
  
  
  asseX <- 667
  y <- asseY <- 1
  asseZ <- altezza_strato_3         
  pacchi <- small_sample
  i <- 0
  while ((i != nrow(pacchi)) & (asseY + pacchi[i+1,]$y <= 1000)){             
    while ((asseX + pacchi[i+1,]$x <= 1000) & (asseY + pacchi[i+1,]$y <= 1000) & (i != nrow(pacchi))) {                       
      i <- i + 1
      pacco <- pacchi[i,]    
      vertici <- rbind(vertici, c(pacco$Id, asseX, y, z, asseX, y + pacco$y, z, asseX + pacco$x, y, z, asseX + pacco$x, y + pacco$y, z, asseX, y, z + pacco$z, asseX, y + pacco$y, z + pacco$z, asseX + pacco$x, y, z + pacco$z, asseX + pacco$x, y + pacco$y, z + pacco$z))
      Id_posizionato <- rbind(Id_posizionato, pacco$Id)
      asseX <- asseX + pacco$x
      if (y + pacco$y > asseY) {asseY <- y + pacco$y}   
      if (z + pacco$z > asseZ) {asseZ <- z + pacco$z}   
    }
    asseX <- 667
    y <- asseY  
  }
  altezza_strato_3_3 <- asseZ
  
  
  z <- max(altezza_strato_1_1, altezza_strato_2_2, altezza_strato_3_3)
  

  sample <- sample %>% filter(!(Id %in% Id_posizionato))
  
  
  if (nrow(vertici) == 15000) { break }
  
  print(nrow(Id_posizionato))
  
  
}

final_z <- max(z, altezza_strato_1, altezza_strato_2, altezza_strato_3)


print(final_z)




