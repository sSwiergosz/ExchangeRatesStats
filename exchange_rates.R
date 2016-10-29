library(gdata)
library(psych)
library(vioplot)
library(stringr)

waluty <- function(){
  cat("Program oblicza statystyki dla kursow walut w latach 1995 - 2016 (do 8 czerwca)\n")
  cat("Prosze podac rok poczatkowy: ")
  rok_poczatek <- scan(what = 1, n = 1) #wczytywanie roku poczatkowego
  if(rok_poczatek < 1995 || rok_poczatek%%1!=0){
    stop("Prosze podac date z poprawnego zakresu")
  }
  cat("Prosze podac rok koncowy: ")
  rok_koniec <- scan(what = 1, n = 1) #wczytywanie roku koncowego
  if(rok_koniec > 2016 || rok_koniec%%1!=0){
    stop("Prosze podac date z poprawnego zakresu")
  }
  
  wektor_kursow_walut <- c() #przechowywanie kursow walut z kilku lat

  lata <- vector()
  for(i in rok_poczatek:rok_koniec){ #dodawanie lat do wektora
    lata <- c(lata, i)
  }
  
  cat("Podaj trzyliterowy skrot waluty: ")
  nazwa_waluty <- scan(what = "", n = 1) #wczytywanie skrotu waluty
  
  for(i in lata){ #sciaganie plikow ze strony NBP dla lat umieszczonych w wektorze
    download.file(paste0("http://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_", i, ".xls"), destfile = paste0("/home/szymek/Desktop/Waluty/", i, ".xls"))
    if(i > "1994" && i < "2000"){
      kursy <- read.xls(paste0("/home/szymek/Desktop/Waluty/", i, ".xls"), check.names = FALSE, skip = 1) #wczytywanie arkusza z danego roku
      if(i == "1995"){
        kursy <- kursy[-93,] #usuwanie wiersza ze skrotami walut znajdujacego sie w 93 linii pliku
      }
    }
    else if(i > "1999" && i < "2004"){
      kursy <- read.xls(paste0("/home/szymek/Desktop/Waluty/", i, ".xls"), check.names = FALSE)
    }
    else if(i == "2004"){
      kursy <- read.xls(paste0("/home/szymek/Desktop/Waluty/", i, ".xls"), check.names = FALSE)
      kursy <- kursy[-nrow(kursy), ] #usuwanie ostatniego wiersza zawierajacego niepotrzebne informacje
    }
    else if(i > "2004" && i < "2008" || i == "2012"){
      kursy <- read.xls(paste0("/home/szymek/Desktop/Waluty/", i, ".xls"), check.names = FALSE)
      kursy <- kursy[-1,] #usuwanie zbednego wiersza z poczatku arkusza
      if(i == "2005"){
        kursy <- kursy[-nrow(kursy), ]
      }
    }
    else if(i > "2007" && i < "2017"){
      kursy <- read.xls(paste0("/home/szymek/Desktop/Waluty/", i, ".xls"), check.names = FALSE)
      kursy <- kursy[-1,]
      kursy <- kursy[-nrow(kursy), ]
      kursy <- kursy[-nrow(kursy), ]
      if(i > "2012" && i < "2017"){
        kursy <- kursy[-nrow(kursy), ]
      }
    }
    if(nazwa_waluty %in% str_sub(names(kursy), start = -3) == TRUE){
      #wycinanie z naglowkow skrotow walut i sprawdzanie czy w nich znajduje sie skrot podany prez uzytkownika
      for(j in names(kursy)){ #przechodzenie po wszystkich naglowkach w arkuszu
        if(str_sub(j, start = -3) == nazwa_waluty){
          indeks <- grep(j, colnames(kursy)) #pobieranie indeksu aktualnej kolumny
          j <- gsub("([0-9])([A-z])", '\\1 \\2', j) #rozdzielanie cyfr od liter w nazwie kolumny
          
          if(i == "2008"){
            przelicznik <- as.numeric(as.character(kursy[1, indeks])) #przelicznik przez ktory dzielone sa kursy znajduje sie w 1 wierszu
          }
          else{
            przelicznik <- gsub(" .*$", "", j) #wycinanie z naglowka kolumny przelicznika(do pierwszej spacji po liczbie)
            przelicznik <- strtoi(przelicznik) #konwersja do typu integer
          }
          
          kursy_z_kolumny <- kursy[,indeks] #pobranie wszystkich wartosci z wybranej kolumny
          kursy_ostateczne <- as.numeric(as.character(kursy_z_kolumny)) #wyciagniecie wartosci liczbowych(z typu factor)
          
          if(i == "1995"){
            for(k in 1:91){ #przeliczanie kursow walut na 1 zl
              kursy_ostateczne[k] <- kursy_ostateczne[k]/przelicznik
              wektor_kursow_walut <- c(wektor_kursow_walut, kursy_ostateczne[k]) #dodawanie zmienionych kursow do wektora
            }
            for(k in 93:nrow(kursy)){
              wektor_kursow_walut <- c(wektor_kursow_walut, kursy_ostateczne[k]) 
            }
          }
          else if(i == "2008"){
            for(k in 2:nrow(kursy)){
              if(przelicznik > 1){
                kursy_ostateczne[k] <- kursy_ostateczne[k]/przelicznik
              }
              if(is.na(kursy_ostateczne[k]) == FALSE){ #sprawdzanie czy nie ma brakujacych wartosci
                wektor_kursow_walut <- c(wektor_kursow_walut, kursy_ostateczne[k])
              }
            }
          }
          else{
            for(k in 1:nrow(kursy)){
              if(przelicznik > 1){
                kursy_ostateczne[k] <- kursy_ostateczne[k]/przelicznik
              }
              if(is.na(kursy_ostateczne[k]) == FALSE){
                wektor_kursow_walut <- c(wektor_kursow_walut, kursy_ostateczne[k])
              }
            }
          }
        }
      }
    }
    else{
      cat(paste("W roku", i, "nie ma kursu dla podanej waluty \n"))
    }
  }
  if(length(wektor_kursow_walut) != 0){
    cat(paste("Statystyki dla wybranej waluty:", nazwa_waluty, "w latach", head(lata, n = 1), "-", tail(lata, n = 1), "\n"))
    
    cat(paste("Wielkosc proby:", length(wektor_kursow_walut), "\n"))
    cat(paste("Wartosc maksymalna:", max(wektor_kursow_walut), "\n"))
    cat(paste("Wartosc minimalna:", min(wektor_kursow_walut), "\n"))
    cat(paste("Srednia arytmetyczna:", round(mean(wektor_kursow_walut), digits = 5), "\n"))
    cat(paste("Srednia geometryczna:", round(geometric.mean(wektor_kursow_walut), digits = 5), "\n"))
    cat(paste("Srednia harmoniczna:", round(harmonic.mean(wektor_kursow_walut), digits = 5), "\n"))
    cat(paste("Kwartyl rzedu 0.25:", round(quantile(wektor_kursow_walut, probs = 0.25), digits = 5), "\n"))
    cat(paste("Kwartyl rzedu 0.75:", round(quantile(wektor_kursow_walut, probs = 0.75), digits = 5), "\n"))
    cat(paste("Mediana:", round(median(wektor_kursow_walut), digits = 5), "\n"))
    cat(paste0("Przedzial zmiennosci proby: (", min(wektor_kursow_walut), ", ", max(wektor_kursow_walut), ")\n"))
    cat(paste("Wariancja:", round(var(wektor_kursow_walut), digits = 5), "\n"))
    cat(paste("Odchylenie standardowe:", round(sd(wektor_kursow_walut), digits = 5), "\n"))
    cat(paste("Rozstep miedzy kwartylami:", round(quantile(wektor_kursow_walut, probs = 0.75)-quantile(wektor_kursow_walut, probs = 0.25), digits = 5), "\n"))
    cat(paste("Kurtoza:", round(kurtosi(wektor_kursow_walut), digits = 5), "\n"))
    cat(paste("Skosnosc:", round(skew(wektor_kursow_walut), digits = 5), "\n"))
    pdf(paste0("/home/szymek/Desktop/Waluty/", nazwa_waluty, "_", lata[1], "_", tail(lata, n=1), ".pdf"), width = 10, height = 10)
    hist(wektor_kursow_walut, main = paste("Histogram dla", nazwa_waluty, "w latach", head(lata, n = 1), "-", tail(lata, n = 1), "\n"))
    boxplot(wektor_kursow_walut, main = paste("Wykres pudelkowy dla", nazwa_waluty, "w latach", head(lata, n = 1), "-", tail(lata, n = 1), "\n"))
    vioplot(wektor_kursow_walut, names = paste("Wykres skrzypcowy", nazwa_waluty, "w latach", head(lata, n = 1), "-", tail(lata, n = 1), "\n"))
    dev.off()
  }
  else{
    cat("Brak statystyk")
  }
}