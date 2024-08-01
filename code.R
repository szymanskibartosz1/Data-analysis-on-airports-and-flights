library(shiny)
library(dplyr)
library("R.utils")
library(ggplot2)

#Pobieranie, rozpakowywanie plikow
cyferki <- seq(1375005,1374993, by = -1)
cyferki2 <- seq(1374929,1374922, by=-1)
wszystkie <- c(cyferki, cyferki2, 1374918)
lata <- seq(1987,2008,1)
urls <- paste("https://dataverse.harvard.edu/api/access/datafile/", wszystkie, sep = "")
sciezki <- paste("C:/Users/barto/OneDrive/Pulpit/Projekt grupowy PDU 2/projektpdu/",lata, ".csv.bz2", sep = "")
download.file(urls, sciezki)

for (i in 1:length(sciezki)){
  bunzip2(sciezki[i]) 
}
####### Wczytanie plikow
dwa <- paste("C:/Users/barto/OneDrive/Pulpit/Projekt grupowy PDU 2/projektpdu/",lata, ".csv", sep = "")
rok1987 <- read.csv(dwa[1])
rok1988 <- read.csv(dwa[2])
rok1989 <- read.csv(dwa[3])
rok1990 <- read.csv(dwa[4])
rok1991 <- read.csv(dwa[5])
rok1992 <- read.csv(dwa[6])
rok1993 <- read.csv(dwa[7])
rok1994 <- read.csv(dwa[8])
rok1995 <- read.csv(dwa[9])
rok1996 <- read.csv(dwa[10])
rok1997 <- read.csv(dwa[11])
rok1998 <- read.csv(dwa[12])
rok1999 <- read.csv(dwa[13])
rok2000 <- read.csv(dwa[14])
rok2001 <- read.csv(dwa[15])
rok2002 <- read.csv(dwa[16])
rok2003 <- read.csv(dwa[17])
rok2004 <- read.csv(dwa[18])
rok2005 <- read.csv(dwa[19])
rok2006 <- read.csv(dwa[20])
rok2007 <- read.csv(dwa[21])
rok2008 <- read.csv(dwa[22])

#Wykres 1

opoznienie1987 <- aggregate(rok1987$ArrDelay , rok1987[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1988 <- aggregate(rok1988$ArrDelay , rok1988[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1989 <- aggregate(rok1989$ArrDelay , rok1989[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1990 <- aggregate(rok1990$ArrDelay , rok1990[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1991 <- aggregate(rok1991$ArrDelay , rok1991[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1992 <- aggregate(rok1992$ArrDelay , rok1992[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1993 <- aggregate(rok1993$ArrDelay , rok1993[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1994 <- aggregate(rok1994$ArrDelay , rok1994[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1995 <- aggregate(rok1995$ArrDelay , rok1995[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1996 <- aggregate(rok1996$ArrDelay , rok1996[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1997 <- aggregate(rok1997$ArrDelay , rok1997[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1998 <- aggregate(rok1998$ArrDelay , rok1998[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1999 <- aggregate(rok1999$ArrDelay , rok1999[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie1990 <- aggregate(rok1990$ArrDelay , rok1990[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2000 <- aggregate(rok2000$ArrDelay , rok2000[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2001 <- aggregate(rok2001$ArrDelay , rok2001[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2002 <- aggregate(rok2002$ArrDelay , rok2002[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2003 <- aggregate(rok2003$ArrDelay , rok2003[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2004 <- aggregate(rok2004$ArrDelay , rok2004[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2005 <- aggregate(rok2005$ArrDelay , rok2005[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2006 <- aggregate(rok2006$ArrDelay , rok2006[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2007 <- aggregate(rok2007$ArrDelay , rok2007[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
opoznienie2008 <- aggregate(rok2008$ArrDelay , rok2008[,c("Year", "Month") ], FUN=mean, na.rm = TRUE)
ostatecznie <- rbind(opoznienie1987, opoznienie1988, opoznienie1989, opoznienie1990, opoznienie1991, opoznienie1992, opoznienie1993, opoznienie1994, opoznienie1995, opoznienie1996, opoznienie1997, opoznienie1998, opoznienie1999, opoznienie2000, opoznienie2001, opoznienie2002, opoznienie2003, opoznienie2004, opoznienie2005, opoznienie2006, opoznienie2007, opoznienie2008)


ggplot(ostatecznie, aes(x = Month, y = Year, fill = x)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black") +
  labs(x = "Miesiąc", y = "Rok", fill = "Opoznienie (min)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_y_continuous(breaks = seq(1987, 2008, by = 1)) +
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    axis.text.x = element_text(size = 10, face = "bold"),  # Powiększenie i pogrubienie napisów na osi x
    axis.text.y = element_text(size = 10, face = "bold")   # Powiększenie i pogrubienie napisów na osi y
  )

#Wykres 2

styczen1 <- round(mean(ostatecznie[ostatecznie$Month == 1, "x"]), 3)
luty2 <- round(mean(ostatecznie[ostatecznie$Month == 2, "x"]), 3)
marzec3 <- round(mean(ostatecznie[ostatecznie$Month == 3, "x"]), 3)
kwiecien4 <- round(mean(ostatecznie[ostatecznie$Month == 4, "x"]), 3)
maj5 <- round(mean(ostatecznie[ostatecznie$Month == 5, "x"]), 3)
czerwiec6 <- round(mean(ostatecznie[ostatecznie$Month == 6, "x"]), 3)
lipiec7 <- round(mean(ostatecznie[ostatecznie$Month == 7, "x"]), 3)
sierpien8 <- round(mean(ostatecznie[ostatecznie$Month == 8, "x"]), 3)
wrzesien9 <- round(mean(ostatecznie[ostatecznie$Month == 9, "x"]), 3)
pazdziernik10 <- round(mean(ostatecznie[ostatecznie$Month == 10, "x"]), 3)
listopad11 <- round(mean(ostatecznie[ostatecznie$Month == 11, "x"]), 3)
gruzien12 <- round(mean(ostatecznie[ostatecznie$Month == 12, "x"]), 3)

j <- c(styczen1, luty2, marzec3, kwiecien4, maj5, czerwiec6, lipiec7, sierpien8, wrzesien9, pazdziernik10, listopad11, gruzien12)
plot(1:12, j, type = "o", lty = 1, col = "blue", pch = 16, lwd = 3, ylim=c(0,12), xlab = "Miesiąc", ylab = "Wartość w minutach", main = "Wykres wartosci sredniego opoznienia w danych miesiacach", xaxt = "n")
axis(1, at = 1:12, labels = c("styczeń", "luty", "marzec", "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", "wrzesień", "październik", "listopad", "grudzień"), las = 1)


#Wykres 3 Shiny

ramki_danych <- rbind(rok1987, rok1988, rok1989, rok1990,rok1991,rok1992,rok1993,rok1994,rok1995,rok1996,rok1997,rok1998,rok1999, rok2000, rok2001, rok2002, rok2003, rok2004, rok2005, rok2006, rok2007, rok2008)


ui <- fluidPage(
  titlePanel("Wykres ilości odwołanych lotów w danym roku"),
  sidebarLayout(
    sidebarPanel(
      h4("Ustawienia wykresu"),
      sliderInput("rok", "Rok:", min = 1987, max = 2008, value = c(1987, 2008))
    ),
    mainPanel(
      plotOutput("wykres")
    )
  )
)

nrow(rok1987[rok1987$Cancelled == 1,])
nrow(rok1987)

server <- function(input, output) {
  dane <- reactive({
    ramki_danych %>%
      filter(Year >= input$rok[1] & Year <= input$rok[2] & Cancelled == 1)
  })
  
  output$wykres <- renderPlot({
    ggplot(dane(), aes(x = Year)) +
      geom_bar() +
      labs(x = "Rok", y = "Ilość lotów odwołanych", fill = "Odwołany") +
      scale_x_continuous(breaks = seq(input$rok[1], input$rok[2], by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  })
}

shinyApp(ui = ui, server = server)

#wykres4

odwloane = c(nrow(rok1987[rok1987$Cancelled == 1, ]),nrow(rok1988[rok1988$Cancelled == 1, ]),nrow(rok1989[rok1989$Cancelled == 1, ]),nrow(rok1990[rok1990$Cancelled == 1, ]),nrow(rok1991[rok1991$Cancelled == 1, ]),nrow(rok1992[rok1992$Cancelled == 1, ]),nrow(rok1993[rok1993$Cancelled == 1, ]),nrow(rok1994[rok1994$Cancelled == 1, ]),nrow(rok1995[rok1995$Cancelled == 1, ]),nrow(rok1996[rok1996$Cancelled == 1, ]),nrow(rok1997[rok1997$Cancelled == 1, ]),nrow(rok1998[rok1998$Cancelled == 1, ]),nrow(rok1999[rok1999$Cancelled == 1, ]),nrow(rok2000[rok2000$Cancelled == 1, ]),nrow(rok2001[rok2001$Cancelled == 1, ]),nrow(rok2002[rok2002$Cancelled == 1, ]),nrow(rok2003[rok2003$Cancelled == 1, ]),nrow(rok2004[rok2004$Cancelled == 1, ]),nrow(rok2005[rok2005$Cancelled == 1, ]),nrow(rok2006[rok2006$Cancelled == 1, ]),nrow(rok2007[rok2007$Cancelled == 1, ]),nrow(rok2008[rok2008$Cancelled == 1, ]))  
Rok = c(nrow(rok1987),nrow(rok1988),nrow(rok1989),nrow(rok1990),nrow(rok1991),nrow(rok1992),nrow(rok1993),nrow(rok1994),nrow(rok1995),nrow(rok1996),nrow(rok1997),nrow(rok1998),nrow(rok1999),nrow(rok2000),nrow(rok2001),nrow(rok2002),nrow(rok2003),nrow(rok2004),nrow(rok2005),nrow(rok2006),nrow(rok2007),nrow(rok2008)) 

a <- data.frame( rok = 1987:2008, procent = (odwloane/Rok)*100)

wykres <- ggplot(a, aes(x = rok, y = procent)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Rok", y = "Udział") +
  ggtitle("Procent odwołanych lotów w danym roku") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(1987, 2008, 1))

print(wykres)
