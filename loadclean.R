# Draft script

# leer datos
library(ggplot2)
Sys.setlocale("LC_TIME", "C")
stormdata <- read.csv(bzfile('repdata-data-StormData.csv.bz2'),
                      stringsAsFactors = F)

# extraer columnas.
# fecha. estado. evtype. fatalities. injuries. propdmg cropdmg
todollars <- function(ammount, expfactor) {
    if (expfactor == "K") { ammount * 1000 }
    else if (expfactor == "M") { ammount * 1000000 }
    else if (expfactor == "B") { ammount * 1000000000 }
    else { ammount }
}

storms <- data.frame(as.Date(stormdata$BGN_DATE, '%m/%d/%Y'),
                     stormdata$STATE,
                     stormdata$EVTYPE,
                     stormdata$FATALITIES,
                     stormdata$INJURIES,
                     mapply(todollars, stormdata$PROPDMG, stormdata$PROPDMGEXP),
                     mapply(todollars, stormdata$CROPDMG, stormdata$CROPDMGEXP),
                     stringsAsFactors=F
)

names(storms) <- c("date","state","evtype",
                   "fatalities","injuries","propdmg","cropdmg")


# ver N/A. filtrar fechas si hace falta.
# desde 1996 segun http://www.ncdc.noaa.gov/stormevents/details.jsp
# nov 95

storms96 <- storms[which(as.numeric(format(storms$date, "%Y%m")) >= 199601 &
                             !grepl("^summary",ignore.case=T,storms$evtype)),]


storms96$evtype <- tolower(storms96$evtype)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
storms96$evtype <- trim(storms96$evtype)

storms96$evtype[storms96$evtype == "hurricane edouard"] <- "hurricane"
storms96$evtype[storms96$evtype == "hurricane/typhoon"] <- "hurricane"
storms96$evtype[storms96$evtype == "typhoon"] <- "hurricane"
storms96$evtype <- gsub("tstm.*","thunderstorm", storms96$evtype)
storms96$evtype[storms96$evtype == "thunderstorms"] <- "thunderstorm"
storms96$evtype[storms96$evtype == "thunderstorm wind"] <- "thunderstorm"
storms96$evtype[storms96$evtype == "thunderstorm wind (g40)"] <- "thunderstorm"
storms96$evtype <- gsub("ice.*","ice", storms96$evtype)
storms96$evtype <- gsub("icy.*","ice", storms96$evtype)
storms96$evtype <- gsub("flood.*","flood", storms96$evtype)
storms96$evtype <- gsub("flash flood.*","flash flood", storms96$evtype)
storms96$evtype <- gsub("  "," ", storms96$evtype)

storms96$evtype <- as.factor(storms96$evtype)

storms96$damages <- storms96$propdmg + storms96$cropdmg

# convertir a dolares 2011.



