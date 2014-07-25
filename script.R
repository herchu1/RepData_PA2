# Draft script

# leer datos
library(ggplot2)
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
                     as.factor(stormdata$EVTYPE),
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

storms96 <- storms[which(as.numeric(format(storms$date, "%Y%m")) >= 199512),]

# sumar variables
storms96$damages <- storms96$propdmg + storms96$cropdmg


# convertir a dolares 2011.

# agregado por tipo
fatlpertype <- aggregate(fatalities ~ evtype,
                    data=storms96[storms96$fatalities > 0,],
                    FUN=sum)

injrpertype <- aggregate(injuries ~ evtype,
                         data=storms96[storms96$injuries > 0,],
                         FUN=sum)

dmgspertype <- aggregate(damages ~ evtype,
                         data=storms96[storms96$damages > 0,],
                         FUN=sum)



# eventos sin victimas
round(sum(storms96$fatalities == 0 & storms96$fatalities == 0)
      / nrow(storms96) * 100, 1)

# muchas victimas
storms96[storms96$fatalities >= 50,]
# joplin tornado may22 2011  150.


# 10 worst.
worstfatl <- order(fatlpertype$fatalities, decreasing=T)
worstinjr <- order(injrpertype$injuries, decreasing=T)

# per year.
fatlpertype$fatalitiesperyear <- round(fatlpertype$fatalities / 6)
injrpertype$injuriesperyear <- round(injrpertype$injuries / 6)

head(fatlpertype[worstfatl, c("evtype","fatalitiesperyear")], n=10)
head(injrpertype[worstinjr, c("evtype","injuriesperyear")], n=10)


# damages

worstdmgs <- order(dmgspertype$damages, decreasing=T)
# millions per year
dmgspertype$damagesperyear <- round(dmgspertype$damages / 6000000)
head(dmgspertype[worstdmgs, c("evtype","damagesperyear")], n=10)


# per state
dmgspertypestate <- aggregate(damages ~ evtype + state,
                         data=storms96[storms96$damages > 0,],
                         FUN=sum)
maxdmgsperstate <- aggregate(damages ~ state,
                             data=dmgspertypestate, FUN=max)
worststate <- merge(maxdmgsperstate, dmgspertypestate)

# 12 events make >100M each state.
unique(worststate[worststate$damages > 100000000,3])



dmgspermonth <- aggregate(damages ~ evtype + format(date, "%m %Y"), data=storms96, FUN=sum)
names(dmgspermonth)[2] <- "when"

worstevents <- head(dmgspertype[worstdmgs, c("evtype")], n=5)
dmgspermonthworst5 <- dmgspermonth[dmgspermonth$evtype %in% worstevents,]
