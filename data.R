#setwd
# setwd('/home/arndt/Dropbox/01 PhD/02 projects/hhwahl2015')
# load('data/data.RData')

# load packages
library(gdata)

#
# download data
#

# URL to Wahlstrukturdaten
URL <- paste0('http://www.statistik-nord.de/fileadmin/Dokumente/Wahlen/Hamburg/',
              'B%C3%BCrgerschaftswahlen/2015/Vor_der_Wahl/',
              'Bue2015_WahlstrukturdatenTransponiert.xls')

#
# download and format election returns
#
# The voting data were manually downloaded as an interactive form needs to be
# used to download the data from
# http://wahlen-hamburg.de/wahlen.php?site=left/export&wahl=973

w <- read.delim('data/export_973_1_1_0_8_2_23.csv', sep = ';')

w <- w[,c(1,2, 6, 9, 12:24)]

names(w) <- tolower(names(w))
names(w)[1:4] <- c('wbezirk', 'wahlberechtigte', 'waehler', 'gueltigestimmzettel')

w[,2:ncol(w)] <- apply(w[,2:ncol(w)], 2, as.numeric)

w$to <- round((w$waehler / w$wahlberechtigte) * 100,1)  # turnout in pct

w2 <- data.frame(to = round((sum(w$waehler) / sum(w$wahlberechtigte)) * 100, 1),
                 afd =  round((sum(w$afd) / sum(w$gueltigestimmzettel)) * 100, 1))

w[,4:(ncol(w)-1)] <-apply(w[,4:(ncol(w)-1)], 2, function(x)
  round((x / w$gueltigestimmzettel) * 100, 1)) # party vote shares in pct



#
# download and format Wahlstrukturdaten
#

# donwload data
s <- read.xls(URL, sheet = 1, pattern = 'Merkmal') # read data from source

# sbackup <-  s
# s <- sbackup
s <- s[,-ncol(s)] # drop empty last col
s <- s[-c(1, nrow(s)),]  #  drop empty first and last row
s[2,2:ncol(s)] <- s[1,2:ncol(s)]
s <- s[-1,]
s <- s[which(s$Wahlkreis != ''),]

# transpose data
s <- t(as.matrix(s)) # transpose data
s <- data.frame(s)


# subset to essential variables
s <- s[-1, c('X4', 'X6', 'X10', 'X12', 'X29', 'X31', 'X37', 'X42',
                        'X61')]

# final formattting
s <- data.frame(apply(s, 2, as.character))
s <- data.frame(apply(s, 2,  function(x) gsub(' ', '', x)))
s <- data.frame(apply(s, 2, as.numeric))
names(s) <- c('bevoelkerung', 'u18', 'auslaender_innen', 'migrant_innen' ,
              'beschaeftigte', 'arbeitslosigkeit', 'sgb2', 'einkommen',
              'to11')

s2 <- s[nrow(s),]
s <- s[-nrow(s),]

# merge electoral returns and 'Wahlstrukturdaten'
d <- cbind(w,s)

# create table of data for article

t <- d[,c('wbezirk', 'to', 'to11', 'afd', 'arbeitslosigkeit', 'auslaender_innen')]
t <- rbind(t, data.frame(wbezirk = 'Hamburg', to = w2$to, to11 = s2$to11, afd = w2$afd,
                         arbeitslosigkeit = s2$arbeitslosigkeit,
                         auslaender_innen = s2$auslaender_innen))
names(t) <- c('Wahlbezirk', 'Wahlbeteiligung', 'Wahlb. 2011', 'AfD', 'Arbeitslosigkeit', 'Ausländeranteil')


# save data
save.image('data/data.RData')
