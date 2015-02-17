# analysis

# load packages
# library(ggplot2)
# library("FField")
# library(magrittr)

# to ~ arbeitslosigkeit
mToArbeit <- lm(to ~ arbeitslosigkeit, d)
# summary(mToArbeit)

# lm(to ~ sgb2, d) %>% summary
# lm(to ~ einkommen, d) %>% summary

# ggplot(d, aes(x = einkommen, y = to)) +
#   geom_smooth(method=lm, fill = 'cornsilk3', color = 'firebrick4') +
#   geom_point(size = 3, color = 'gray23') +
#   geom_text(aes(label=wbezirk), vjust=-.5, size = 2.5) +
#   theme_bw() + xlab('Einkommen') + ylab('Wahlbeteiligung %')

coords1 <- FFieldPtRep(coords = cbind(d$arbeitslosigkeit, d$to), rep.fact = 2, adj.lmt = 5)

fToArbeit <-  ggplot(d, aes(x = arbeitslosigkeit, y = to)) +
  geom_smooth(method=lm, fill = 'cornsilk3', color = 'firebrick4') +
  geom_point(size = 3, color = 'gray23') +
  geom_text(aes(x = coords1$x, y = coords1$y, label=wbezirk), vjust=-.5, size = 2.5) +
  theme_bw() + xlab('Arbeitslosigkeit %') + ylab('Wahlbeteiligung %') +
  theme(axis.text = element_text(size = .5))

# to-to11 ~ arbeitslosigkeit

mDifToArbeit <- lm(to-to11 ~ arbeitslosigkeit, d)

fDifToArbeit <- ggplot(d, aes(x = arbeitslosigkeit, y = (to-to11))) +
  geom_smooth(method=lm, fill = 'cornsilk3', color = 'firebrick4') +
  geom_point(size = 3, color = 'gray23') +
  geom_text(aes(label=wbezirk), vjust=-.5, size = 2.5) + theme_bw() +
  ylab('Wahlbeteiligung 2015-2011 %-Punkte') + xlab('Arbeitslosigkeit %')

# AfD - auslaender_innen
mAfdAuslander <- lm(afd ~ auslaender_innen, d)

coords2 <- FFieldPtRep(coords = cbind(d$auslaender_innen, d$afd),
                      rep.fact = 1, adj.lmt = 4)

fAfdAuslander <- ggplot(d, aes(x = auslaender_innen, y = afd)) +
  geom_smooth(method=lm, fill = 'cornsilk3', color = 'firebrick4') +
  geom_point(size = 3, color = 'gray23') +
  geom_text(aes(x = coords2$x, y = coords2$y, label=d$wbezirk), vjust=-.5, size = 2.5) +
  theme_bw() +  ylab('AfD %') + xlab('Ausländeranteil %') +
  theme(axis.text.x = element_text(size = 2))

# AfD - arbeitslosigkeit
mAfdArbeit <- lm(afd ~ arbeitslosigkeit, d)

coords3 <- FFieldPtRep(coords = cbind(d$arbeitslosigkeit, d$afd),
                      rep.fact = 1, adj.lmt = 2)

fAfdArbeit <- ggplot(d, aes(x = arbeitslosigkeit, y = afd)) +
  geom_smooth(method=lm, fill = 'cornsilk3', color = 'firebrick4') +
  geom_point(size = 3, color = 'gray23') +
  geom_text(aes(x = coords3$x, y = coords3$y, label=wbezirk), vjust=-.5, size = 2.5) +
  theme_bw() +  ylab('AfD %') + xlab('Arbeitslosigkeit %')

# AfD - auslander + arbeitslosigkeit
mAfdAuslanderArbeit <- lm(afd ~ auslaender_innen + arbeitslosigkeit, d)

tores <- as.numeric(resid(lm(afd ~ arbeitslosigkeit, d)))
ausres <- as.numeric(resid(lm(auslaender_innen ~ arbeitslosigkeit, d)))

coords4 <- FFieldPtRep(coords = cbind(ausres, tores),
                       rep.fact = 1, adj.lmt = 2)

fAfdAuslanderArbeit <- ggplot(NULL, aes(x = ausres, y = tores)) + geom_point(size = 3, color = 'gray23') +
  geom_smooth(method = lm, se = F, color = 'firebrick4') +
  xlab('Ausländeranteil | Arbeitslosigkeit') +
  ylab('AfD | Arbeitslosigkeit') +
  geom_text(aes(x = coords4$x, y = coords4$y, label = d$wbezirk),
            vjust=-.5, size = 2.5) + theme_bw()

# AfD ~ migrant_innen

# lm(afd ~ migrant_innen, d) %>% summary
# lm(afd ~ migrant_innen + arbeitslosigkeit, d) %>% summary

