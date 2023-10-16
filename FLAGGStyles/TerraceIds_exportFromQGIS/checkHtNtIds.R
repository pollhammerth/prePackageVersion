# this is a helper script, that checks, whether all projected terraces of ht and nt are listed in htIds and ntIds
# the latter are needed for filter expressions, to extract all required terraces of a unit
# please check, if collection includes all projected terraces, before plotting a new projection

#### check already done for:
# SyntheseEW_01
# FLAGG_Rhein
# FLAGG_Aare

####

# get the terrace ids not included in Syntheseis profile
# for HT
RhineHTs <- unique(data[which(data$mapped.NAME_KURZ == "02_HT"),][["mapped.terrace"]])
RhineHTs[which(RhineHTs %!in% htIds)]

# add them to the collection
htIds <- c(htIds, RhineHTs[which(RhineHTs %!in% htIds)] )

# save it
save(htIds, file = "htIds.Rdata")


# get the terrace ids not included in Syntheseis profile
# for NT
RhineNTs <- unique(data[which(data$mapped.NAME_KURZ == "01_NT"),][["mapped.terrace"]])
RhineNTs[which(RhineNTs %!in% ntIds)]

# add them to the collection
ntIds <- c(ntIds, RhineNTs[which(RhineNTs %!in% ntIds)] )

# save it
save(ntIds, file = "ntIds.Rdata")



