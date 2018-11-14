library(data.table)

dat <- read_csv("updated_data_grab_combo.csv")
# get rid of dups 
dat <- unique(dat)
sum(duplicated(dat))
setDT(dat)
dat[, Month := gsub(" ", "", Month)]
dat[, Month := gsub("/", "-", Month)]
dat[, MY := as.yearmon(Month)]
dat[, Date := as.Date(MY)]
# dat[, Date := as.character(MY)]
dat[, Year:= format(MY, "%Y")]
setnames(dat, "Indicator Name", "ind")
setnames(dat, "Raw Value", "val")

# remove na and retired inds 
datsub <- dat[!grep("RETIRED", dat$ind, fixed = T), ][!is.na(val), ]
datsub[, c("Citywide Theme", "Late Flag") := NULL]
sum(duplicated(datsub))
datsub <- unique(datsub)
nrow(datsub)

# work with copy 
tempdt <- copy(datsub)

# make sure we are ordering each indicator by time 
setorder(tempdt,ind, MY)
tempdt[, Month := NULL]

# get rid of MMR early report vals for now - not sure how to handle - maybe we should exploit to predict or flag
tempdt <- tempdt[!Frequency %in% "PMMR/MMR", ]

# clean and derive freq N 
dupfreq <- tempdt[, .(length(unique(Frequency)), unique(Frequency)), by = .(ind, Agency)][V1>1, ind]
tempdt[ind %in% dupfreq, .(lngt = .N),  by = .(ind, Frequency)] # change these to monthly 
tempdt[ind %in% dupfreq, Frequency := "Monthly"]
tempdt[Frequency %in% "Annually", Freq.N := 1]
tempdt[Frequency %in% "Quarterly", Freq.N := 4]
tempdt[Frequency %in% "Monthly", Freq.N := 12]
tempdt[Frequency %in% "Bi-Annually", Freq.N := 2]


# How much data do we have for each period 
tempdt[, N := .N, by = c("ind", "Agency", "Frequency")]
tempdt[Frequency %in% "Quarterly", unique(N)]
tempdt[Freq.N == 1, unique(N)]
tempdt[Freq.N == 4, unique(N)]
tempdt[Freq.N == 2, unique(N)]
tempdt[Freq.N == 12, unique(N)]

# which indicators are used by multiple agencies?
tempdt[, length(unique(Agency)), by = "ind"][V1 > 1]

# to ensure that we have enough data to do a ts analysis, we need to set min N (24 is the min N for monthly data)
hist(tempdt$N)

# what's going on at the tail? 
cubsd <- tempdt[ind %in% tempdt[N>300, unique(ind)[1]], ]
cubsd[1:30, ] # some months have 2 vals reported - take the mean ####
tempdt[, val2 := mean(val), by = .(ind, Agency, MY)] 
tempdt[, val := NULL]

# make unique
tempdt <- unique(tempdt)

# redo the N 
tempdt[, N := .N, by = c("ind", "Agency")]

#check 
hist(tempdt$N) # reasonable

# work with this 
write.csv(tempdt, "cpr_clean.csv")
