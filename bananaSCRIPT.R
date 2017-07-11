## prepare workspace
cat("\014") # clear console
setwd("C:\\Users\\mkaranja\\Desktop\\bananaTool")
rm(list=ls(all=T)) # remove r objects

library(ona)
## Get data from ona 
mybanana = onaDownload("20170622","seedtracker","megy","megy")
banana = as.data.frame(mybanana)
banana.raw = write.csv(banana, file = "raw.banana.csv")

library(dplyr)
library(tidyr)
library(stringr)
library(httr)
library(jsonlite)
library(RCurl)

# Import data
banana = read.csv("raw.banana.csv")

## Organize data: tidyr, dplyr packages
flowering = select(banana, contains("flowering"))
fID = select(flowering, ends_with("plantid"))
fIDs = gather(fID, flowering, flowerID, na.rm = TRUE)
flowers = fIDs
colnames(flowers) = c("flowered","flower")
fSex = select(flowering, ends_with("sex"))
fSexs = gather(fSex, fSex, sex, na.rm = TRUE)
fDate = select(flowering, ends_with("date"))
fDate1 = gather(fDate, fDates1, floweringDate, na.rm = TRUE)
floweringdf1 = as.data.frame(c(fIDs, flowers, fSexs, fDate1)) 
floweringdata1 = select(floweringdf1, flowerID, flower, sex, floweringDate)
fDate2 = gather(fDate, fDates2, date, na.rm = TRUE)
nflower = strrep("flowering", 1)
floweringdf2 = as.data.frame(c(nflower, fIDs, flowers, fSexs, fDate2)) 
floweringdata2 = select(floweringdf2, X.flowering., flowerID, flower, sex, date)
colnames(floweringdata2) = c("activity","flowerID", "flower", "sex", "date")
# Save flowering data 
flowering.df1 = write.csv(floweringdata1, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool_final\\flowering1.csv", row.names=F)# for merging
flowering.df2 = write.csv(floweringdata2, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool_final\\flowering2.csv", row.names=F)# for grouping datasets

# First pollination
firstpolln = select(banana, contains("firstpollination"))
#cross parents
crosses = select(firstpolln, ends_with("parent"))
parent = gather(crosses, activity, parents, na.rm = TRUE)
parents = str_split_fixed(parent$parents, "/", 2)
colnames(parents) = c("mother","father")
parents = as.data.frame(parents)

#cross id and polln date
crossnumber = select(firstpolln, ends_with("cross_id"))
crossnumbers = gather(crossnumber, firstcrossing, crossnumber, na.rm = TRUE)

firstpollinationdate = select(firstpolln, ends_with("date"))
firstpollndate1 = gather(firstpollinationdate, activit1stpolln, firstPollinationDate, na.rm = TRUE)
firstpollndate2 = gather(firstpollinationdate, activit1stpolln, date, na.rm = TRUE)

firstpollndf1 = as.data.frame(c(parents,crossnumbers, firstpollndate1))
firstpollndf2 = as.data.frame(c(parents,crossnumbers, firstpollndate2))
first1 = select(firstpollndf1, mother, father, crossnumber, firstPollinationDate)
first2 = select(firstpollndf2, mother, father, crossnumber, date)
npolln = strrep("first pollination", 1)
firstpolln.df2 = as.data.frame(c(npolln, first2))
colnames(firstpolln.df2) = c("activity","mother", "father", "crossnumber", "date")

firstdf1 = write.csv(first1, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool_final\\firstpollination1.csv", row.names = F) # merge
firstdf2 = write.csv(firstpolln.df2, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool_final\\firstpollination2.csv", row.names = F) # dataset

#REPEAT POLLINATION
rptpollination = select(banana, contains("repeatpollination"))
rptmother = select(rptpollination, contains("rpt_mother_id"))
rptfather = select(rptpollination, contains("repeatfather_barcode"))
rpt_date = select(rptpollination, contains("repeatpollination_date"))

mother = gather(rptmother, rptmother, mother, na.rm = T)
father = gather(rptfather, rptfather, father, na.rm = T)
repeatpollinationdate = gather(rpt_date, rptdate, date, na.rm = T)

rptpolln = as.data.frame(c(mother, father, repeatpollinationdate))
repeatpollinationdf = select(rptpolln, mother, father, date)
rprpollinationdat = arrange(repeatpollinationdf, desc(date))
#pollination = merge(x = firstpollination, y = repeatpollinationdf, by = c("mother", "father"), all.x = T)
repeatpolln = write.csv(repeatpollinationdat, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\repeatpollination.csv", row.names = F)


# HARVESTING
harvest = select(banana, contains("harvest"))
harvestid = select(harvest, contains("bunch_id"))
bunchid = gather(harvestid, id, crossnumber, na.rm = T)
harvestdate = select(harvest, contains("date"))
date = gather(harvestdate, date, harvesting_date, na.rm = T)
ripingshed = select(harvest, contains("shed"))
shed = gather(ripingshed, shed, ripeningshed, na.rm = T)
harvested = as.data.frame(c(bunchid, date, shed))
harvestingdf = select(harvested, crossnumber, harvesting_date, ripeningshed)
harvest = write.csv(harvestingdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\harvesting.csv", row.names = F)

# RIPENING
ripen = select(banana, contains("ripening"))
ripenid = select(ripen, contains("ripened_id"))
id = gather(ripenid, ids, crossnumber, na.rm  = T)
ripendate = select(ripen, contains("date"))
date = gather(ripendate, date, ripen_date, na.rm = T)
ripened = as.data.frame(c(id, date))
ripeningdf = select(ripened, crossnumber, ripen_date)
ripening = write.csv(ripeningdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\ripening.csv", row.names = F) 

# STATUS - can be any plant - SAVED IN A SEPARATE FILE
status = select(banana, contains("plantstatus"))
statusid = select(status, contains("plant_id"))
plantstatusid = gather(statusid, status, crossnumber, na.rm = T)
statusdate = select(status, contains("status_date"))
plantstatusdate = gather(statusdate, date, plantstatus_date, na.rm = T)
statustype = select(status, contains("status"))
plantstatustype = gather(statustype, type, plant_status, na.rm = T)
statuscomment = select(status, end("comment"))
comments = gather(statuscomment, status, status_comments, na.rm = T) 
plantstatus = as.data.frame(c(plantstatusid, plantstatusdate, plantstatustype, comments))
plantstatusdf = select(plantstatus, crossnumber, plantstatus_date, plant_status, status_comments)
pstatus = write.csv(plantstatusdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\plantstatus.csv", row.names = F)

## LAB
# EXTRACTION
extracts = select(banana, contains("seed_extraction"))
extractid = select(extracts, contains("extraction_id"))
extractedid = gather(extractid, extract, crossnumber, na.rm = T)
extractdate = select(extracts, contains("extraction_date"))
extracteddate = gather(extractdate, date, seed_extraction_date, na.rm = T)
total = select(extracts, contains("seeds_extracted"))
totalseeds = gather(total, total, number_seeds, na.rm = T)
good = select(extracts, contains("good_seeds"))
goodseeds = gather(good, good, good_seeds, na.rm = T)
bad = select(extracts, contains("bad_seeds"))
badseeds = gather(bad, bad, bad_seeds, na.rm = T)
extracted = as.data.frame(c(extractedid, extracteddate, totalseeds, goodseeds, badseeds))
extractiondf = select(extracted, crossnumber, seed_extraction_date, number_seeds, good_seeds, bad_seeds)
extractd = write.csv(extractiondf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\extraction.csv", row.names = F)

# RESCUE
rescue = select(banana, contains("rescue"))
rescueid = select(rescue, contains("embryorescue_id"))
embryorescueid = gather(rescueid, id, crossnumber, na.rm = T)
rescueseeds = select(rescue, contains("embryorescue_seeds"))
embryorescueseeds = gather(rescueseeds, seeds, number_rescued, na.rm = T)
rescuedate = select(rescue, contains("embryorescue_date"))
embryorescuedate = gather(rescuedate, id, rescue_date, na.rm = T)
rescued = as.data.frame(c(embryorescueid,embryorescuedate, embryorescueseeds))
rescuingdf = select(rescued, crossnumber, number_rescued, rescue_date)
rescueed = write.csv(rescuingdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\rescued.csv", row.names = F)

# 2 WEEKS GERMINATION
twoweeks = select(banana, contains("germinatn_after_2wks"))
twowksid = select(twoweeks, ends_with("2wks_id"))
germinating2wksid = gather(twowksid, germinating, crossnumber, na.rm = T)
twowksdate = select(twoweeks, ends_with("2wks_date"))
germinating2wksdate = gather(twowksdate, date, germination_after_2weeks_date, na.rm = T)
active2wks = select(twoweeks, contains("actively_2wks"))
actively = gather(active2wks, active, actively_germination_after_two_weeks, na.rm = T)
germination2weeks = as.data.frame(c(germinating2wksid, germinating2wksdate, actively))
germination2weeksdf = select(germination2weeks, crossnumber, germination_after_2weeks_date, actively_germination_after_two_weeks)

germ2wks = write.csv(germination2weeksdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\germinating2weeks.csv", row.names = F)
# 1 MONTH GERMINATION
onemonth = select(banana, contains("germinatn_after_1month"))
onemonthid = select(onemonth, contains("1month_id"))
germinating1monthid = gather(onemonthid, germinating, crossnumber, na.rm = T)
twowksdate = select(onemonth, contains("1month_date"))
germinating1monthdate = gather(twowksdate, date, germination_after_1month_date, na.rm = T)
active1month = select(onemonth, contains("actively_1month"))
actively = gather(active1month, active, actively_germination_after_one_month, na.rm = T)
germination1month = as.data.frame(c(germinating1monthid, germinating1monthdate, actively))
germination1monthdf = select(germination1month, crossnumber, germination_after_1month_date, actively_germination_after_one_month)
germ1month = write.csv(germination1monthdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\germinating1month.csv", row.names = F)

# SUBCULTURE
subculture = select(banana, contains("subcltre"))
subcultureid = select(subculture, ends_with("subculture_id"))
subids = gather(subcultureid, ids, crossnumber, na.rm = T)
subcdate = select(subculture, ends_with("subculture_date"))
subdate = gather(subcdate, date, subculture_date, na.rm = T)
subcultures = select(subculture, contains("subcultures_number"))
subnumbers = gather(subcultures, date, plantlets_number, na.rm = T)
subculturin = as.data.frame(c(subids, subnumbers, subdate))
subculturingdf = select(subculturin, crossnumber, subculture_date,plantlets_number)

subdata = write.csv(subculturingdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\subculture.csv", row.names = F)
# ROOTING
root = select(banana, contains("rooting"))
rootid = select(root, ends_with("rooting_id"))
rootingid = gather(rootid, id, crossnumber, na.rm = T)
rootdate = select(root, ends_with("date"))
rootingdate = gather(rootdate, date, date_rooting, na.rm = T)
rootin = as.data.frame(c(rootingid, rootingdate))
rootingdf = select(rootin, crossnumber, date_rooting)
rooted = write.csv(rootingdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\rooting.csv", row.names = F)

# SCREEN HOUSE
screenhouse = select(banana, contains("screenhouse"))
transferscrnhseid = select(banana, contains("screenhse_plantid"))
transferredcross = gather(transferscrnhseid, id, crossnumber, na.rm = T)
transferdate = select(screenhouse, contains("screenhse_transfer_date"))
transferreddate = gather(transferdate, date, date_of_transfer_to_screenhse, na.rm = T)
transferscreenhse = as.data.frame(c(transferredcross, transferreddate))
screenhsedf = select(transferscreenhse, crossnumber, date_of_transfer_to_screenhse)      #transfer to screenhsedf
screenhse = write.csv(screenhsedf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\screenhouse.csv", row.names = F)

harden = select(banana, contains("hardening"))
hardenedid = select(harden, contains("hardening"))
hardenedcrossnumber = gather(hardenedid, id, crossnumber, na.rm = T)
hardendate = select(harden, contains("harden_date"))
hardeningdate = gather(hardendate,date, date_recording_hardening, na.rm = T)
hardening = as.data.frame(c(hardenedcrossnumber, hardeningdate))
hardeningdf = select(hardening, crossnumber, date_recording_hardening)  ## hardening
hardenned = write.csv(hardeningdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\hardening.csv", row.names = F)

openfld = select(banana, contains("openfield"))
openfield = select(openfld, contains("openfield_plantid"))
openfieldid = gather(openfield, id, crossnumber, na.rm = T)
opendate = select(openfld, contains("openfield.trasplanting_date"))
openfielddate = gather(opendate, date, date_of_transfer_to_openfield, na.rm = T)
openfieldtransfer = as.data.frame(c(openfieldid, openfielddate))
openfieldtransferdf = select(openfieldtransfer, crossnumber,date_of_transfer_to_openfield) #to open field
openfd = write.csv(openfieldtransferdf, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\openfield.csv", row.names = F)

### ALL DATA
allbanana = list(pollination, harvestingdf, ripeningdf, extractiondf, rescuingdf, germination2weeksdf,germination1monthdf, subculturingdf, rootingdf, screenhsedf, hardeningdf, openfieldtransferdf)                   
bananadat = Reduce(function(x,y) merge(x,y, all = T, by= "crossnumber"), allbanana)
#bananadat$SNo <- seq.int(nrow(bananadat))
bananadf = select(bananadat, crossnumber, everything())
bananadata = bananadf
bananadata1 = write.csv(bananadata, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\bananadata.csv", row.names = F)

## flowering and first pollination datasets
ffp = c(floweringdata2,firstpolln.df2)
write.csv(ffp, file = "C:\\Users\\mkaranja\\Desktop\\bananaTool_final\\dataset.csv", row.names=F")

# Auth Tokens
raw.result <- GET("https://api.ona.io/api/v1/user.json", authenticate(user = "seedtracker",password = "Seedtracking101"))
raw.result.char<-rawToChar(raw.result$content)
raw.result.json<-fromJSON(raw.result.char)
TOKEN_KEY <- raw.result.json$temp_token

#UPLOAD FLOWERING DATA
header=c(Authorization=paste("Temptoken ", TOKEN_KEY), `Content-Type` = 'multipart/form-data')
post.flower.results <- postForm("https://api.ona.io/api/v1/metadata.json",
                          data_value='flowering.csv',data_type='media',xform=215418,
                          data_file=fileUpload(filename = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\flowering.csv",contentType = 'text/csv'),
                         .opts=list(httpheader=header), verbose = TRUE)
media_id = substr(post.flower.results, 7, 12) # extracts the media file id
#POST ALL DATA
header=c(Authorization=paste("Temptoken ",TOKEN_KEY), `Content-Type` = 'multipart/form-data')
post.data.result <- postForm("https://api.ona.io/api/v1/metadata.json",
                         data_value='firstpollination.csv',data_type='media',xform=215418,
                         data_file=fileUpload(filename = "C:\\Users\\mkaranja\\Desktop\\bananaTool\\app\\bananadata.csv",contentType = 'text/csv'),
                         .opts=list(httpheader=header), verbose = TRUE)

#raw.data.result <- GET("https://api.ona.io/api/v1/user.json", authenticate(user = "seedtracker",password = "Seedtracking101"))
raw.data.char<-rawToChar(post.data.results$id)
raw.data.json<-fromJSON(raw.data.char)

# DELETE CSV FILES
hdr=c(Authorization=paste("Temptoken ",TOKEN_KEY))
     DELETE("https://api.ona.io/api/v1/metadata/ ",id1,add_headers(Authorization=paste("Temptoken ",TOKEN_KEY)))

#hdr=c(Authorization=paste("Temptoken ",TOKEN_KEY))
 #       DELETE("https://api.ona.io/api/v1/metadata/521557",add_headers(Authorization=paste("Temptoken ",TOKEN_KEY)))
#header=c(Authorization=paste("Temptoken ",TOKEN_KEY))
 # GET("https://api.ona.io/api/v1/metadata/530173",add_headers(Authorization=paste("Temptoken ",TOKEN_KEY)))

#y = onaDownload(20170602, "seedtracker")

substring(post.flower.results, seq(1))
nchar(post.flower.results)

sst <- strsplit(post.flower.results, "")[[1]]
seq(0, 1, length.out = 11)
seq(stats::rnorm(20)) # effectively 'along'
seq(1, 9, by = 2)     # matches 'end'
seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by = 0.05)
seq(17) # same as 1:17, or even better seq_len(17)