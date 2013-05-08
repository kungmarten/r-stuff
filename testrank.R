# Fetch data
library(RODBC)
sqlq <- "select iUserId, dCreated, iEventId, nvl(lead(iEventId, 1) over (partition by iUserId order by dCreated), 0) as NextEvent
from tblEventTracking
order by 1, 2"

conVertica <- odbcConnect("Vertica")
events <- sqlQuery(conVertica, sqlq)
odbcClose(conVertica)

events <- events[!names(events) == 'dCreated']
# Ugly fix to make "Exit" an absorbing state

events <- rbind(events, data.frame(iUserId = 0, iEventId = 0 , NextEvent = 0))

events$iEventId <- factor(events$iEventId, 
  levels = c(sort(unique(events$iEventId))),
  labels = c("Exit",
             "Login", 
             "ShowTown", 
			 "NavClick", 
			 "OpenSuite", 
			 "OpenNews", 
			 "OpenStarplaza", 
			 "OpenDressUp", 
			 "OpenCatwalk", 
			 "OpenGifts", 
			 "OpenBeautyParlor", 
			 "OpenAlbum", 
			 "OpenGames", 
			 "OpenSDesignFashion", 
			 "OpenSDesignInterior", 
			 "OpenSDesignJewelry", 
			 "OpenSDesignHair", 
			 #"OpenStylingStudio", 
			 "OpenDressupCat", 
			 "OpenAccount", 
			 "OpenMoreSD", 
			 "OpenMoreSC",
			 "SubOpenDressup",
			 "SubOpenGame",
			 #"SubOpenShopItems",
			 "SubOpenGiftsInter"))

events$NextEvent <- factor(events$NextEvent, 
  levels = c(sort(unique(events$NextEvent))),
  labels = c("Exit",
             "Login", 
             "ShowTown", 
			 "NavClick", 
			 "OpenSuite", 
			 "OpenNews", 
			 "OpenStarplaza", 
			 "OpenDressUp", 
			 "OpenCatwalk", 
			 "OpenGifts", 
			 "OpenBeautyParlor", 
			 "OpenAlbum", 
			 "OpenGames", 
			 "OpenSDesignFashion", 
			 "OpenSDesignInterior", 
			 "OpenSDesignJewelry", 
			 "OpenSDesignHair", 
			 #"OpenStylingStudio", 
			 "OpenDressupCat", 
			 "OpenAccount", 
			 "OpenMoreSD", 
			 "OpenMoreSC",
			 "SubOpenDressup",
			 "SubOpenGame",
			 #"SubOpenShopItems",
			 "SubOpenGiftsInter"))

trmat <- table(events$iEventId, events$NextEvent)

pagemat <- trmat/rowSums(trmat)
pagemat <- t(pagemat)

pagerank(pagemat, .85, .0001)

# Number of runs (one for each cell)
cc = length(trmat)

# let's measure the inactivity rank
rankpoint = 1

# Create empty matrix
inacmat <- zeros(size(trmat, 1), size(trmat, 2))

while (cc > 0) {
   nmat = trmat
   nmat[cc] = (trmat[cc] * 1.1)
   nmat = nmat/rowSums(nmat)
   nmat = t(nmat)
   prv = pagerank (nmat, 0.95, 0.0001);
   inacmat[cc] = prv[rankpoint];
   cc = cc - 1;
}