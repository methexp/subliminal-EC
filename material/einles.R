#Import data from different txt or dat files
einles <- function(expname, pfad, skips = 0, totalrows = 0, nrows = -1, vars, filename = "", sepr= "", filetype = ".dat")
{
  if(filename=="") {filename = "all"}
  ## Dateinamen und VP-Nr
  files <- list.files(path = pfad, pattern = filetype, ignore.case=TRUE)
  prefix <- paste(expname, "_", filename, "_", sep="")
  if(filetype==".dat"){
    vpnr <- gsub(paste(prefix, "|", filetype, sep = ""), "", x=files)
  } else {
    vpnr <- gsub(".dat", "", x=files)
  }
  ## ab Zeile 'skips+1' einlesen
  ## insgesamt 'nrows' Zeilen
  ## Variablennamen im Header
  split <- ifelse(sepr=="", " ", sepr)
  h <- strsplit(vars, split= split, fixed=T)
  ## create empty data frame
  d <- as.data.frame(matrix(nrow=0, ncol=length(h[[1]])))
  # loop over all files
  for(i in 1:length(files)){
    # einlesen
    file <- readLines(paste(pfad, files[i], sep=""))
    if(length(file) == totalrows){
    d_ <- read.table(file=paste(pfad, files[i], sep=""), header=FALSE, sep= sepr, skip=skips, nrows=nrows, col.names=h[[1]])
    # vpnr setzen
    d_$vpnr=vpnr[i]
    #
    d <- rbind(d, d_)
    }
  }
  # save data
  write.table(d, file=paste("data/", filename, "_", expname, ".dat", sep=""))
  #write.csv(d, file=paste("data/", filename, "_", expname, ".csv", sep=""))
  #save(d, file=paste("data/", filename, "_", expname, ".RDa", sep=""))
}


#Descriptives: How many participants and stimuli?
N.and.CS <- function(vpnr, cs){
  participants <<- unique(vpnr)
  N_participants <<- length(participants)
  N_participants
  
 if(is.character(cs)){
   CS <<- unique(cs)
 } else{
   CS <<- unique(as.character(cs))
 }
   
  nCS <<- length(CS) 
}

einles_6 <- function(){
  #####################If you want to import raw data:######################################
  ## combine individual files ###
  
  ##load memdata
  #pfad = paste("/mnt/nas/labor/daten/", experiment, "/memdata/", sep="")
#   pfad = paste("l:/daten/", experiment, "/memdata/", sep="")
#   varnames <- "subject_nr,cs_filename_usmem,valmem_response_angenehm,valmem_response_unangenehm,valmem_response_neu,usmem_option_1,usmem_option_2,usmem_option_3,usmem_option_4,usmem_option_5,usmem_option_6,usmem_resp1,usmem_resp2,usmem_resp3,usmem_resp4,usmem_resp5,usmem_resp6,time_valmem_form,time_valmem_delay,time_usmem_form,time_usmem_delay"
#   einles(expname = experiment
#          , pfad = pfad
#          , skips = 1
#          , nrows = 30
#          , vars = varnames
#          , totalrows = 31
#          , filename= "memdata"
#          , sepr = ","
#          )
#    
  memdata <- read.table(file=paste("data/", "memdata_", experiment, ".dat", sep=""), header=T)
  
  ##load ratingdata
  # pfad = paste("/mnt/nas/labor/daten/", experiment, "/ratingdata/", sep="")
#   pfad = paste("l:/daten/", experiment, "/ratingdata/", sep="")
#   varnames <- "subject_nr,cs_filename_rating,eval_rating,time_ratings_form,time_ratings_delay"
#   einles(expname = experiment
#          , pfad = pfad
#          , skips = 1
#          , nrows = 30
#          , vars = varnames
#          , totalrows = 31
#          , filename = "ratingdata"
#          , sepr=",")
  
  ratingdata <- read.table(file=paste("data/", "ratingdata_", experiment, ".dat", sep=""), header=T)
  
  ##load trialdata
  # pfad = paste("/mnt/nas/labor/daten/", experiment, "/trialdata/", sep="")
#   pfad = paste("l:/daten/", experiment, "/trialdata/", sep="")
#   varnames <- "subject_nr,blocknr,trialnr,cs_filename,usval,cs_duration,us_filename,us_duration,premask_filename,premask_duration,postmask_filename,postmask_duration,vis_option_6,vis_option_5,vis_option_4,vis_option_3,vis_option_2,vis_option_1,vis_option_6_resp,vis_option_5_resp,vis_option_4_resp,vis_option_3_resp,vis_option_2_resp,vis_option_1_resp,time_gesehen_form,time_trial_logger"
#   einles(expname = experiment
#          , pfad = pfad
#          , skips = 1
#          , nrows = 336
#          , vars = varnames
#          , totalrows = 337
#          , filename = "trialdata"
#          , sepr=",")
  
  trialdata <- read.table(file=paste("data/", "trialdata_", experiment, ".dat", sep=""), header=T)
  
  ##load triallists
  # pfad = paste("/mnt/nas/labor/daten/", experiment, "/triallist/", sep="")
#   pfad = paste("l:/daten/", experiment, "/triallist/", sep="")
#   varnames <- "trialnr,blocknr,cs_filename,usval,cs_duration,us_filename,us_duration,premask_filename,premask_duration,postmask_filename,postmask_duration,vis_option_6,vis_option_5,vis_option_4,vis_option_3,vis_option_2,vis_option_1"
#   einles(expname = experiment
#          , pfad = pfad
#          , skips = 1
#          , nrows = 336
#          , vars = varnames
#          , totalrows = 337
#          , filename = "trialliste"
#          , sepr=","
#          , filetype = ".txt")
  
  triallist <- read.table(file=paste("data/", "trialliste_", experiment, ".dat", sep=""), header=T)
  
  
  # compute necessary variables, save as new files without the rest
  
  #memdata: 
  ##RT for valmem and idmem
  memdata$valmem_RT = memdata$time_valmem_delay - memdata$time_valmem_form
  memdata$usmem_RT = memdata$time_usmem_delay - memdata$time_usmem_form
  
  ##chosen valence?
  memdata$valmem_response <- apply(memdata,1,function(x) c("pleasant","unpleasant","new")[x[c("valmem_response_angenehm","valmem_response_unangenehm","valmem_response_neu")] == "yes"])
  
  ##chosen US?
  memdata$usmem_response <- apply(memdata,1,function(x) x[c("usmem_option_1","usmem_option_2","usmem_option_3","usmem_option_4","usmem_option_5","usmem_option_6")][x[c("usmem_resp1","usmem_resp2","usmem_resp3","usmem_resp4","usmem_resp5","usmem_resp6")] == "yes"])
  
  ##save relevant variables
  memdata_short <- memdata[,c("subject_nr","cs_filename_usmem","valmem_RT","usmem_RT","valmem_response","usmem_response")]
  #write.csv(memdata_short, file=paste("data/", experiment, "_memdata_short.csv", sep=""), row.names=FALSE)
  
  #ratingdata
  ratingdata$rating_RT = ratingdata$time_ratings_delay - ratingdata$time_ratings_form
  
  ##save relevant variables
  ratingdata_short <- ratingdata[,c("subject_nr","cs_filename_rating","rating_RT","eval_rating")]
  #write.csv(ratingdata_short, file=paste("data/", experiment, "_ratingdata_short.csv", sep=""), row.names=FALSE)
  
  #trialdata
  names(trialdata)[3] <- "trialnr_data"
  trialdata$vis_RT = trialdata$time_trial_logger - trialdata$time_gesehen_form
  
  ##visibility: chosen image
  trialdata$vis_response <- apply(trialdata,1,function(x) x[c("vis_option_6","vis_option_5","vis_option_4","vis_option_3","vis_option_2","vis_option_1")][x[c("vis_option_6_resp","vis_option_5_resp","vis_option_4_resp","vis_option_3_resp","vis_option_2_resp","vis_option_1_resp")] == "yes"])
  
  ##save all variables
  #write.csv(trialdata, file=paste("data/", experiment, "_trialdata_all.csv", sep=""), row.names=FALSE)
  
  # combine all the data that are organized by subject & CS
  mem <- data.table(memdata_short, key=c("subject_nr","cs_filename_usmem"))
  rat <- data.table(ratingdata_short, key=c("subject_nr","cs_filename_rating"))
  csdat <- mem[rat]
  # then combine the data that are organized by subject,block,trial,etc.
  vis <- data.table(trialdata, key=c("subject_nr", "blocknr", "cs_filename", "us_filename", 
    "usval", "cs_duration", "us_duration",                    
    "vis_option_6", "vis_option_5", "vis_option_4", "vis_option_3", "vis_option_2", "vis_option_1",
    "premask_filename", "premask_duration", "postmask_filename", "postmask_duration"))
  vis$subject_nr <- as.factor(vis$subject_nr)
  tlist <- data.table(triallist, key=c("vpnr", "blocknr", "cs_filename", "us_filename", 
                                        "usval", "cs_duration", "us_duration",                    
                                        "vis_option_6", "vis_option_5", "vis_option_4", "vis_option_3", "vis_option_2", "vis_option_1",
                                        "premask_filename", "premask_duration", "postmask_filename", "postmask_duration"))
  d1 <- tlist[vis]
  alldata <- merge(x=as.data.frame(d1), y=as.data.frame(csdat), by.x=c("vpnr","cs_filename"), by.y=c("subject_nr", "cs_filename_usmem"))
  write.csv(alldata, file=paste("data/ecosub9_alldata.csv", sep=""), row.names=FALSE)
}
