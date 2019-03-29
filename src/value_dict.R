
df1 <- data.frame(term=unique(raw_dict$AC[which(!is.na(raw_dict$AC))]),value="AC",stringsAsFactors = F)
df2 <- data.frame(term=unique(raw_dict$BE[which(!is.na(raw_dict$BE))]),value="BE",stringsAsFactors = F)
df3 <- data.frame(term=unique(raw_dict$CO[which(!is.na(raw_dict$CO))]),value="CO",stringsAsFactors = F)
df4 <- data.frame(term=unique(raw_dict$HE[which(!is.na(raw_dict$HE))]),value="HE",stringsAsFactors = F)
df5 <- data.frame(term=unique(raw_dict$PO[which(!is.na(raw_dict$PO))]),value="PO",stringsAsFactors = F)
df6 <- data.frame(term=unique(raw_dict$SE[which(!is.na(raw_dict$SE))]),value="SE",stringsAsFactors = F)
df7 <- data.frame(term=unique(raw_dict$SD[which(!is.na(raw_dict$SD))]),value="SD",stringsAsFactors = F)
df8 <- data.frame(term=unique(raw_dict$ST[which(!is.na(raw_dict$ST))]),value="ST",stringsAsFactors = F)
df9 <- data.frame(term=unique(raw_dict$TR[which(!is.na(raw_dict$TR))]),value="TR",stringsAsFactors = F)
df10 <- data.frame(term=unique(raw_dict$UN[which(!is.na(raw_dict$UN))]),value="UN",stringsAsFactors = F)

val_dict <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10)

write.table(val_dict,"/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/resources/values.txt",sep = ";", col.names = F,row.names = F)


### prep

