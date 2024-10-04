shinyServer(function(input,output,session){
  
  
  output$PolNo<-renderText({
    input$polno
  })
  
  output$InsuredName<-renderText({
    input$insname
  })
  
  output$KYCname<-renderText({
    input$kycname
  })
  
  trialdata<- reactive({
    req(input$polno)
    req(input$insname)
    req(input$kycname)
    
    matchdata<-data.frame(Policyno=input$polno,INSURED_NAME=input$insname,
                          KYC_NAME=input$kycname)
    matchdata$Insured_Name1<-toupper(matchdata$INSURED_NAME)
    matchdata$KYC_Name1<-toupper(matchdata$KYC_NAME)

    matchdata$JW_dist_upper<-stringdist(matchdata$Insured_Name1,matchdata$KYC_Name1,
                                        method = "jw")
    
    matchdata$Insured_Name2<-gsub("[^A-Za-z0-9 ]", " ", matchdata$Insured_Name1)
    matchdata$KYC_Name2<-gsub("[^A-Za-z0-9 ]", " ", matchdata$KYC_Name1)
    matchdata$Insured_Name2<-trimws(matchdata$Insured_Name2)
    matchdata$KYC_Name2<-trimws(matchdata$KYC_Name2)
    
    matchdata$JW_dist_upper_special<-stringdist(matchdata$Insured_Name2,
                                                matchdata$KYC_Name2,method = "jw")
    
    matchdata$Insured_Name3<-gsub("^MR|^MRS|^CDR|^MISS|^MS|^MR.|^NA|^MRS.|^SHRI|^MASTE|^SMT|^MIS|^MRSS|^LESSEE|^SH", "", matchdata$Insured_Name2)
    matchdata$KYC_Name3<-gsub("^MR|^MRS|^CDR|^MISS|^MS|^MR.|^NA|^MRS.|^SHRI|^MASTE|^SMT|^MIS|^MRSS|^LESSEE|^SH", "", matchdata$KYC_Name2)
    
    matchdata$Insured_Name3 <- gsub("\\s+", " ", matchdata$Insured_Name3)
    matchdata$KYC_Name3 <- gsub("\\s+", " ", matchdata$KYC_Name3)
    
    matchdata$ID<-row.names(matchdata)
    
    stop_words2<-subset(stop_words,word%in%stopwords())
    
    df_cleaned <- matchdata %>%
      unnest_tokens(word, Insured_Name3) %>%
      anti_join(stop_words2, by = "word") %>%
      group_by(ID) %>%
      summarize(cleaned_in = paste(word, collapse = " "))
    
    df_cleaned2 <- matchdata %>%
      unnest_tokens(word, KYC_Name3) %>%
      anti_join(stop_words2, by = "word") %>%
      group_by(ID) %>%
      summarize(cleaned_kn = paste(word, collapse = " "))
    
    matchdata<-merge(matchdata,df_cleaned,by="ID",all.x = T)
    matchdata<-merge(matchdata,df_cleaned2,by="ID",all.x = T)
    
    matchdata$Insured_Name3<-matchdata$cleaned_in
    matchdata$KYC_Name3<-matchdata$cleaned_kn
    matchdata$cleaned_kn<-NULL
    matchdata$cleaned_in<-NULL
    
    matchdata$Insured_Name3<-toupper(matchdata$Insured_Name3)
    matchdata$KYC_Name3<-toupper(matchdata$KYC_Name3)
    
    matchdata$Insured_Name3 <- sub(".*PROP\\s*", "", matchdata$Insured_Name3) 
    matchdata$KYC_Name3 <- sub(".*PROP\\s*", "", matchdata$KYC_Name3) 
    matchdata$Insured_Name3 <- sub(".*PRO\\s*", "", matchdata$Insured_Name3) 
    matchdata$KYC_Name3 <- sub(".*PRO\\s*", "", matchdata$KYC_Name3) 
    
    matchdata$JW_dist_upper_special_salutation<-stringdist(matchdata$Insured_Name3,
                                                           matchdata$KYC_Name3,method = "jw")
    
    matchdata$Cosine_dist_upper<-stringdist(matchdata$Insured_Name1,matchdata$KYC_Name1,
                                            method = "cosine")
    
    matchdata$Cosine_dist_upper_special<-stringdist(matchdata$Insured_Name2,
                                                    matchdata$KYC_Name2,method = "cosine")
    
    matchdata$Cosine_dist_upper_special_salutation<-stringdist(matchdata$Insured_Name3,
                                                               matchdata$KYC_Name3,method = "cosine")
    
    matchdata$Jaccard_dist_upper<-stringdist(matchdata$Insured_Name1,matchdata$KYC_Name1,
                                             method = "jaccard")
    
    matchdata$Jaccard_dist_upper_special<-stringdist(matchdata$Insured_Name2,
                                                     matchdata$KYC_Name2,method = "jaccard")
    
    matchdata$Jaccard_dist_upper_special_salutation<-stringdist(matchdata$Insured_Name3,
                                                                matchdata$KYC_Name3,method = "jaccard")
    
    check_names <- function(insured, kyc) {
      insured_words <- str_split(insured, " ")[[1]]
      insured_words<-insured_words[nchar(insured_words)>1]
      kyc_words <- str_split(kyc, " ")[[1]]
      kyc_words<-kyc_words[nchar(kyc_words)>1]
      any(insured_words%in%kyc_words)
    }
    
    matchdata <- matchdata %>%
      rowwise() %>%
      mutate(Match = check_names(Insured_Name3, KYC_Name3),
             Match2 = check_names(Insured_Name2, KYC_Name2))
    
    matchdata_word<-subset(matchdata,Match=="TRUE" | Match2=="TRUE" & JW_dist_upper_special_salutation>0.25)
    matchdata_wordnot<-subset(matchdata,!ID%in%matchdata_word$ID)
    
    
    
    
    extract_non_common <- function(insured_name, kyc_name) {
      insured_words <- unlist(strsplit(insured_name, " "))
      kyc_words <- unlist(strsplit(kyc_name, " "))
      
      non_common_insured <- insured_words[!insured_words %in% kyc_words]
      non_common_kyc <- kyc_words[!kyc_words %in% insured_words]
      
      non_common_insured_str <- paste(non_common_insured, collapse = " ")
      non_common_kyc_str <- paste(non_common_kyc, collapse = " ")
      
      return(list(non_common_insured_str, non_common_kyc_str))
    }
    
    df <- matchdata_word %>%
      rowwise() %>%
      mutate(
        Non_Common_Insured = extract_non_common(Insured_Name3, KYC_Name3)[[1]],
        Non_Common_KYC = extract_non_common(Insured_Name3, KYC_Name3)[[2]]
      ) %>%
      ungroup()
    
    min_nchar <- function(x) {
      if (x == "") return(0)
      words <- strsplit(x, " ")[[1]]
      if (length(words) == 0) return(0)
      min(nchar(words))
    }
    
    df$min_nchar1 <- sapply(df$Non_Common_Insured, min_nchar)
    df$min_nchar2 <- sapply(df$Non_Common_KYC, min_nchar)
    df2<-subset(df,min_nchar1==1 | min_nchar2==1)
    df3<-subset(matchdata_word,!ID%in%df2$ID)
    matchdata_wordnot<-rbind(df3,matchdata_wordnot)
    matchdata_word<-df2
    if (nrow(matchdata_word)>0){
    
    transform_names <- function(insured, kyc) {
      insured_words <- str_split(insured, "\\s+")[[1]]
      kyc_words <- str_split(kyc, "\\s+")[[1]]
      common_words <- intersect(insured_words, kyc_words)
      
      insured_transformed <- sapply(insured_words, function(word) {
        if (word %in% common_words) {
          word
        } else {
          str_sub(word, 1, 1)
        }
      })
      
      kyc_transformed <- sapply(kyc_words, function(word) {
        if (word %in% common_words) {
          word
        } else {
          str_sub(word, 1, 1)
        }
      })
      
      list(Insured_Name_Transformed = paste(insured_transformed, collapse = " "),
           KYC_Name_Transformed = paste(kyc_transformed, collapse = " "))
    }
    
    matchdata_word <- matchdata_word %>%
      rowwise() %>%
      mutate(Transformed = list(transform_names(Insured_Name3, KYC_Name3))) %>%
      unnest_wider(Transformed)
    
    matchdata_word$JW_dist_upper_special_salutation2<-stringdist(matchdata_word$Insured_Name_Transformed,
                                                                 matchdata_word$KYC_Name_Transformed,method = "jw")
    
    matchdata_word$Jaccard_dist_upper_special_salutation2<-stringdist(matchdata_word$Insured_Name_Transformed,
                                                                      matchdata_word$KYC_Name_Transformed,method = "jaccard")
    
    matchdata_word$Cosine_dist_upper_special_salutation2<-stringdist(matchdata_word$Insured_Name_Transformed,
                                                                     matchdata_word$KYC_Name_Transformed,method = "cosine")
    
    
    
    matchdata_word$Insured_Name_Transformed<-NULL
    matchdata_word$KYC_Name_Transformed<-NULL
    
    matchdata_word <- matchdata_word %>%
      rowwise() %>%
      mutate(Transformed = list(transform_names(Insured_Name2, KYC_Name2))) %>%
      unnest_wider(Transformed)
    
    matchdata_word$JW_dist_upper_special_salutation3<-stringdist(matchdata_word$Insured_Name_Transformed,
                                                                 matchdata_word$KYC_Name_Transformed,method = "jw")
    
    matchdata_word$Jaccard_dist_upper_special_salutation3<-stringdist(matchdata_word$Insured_Name_Transformed,
                                                                      matchdata_word$KYC_Name_Transformed,method = "jaccard")
    
    matchdata_word$Cosine_dist_upper_special_salutation3<-stringdist(matchdata_word$Insured_Name_Transformed,
                                                                     matchdata_word$KYC_Name_Transformed,method = "cosine")
    
    
    
    matchdata_word$Insured_Name_Transformed<-NULL
    matchdata_word$KYC_Name_Transformed<-NULL
    
    
    matchdata_word$JW_dist_upper_special_salutation2<-apply(matchdata_word[,c('JW_dist_upper_special_salutation3',
                                                                              'JW_dist_upper_special_salutation2')], 1, min)
    matchdata_word$Jaccard_dist_upper_special_salutation2<-apply(matchdata_word[,c('Jaccard_dist_upper_special_salutation3',
                                                                                   'Jaccard_dist_upper_special_salutation2')], 1, min)
    matchdata_word$Cosine_dist_upper_special_salutation2<-apply(matchdata_word[,c('Cosine_dist_upper_special_salutation3',
                                                                                  'Cosine_dist_upper_special_salutation2')], 1, min)
    
    matchdata_word$JW_dist_upper_special_salutation3<-NULL
    matchdata_word$Jaccard_dist_upper_special_salutation3<-NULL
    matchdata_word$Cosine_dist_upper_special_salutation3<-NULL
    
    matchdata_wordnot$JW_dist_upper_special_salutation2<-matchdata_wordnot$JW_dist_upper_special_salutation
    matchdata_wordnot$Jaccard_dist_upper_special_salutation2<-matchdata_wordnot$Jaccard_dist_upper_special_salutation
    matchdata_wordnot$Cosine_dist_upper_special_salutation2<-matchdata_wordnot$Cosine_dist_upper_special_salutation
    
    matchdata_word$Non_Common_Insured<-NULL
    matchdata_word$Non_Common_KYC<-NULL
    matchdata_word$min_nchar1<-NULL
    matchdata_word$min_nchar2<-NULL
    
    matchdata<-rbind(matchdata_word,matchdata_wordnot)
    } else {
      matchdata_wordnot$JW_dist_upper_special_salutation2<-matchdata_wordnot$JW_dist_upper_special_salutation
      matchdata_wordnot$Jaccard_dist_upper_special_salutation2<-matchdata_wordnot$Jaccard_dist_upper_special_salutation
      matchdata_wordnot$Cosine_dist_upper_special_salutation2<-matchdata_wordnot$Cosine_dist_upper_special_salutation
      
      matchdata<-matchdata_wordnot
    }
    matchdata$mindist_jw<-apply(matchdata[,c('JW_dist_upper','JW_dist_upper_special',
                                             'JW_dist_upper_special_salutation')], 1, min)
    
    matchdata$mindist_jac<-apply(matchdata[,c('Jaccard_dist_upper',
                                              'Jaccard_dist_upper_special','Jaccard_dist_upper_special_salutation')], 1, min)
    
    matchdata$mindist_cosine<-apply(matchdata[,c('Cosine_dist_upper',
                                                 'Cosine_dist_upper_special','Cosine_dist_upper_special_salutation')], 1, min)
    
    rm(df_cleaned,df_cleaned2,matchdata_word,matchdata_wordnot,stop_words2)
    
    compute_newdist <- function(kyc_name, insured_name) {
      newdist1 <- ifelse(grepl(kyc_name, insured_name,fixed=T), 0, 0.5)
      newdist2 <- ifelse(grepl(insured_name, kyc_name,fixed=T), 0, 0.5)
      return(c(newdist1, newdist2))
    }
    
    newdists <- mapply(compute_newdist, matchdata$KYC_Name3, matchdata$Insured_Name3)
    
    matchdata$newdist1 <- newdists[1,]
    matchdata$newdist2 <- newdists[2,]
    
    matchdata$newdist<-pmin(matchdata$newdist1, matchdata$newdist2)
    rm(transform_names,check_names,compute_newdist,newdists)
    
    try<-subset(matchdata,
                select = c(Policyno,ID,INSURED_NAME,KYC_NAME,mindist_jw,
                           mindist_jac,mindist_cosine,
                           newdist,JW_dist_upper_special_salutation2,
                           Jaccard_dist_upper_special_salutation2,
                           Cosine_dist_upper_special_salutation2))
    
    jwdata<-subset(try,mindist_jw<=0.22)
    jwdatanot<-subset(try,mindist_jw>0.22)
    jaccarddata<-subset(jwdatanot,mindist_jac<=0.15)
    jaccarddatanot<-subset(jwdatanot,mindist_jac>0.15)
    cosinedata<-subset(jaccarddatanot,mindist_jac<=0.2 & mindist_cosine<=0.06)
    cosinedatanot<-subset(jaccarddatanot,mindist_jac>0.2 | mindist_cosine>0.06)
    cosinedatab<-subset(cosinedatanot, mindist_cosine<=0.03)
    cosinedatanotb<-subset(cosinedatanot, mindist_cosine>0.03)
    cosinedata<-rbind(cosinedata,cosinedatab)
    wordcontaindata<-subset(cosinedatanotb,newdist==0)
    wordcontaindatanot<-subset(cosinedatanotb,newdist>0)
    
    jwtry<-subset(wordcontaindatanot,JW_dist_upper_special_salutation2<=0.2 & Jaccard_dist_upper_special_salutation2<=0.2)
    jwtrynot<-subset(wordcontaindatanot,!ID%in%jwtry$ID)
    jaccardtry<-subset(jwtrynot,Jaccard_dist_upper_special_salutation2<=0.15 & Cosine_dist_upper_special_salutation2<=0.15)
    jaccardtrynot<-subset(jwtrynot,!ID%in%jaccardtry$ID)
    cosinetry<-subset(jaccardtrynot,Cosine_dist_upper_special_salutation2<=0.05 & Jaccard_dist_upper_special_salutation2<=0.15)
    cosinetrynot<-subset(jaccardtrynot,!ID%in%cosinetry$ID)
    jwtry$mindist_jw<-jwtry$JW_dist_upper_special_salutation2
    jaccardtry$mindist_jac<-jaccardtry$Jaccard_dist_upper_special_salutation2
    cosinetry$mindist_cosine<-cosinetry$Cosine_dist_upper_special_salutation2
    jwdata<-rbind(jwdata,jwtry)
    jaccarddata<-rbind(jaccarddata,jaccardtry)
    cosinedata<-rbind(cosinedata,cosinetry)
    
    
    jwdata$Match_method<-"JW"
    jaccarddata$Match_method<-"Jaccard"
    cosinedata$Match_method<-"Cosine"
    wordcontaindata$Match_method<-"Word Contain"
    cosinetrynot$Match_method<-"Not Match"
    nadist<-subset(try,is.na(mindist_jw) | is.na(mindist_jac) | is.na(mindist_cosine) |
                     is.na(JW_dist_upper_special_salutation2) | 
                     is.na(Jaccard_dist_upper_special_salutation2) |
                     is.na(Cosine_dist_upper_special_salutation2))
    nadist$Match_method<-"Not Match"
    
    try2<-rbind(jwdata,jaccarddata,cosinedata,wordcontaindata,cosinetrynot,
                nadist)
    
    
    rm(jwdata,jwdatanot,jaccarddata,jaccarddatanot,cosinedata,cosinedatanot,
       wordcontaindata,wordcontaindatanot,nadist,jwtry,jwtrynot,jaccardtry,
       jaccardtrynot,cosinetry,cosinetrynot,cosinedatab,cosinedatanotb,df,df2,df3)
    
    tryfin<-subset(try2,select = c(Policyno,INSURED_NAME,KYC_NAME,Match_method))
    
   
    return(tryfin)
  })
  
  output$trialdata<-renderTable({
    req(input$polno)
    req(input$insname)
    req(input$kycname)

    trialdata()
  })
  
 
  observe({
    # Check that all inputs are populated
    if (is.null(input$polno) || is.null(input$insname) || is.null(input$kycname)) {
      return() # Exit if any input is NULL
    }
    
    # Call the reactive expression to get the latest matchdata
    matchdata_to_write <- trialdata()
    
    # Define the path to the CSV file
    file_path <- "./matchdata.csv"
    
    # Check if the file exists
    if (file.exists(file_path)) {
      # Read the existing data
      existing_data <- read_csv(file_path)
      # Bind the new row to the existing data
      new_data <- rbind(existing_data, matchdata_to_write)
    } else {
      # If the file does not exist, create new data with just matchdata
      new_data <- matchdata_to_write
    }
    
    # Write the updated data to the CSV file
    write_csv(new_data, file_path)
  })
   
  
})