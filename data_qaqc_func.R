
colourise <- function(x, colour) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", colour, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", colour,
            x)
  } else x
}

conv_date <- function(x) as.Date(x, origin = "1899-12-30")

numeric_qaqc <- function(data_validate, test_set, iteration, valid_message = TRUE){
  
  i = iteration
  
  errors_found = 0
  
  if(is.factor(xlsx_data[,data_validate$xlsx_fields[i]])){
    
    test_set$num_test <- suppressWarnings(as.numeric(levels(xlsx_data[,data_validate$xlsx_fields[i]]))[xlsx_data[,data_validate$xlsx_fields[i]]])
    
  }else{
    
    test_set$num_test <- suppressWarnings(as.numeric(xlsx_data[,data_validate$xlsx_fields[i]]))
    
  }
  
  # test_result <- suppressWarnings(dplyr::select(test_set, test_set, num_test) %>% filter(is.na(num_test)) %>% group_by(test_set, num_test) %>% count(count = n()))
  test_result <- dplyr::select(test_set, test_set, num_test) %>% filter(is.na(num_test)) %>% group_by(test_set, num_test) %>% summarise(count = n(), .groups = 'drop')

  if(data_validate$no_nulls[i] == 1 & nrow(test_result) > 0){
    cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (numeric) has incorrect values. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
    print(kable(test_result[is.na(test_result$num_test),c(1,3)], col.names = c("Invalid Data Values",paste0("Number of Records of ", nrow(xlsx_data)))))
    cat("\n")
    cat("\n\n ---- \n\n")
    errors_found = 1
    
  }else{
    
    if(nrow(test_result[test_result$test_set !="No Value",]) > 0){
      
      cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (numeric) has incorrect values. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
      print(kable(test_result[,c(1,3)], col.names = c("Invalid Data Values",paste0("Number of Records of ", nrow(xlsx_data)))))
      cat("\n")
      cat("\n\n ---- \n\n")
      errors_found = 1        
      
    } else{
        if(valid_message){
          pass_message(i, 'numeric')
        }
    }
  }
  
  return(errors_found)
  
}

string_qaqc <- function(data_validate, test_set, iteration){
  
  i = iteration
  errors_found = 0
  
  test_result <- dplyr::select(test_set, test_set) %>% filter(test_set == "No Value") %>% group_by(test_set) %>% count(count = n())
  # 
  if(data_validate$no_nulls[i] == 1){
    
    
    if(data_validate$fields[i] == "site_code"){
      
      # cat(paste0(i, " ", '[',data_validate$xlsx_fields[i],"] test pass site_code  \n\n"))
      
      if(nrow(test_result[test_result$test_set =="No Value",])==1){
        
        cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (text) is required data and has blank values. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
        print(kable(test_result[test_result$test_set == "No Value",c(1,3)], col.names = c("Invalid Data Values",paste0("Number of Records of ", nrow(xlsx_data)))))
        cat("\n")
        errors_found = 1
        
      } else{
        pass_message(i, 'text')
      }          
      
      if(nrow(data_validate[data_validate$fields == "site_desc",]) > 0){
        
        sitecode <- data_validate[data_validate$fields == "site_code",]$xlsx_fields
        sitedesc <- data_validate[data_validate$fields == "site_desc",]$xlsx_fields
        
        if(nrow(data_validate[data_validate$fields == "waterbody",]) > 0){
          
          waterbdy <- data_validate[data_validate$fields == "waterbody",]$xlsx_fields
          
          vars = c(waterbdy, sitecode, sitedesc)
          sites <- xlsx_data %>% select(all_of(vars)) %>% distinct() 
          vars = c(waterbdy, sitecode)
          dup_desc <- sites %>% select(all_of(vars)) %>% group_by(.dots=vars) %>% count() %>% filter(n>1)
          if(nrow(dup_desc)>0){
            cat(colourise(paste('**These sites have multiple descriptions per site** \n\n', sep=""), "red"))
            print(kable(dup_desc, col.names = c(str_to_title(waterbdy),str_to_title(sitecode), "Number of Location Descriptions")))
            cat("\n")
          } else{
            pass_message(i, 'text')
          } 
          
          vars = c(waterbdy, sitedesc)
          dup_sites <- sites %>% select(all_of(vars)) %>% group_by(.dots=vars) %>% count() %>% filter(n>1)
          if(nrow(dup_sites)>0){
            cat(colourise(paste('**The same description occurs for multiple sites** \n\n', sep=""), "red"))            
            print(kable(dup_sites, col.names = c(str_to_title(waterbdy),"Location Description","Number of Different Sites")))
            cat("\n")
          } else{
            pass_message(i, 'text')
          } 
          
        }else{
          
          vars = c(sitecode, sitedesc)
          sites <- xlsx_data %>% select(all_of(vars)) %>% distinct()
          vars = c(sitecode)
          sites %>% select(all_of(vars)) %>% group_by(.dots=vars) %>% count() %>% filter(n>1)
          vars = c(sitecode)
          
          #!!!!!!! NEED to convert null site codes to 'No Value' for output
          dup_desc <- sites %>% select(all_of(vars)) %>% group_by(.dots=vars) %>% count() %>% filter(n>1)
          
          if(nrow(dup_desc)>0){
            cat(colourise(paste('**These sites have different descriptions** \n\n', sep=""), "red"))
            print(kable(dup_desc, col.names = c(str_to_title(sitecode),"Number of Location Descriptions")))
            cat("\n")
          } else{
            pass_message(i, 'text')
          } 
          vars = c(sitedesc)
          dup_sites <- sites %>% select(all_of(vars)) %>% group_by(.dots=vars) %>% count() %>% filter(n>1)
          if(nrow(dup_sites)>0){
            cat(colourise(paste('**The same description occurs for multiple sites** \n\n', sep=""), "red"))               
            print(kable(dup_sites, col.names = c("Location Description","Number of Different Sites")))
            cat("\n")
          } else{
            pass_message(i, 'text')
          } 
          
        }   
      }
      
    } else if(data_validate$fields[i] == "gear_type"){
      test_set$test_set_upper <- toupper(test_set$test_set)
      gear <- left_join(test_set, gear_lu, by=c("test_set_upper"="gear_type")) %>% select(test_set, gear_desc) %>% filter(is.na(gear_desc)) %>% distinct(test_set) %>% arrange(test_set)
      if(nrow(gear)>0){
        cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (text) is required data and has non standard gear type values. Please update to [**AAE standard gear type values**](https://delwpvicgovau.sharepoint.com/:x:/r/sites/ecm_92/AAEAdministration/AAE%20Database%20and%20Standards/DRAFT_aae_standard_data_entry_attributes.xlsx?d=w537521fd2de044aeb9dd71a0ac804af4&csf=1&web=1&e=iqXs2t). Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
        print(kable(gear, col.names = c("Gear Types")))
        cat("\n")
      } else{
        pass_message(i, 'text')
      }          
      
    } else if(data_validate$fields[i] == "species"){
      
      #!!!!!! NEED to input species name
      
      test_set$test_set_lower <- tolower(test_set$test_set)
      sp <- left_join(test_set, species_lu, by=c("test_set_lower"="species")) %>% select(test_set_lower, test_set, vba_taxon_id) %>% filter(is.na(vba_taxon_id) & test_set_lower != "no fish") %>% distinct(test_set) %>% arrange(test_set)
      if(nrow(sp)>0){
        cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (text) is required data and has non VBA species names. Please update to [**VBA species names**](https://delwpvicgovau.sharepoint.com/:x:/r/sites/ecm_92/AAEAdministration/AAE%20Database%20and%20Standards/DRAFT_aae_standard_data_entry_attributes.xlsx?d=w537521fd2de044aeb9dd71a0ac804af4&csf=1&web=1&e=iqXs2t). Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
        print(kable(sp, col.names = c("Species")))
        cat("\n")
        errors_found = 1
      } else{
        pass_message(i, 'text')
      }           
      
    } else{
      
      if(nrow(test_result[test_result$test_set =="No Value",])==1){
        
        cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (text) is required data and has blank values. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
        print(kable(test_result[test_result$test_set == "No Value",c(1,3)], col.names = c("Invalid Data Values",paste0("Number of Records of ", nrow(xlsx_data)))))
        cat("\n")
        errors_found = 1
        
      }          
      
    }
    
    cat("\n\n ---- \n\n")
    
  }else{
    cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (text) is in the correct format. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "green"))
    cat("\n")
    cat("\n\n ---- \n\n")  
  }  
  
  return(errors_found)
  
}

pass_message <- function(iteration, datatype){
  cat(colourise(paste('**[',data_validate$xlsx_fields[iteration],']** (', datatype, ') is in the correct format. Matched to standard name: **[',data_validate$fields[iteration],']** \n\n', sep=""), "green"))
  cat("\n")
  cat("\n\n ---- \n\n")  
}

date_qaqc <- function(xlsx_data, data_validate, test_set, iteration){
  
  i = iteration
  
  test_set$num_test <- xlsx_data$survey_date_formatted
  test_result <- dplyr::select(test_set, test_set, num_test) %>% filter(is.na(num_test)) %>% group_by(test_set, num_test) %>% count(count = n())
  
  # if(test_result$n > 0){
  if(nrow(test_result) > 0){
    cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (date) is required data and has incorrect date values. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
    print(kable(test_result[,c(1,3)], col.names = c("Invalid date values",paste0("Number of Records of ", nrow(xlsx_data)))))
    cat("\n")
    cat("\n\n ---- \n\n")  
    errors_found = 1
  }else{

    cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (date) is in the correct format. Check summary for incorrect dates \n\n', sep=""), "green"))
    cat("\n")
    cat("\n\n ---- \n\n")  
        
  }
  
  return(errors_found)
  
}

test_longitude <- function(x) ifelse(x >=140 & x <= 151,x, NA)
test_latitude <- function(x) ifelse(x >=-40 & x <= -33,x, NA)
test_northing <- function(zone, coordinate){
  # if Y then if
  #between 5548706 and 6341600 for #z55
  #between 5524051 and 6348270 for #z54 
  #between 2150719 and 2931756 #VG  
  if(zone == 55){
    return(ifelse(coordinate>=5548706 & coordinate <=6341600,TRUE, FALSE))
  }else if(zone == 54){
    return(ifelse(coordinate>=5524051 & coordinate <=6348270,TRUE, FALSE))     
  }else{
    return(FALSE)
  }
}
test_easting <- function(zone, coordinate){
  # if X then if
  #between 168839 and 814627 for #z55
  #between 425179 and 860518 for #z54 
  #between 2031902 and 3061604 #VG   
  if(zone == 55){
    return(ifelse(coordinate>=168839 & coordinate <=814627,TRUE, FALSE))
  }else if(zone == 54){
    return(ifelse(coordinate>=425179 & coordinate <=860518,TRUE, FALSE))     
  }else{
    return(FALSE)
  }
}

coordinate_qaqc_depreciated <- function(data_validate, test_set, iteration){
  
  i = iteration
  
  errors_found = 0
  
  #test if all numeric
  if((numeric_qaqc(data_validate, test_set, i, FALSE)) == 0){
    
    # cat(paste0(i, " ", '[',data_validate$xlsx_fields[i],"] test pass numeric  \n\n"))
    
      # cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (coordinate) are numeric! Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "blue"))
      # cat("\n")
      # cat("\n\n ---- \n\n")
      
      #====== if value >=40 and <=151 #then a lat/lon ==================================================
      if(min(as.numeric(test_set$test_set)) >=-90 & max(as.numeric(test_set$test_set)) <= 180){
        #test if X or Y
        if(data_validate$fields[i] %in% c("x_coordinate", "section_start_x", "section_end_x")){
          # if X then if
          #between 140 and 151 or
          test_set$num_test = test_longitude(as.numeric(test_set$test_set))
          range_text = "140** and **151"
          
        }else{
          # if Y then if
          #between -40 and -33 or  
          test_set$num_test = test_latitude(as.numeric(test_set$test_set)) 
          range_text = "-40** and **-33"
        }
        
        test_result <- dplyr::select(test_set, test_set, num_test) %>% filter(is.na(num_test)) %>% group_by(test_set, num_test) %>% summarise(count = n(), .groups = 'drop')
        
        if(nrow(test_result) > 0){
          cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (coordinate) should have all values between **', range_text,'**. Check they are correctly labelled Latitude or Longitude and within the general vicinity of Victoria. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
          print(kable(test_result[is.na(test_result$num_test),c(1,3)], col.names = c("Invalid Data Values",paste0("Number of Records of ", nrow(xlsx_data)))))
          cat("\n")
          cat("\n\n ---- \n\n")
          errors_found = 1
          
        }else{
          
          cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (coordinate) is correct! Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "green"))
          cat("\n")
          cat("\n\n ---- \n\n")
          errors_found = 0
        }
        
      }else{
        
        cat(paste0(i, " ", '[',data_validate$xlsx_fields[i],"] not lat lon  \n\n"))

      #====== if not lat/lon ==================================================
      
      #test if mga zone field present
      # if so is it populated
      
      # if mga zone is populated
      #test if X or Y
      
      # if X then if
      #between 0 and 873788 or #z55
      #between 406582 and 1436095 or #z54 
      #between 2031902 and 3061604 #VG  
      
      # if Y then if
      #between 5548706 and 6341600 or #z55
      #between 5524051 and 6348270 or #z54 
      #between 2150719 and 2931756 #VG      
      
      }
    
  }else{
    
    # cat(paste0(i, " ", '[',data_validate$xlsx_fields[i],"] test fail numeric  \n\n"))
    # errors_found <- numeric_qaqc(data_validate, test_set, i)
    # cat(paste0(i, " Error ", '[',errors_found,"] test fail numeric  \n\n"))
  }


return(errors_found)
  
#if not numeric
  #remove characters and keep spacing
  #check for space padding and remove
  #check if degrees decimal minutes
  #convert to decimal degrees
  #Convert to numeric
    
}

coordinate_qaqc_beta <- function(data_validate, test_set, iteration){
  
  i = iteration
  
  errors_found = 0
  
  errors_found_numeric <- numeric_qaqc(data_validate, test_set, i)
  
  full_row_count = nrow(test_set) #get row count of dataset
  
  #isolate only numerically valid records
  test_set$num = as.numeric(test_set$test_set)
  test_set = as.data.frame(na.omit(test_set$num))
  colnames(test_set)[1] = "test_set"
  sub_row_count = nrow(test_set)
  
  #test if some numeric
  if(sub_row_count > 0){
    
    # cat(paste0(i, " ", '[',data_validate$xlsx_fields[i],"] test pass numeric  \n\n"))
    
    # cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (coordinate) are numeric! Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "blue"))
    # cat("\n")
    # cat("\n\n ---- \n\n")
    
    #====== if value >=40 and <=151 #then a lat/lon ==================================================
    if(min(as.numeric(test_set$test_set)) >=-90 & max(as.numeric(test_set$test_set)) <= 180){
      #test if X or Y
      if(data_validate$fields[i] %in% c("x_coordinate", "section_start_x", "section_end_x")){
        # if X then if
        #between 140 and 151 or
        test_set$num_test = test_longitude(as.numeric(test_set$test_set))
        range_text = "140** and **151"
        
      }else{
        # if Y then if
        #between -40 and -33 or  
        test_set$num_test = test_latitude(as.numeric(test_set$test_set)) 
        range_text = "-40** and **-33"
      }
      
      test_result <- dplyr::select(test_set, test_set, num_test) %>% filter(is.na(num_test)) %>% group_by(test_set, num_test) %>% summarise(count = n(), .groups = 'drop')
      
      if(nrow(test_result) > 0){
        
        if(is.numeric(test_result$test_set)){test_result$test_set = as.character(test_result$test_set)}
        
        cat(colourise(paste('**[',data_validate$xlsx_fields[i],'] (coordinate)** should have all values between **', range_text,'**. Check they are correctly labelled Latitude (Y) or Longitude (X) are within the general vicinity of Victoria. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
        print(kable(test_result[is.na(test_result$num_test),c(1,3)], col.names = c("Invalid Data Values",paste0("Number of Records of ", nrow(xlsx_data)))))
        cat("\n")
        cat("\n\n ---- \n\n")
        errors_found = 1
        
      }else{
        
        if(full_row_count == sub_row_count){
          cat(colourise(paste('**[',data_validate$xlsx_fields[i],']** (coordinate) is correct! Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "green"))
          cat("\n")
          cat("\n\n ---- \n\n")
          errors_found = 0
          
        }else{
          
          cat(colourise(paste('**[',data_validate$xlsx_fields[i],'] (coordinate)** has ' , sub_row_count , ' correct values of ' , full_row_count , ' records. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "darkorange"))
          cat("\n")
          cat("\n\n ---- \n\n")
          errors_found = 1      
        }
      }
      
    }else{
      
      #test if all GDA or MGA!!!!!!!!!!!!
      if((as.numeric(min(test_set$test_set)) < 0 & as.numeric(max(test_set$test_set)) >= 0) |
       (as.numeric(min(test_set$test_set)) <= 180 & as.numeric(max(test_set$test_set)) > 180)
      ){
      
        cat(colourise(paste0('**[',data_validate$xlsx_fields[i],'] (coordinate)** could have a mix of latitude/longitude or northing/easting coordinates.  \n\n'), "red"))
        cat("\n")
        cat("\n\n ---- \n\n")
        errors_found = 1
      
      }else{
        #if not lat/lon
        
        #test if mga zone field present
        if(nrow(data_validate[data_validate$fields == "mga_zone",]) > 0){
          zone_idx = as.numeric(which(data_validate$fields == "mga_zone"))
          
          # if mga zone is populated
          
          zone_set <- xlsx_data[,data_validate$xlsx_fields[zone_idx]]
          zone_set <- as.character(zone_set, stringsAsFactors = FALSE)
          zone_set[which(zone_set == "")] <- "No Value"
          zone_set <- as.data.frame(zone_set, stringsAsFactors = FALSE)
          
          if(length(zone_set[zone_set$zone_set !="No Value",]) > 0){
            
            #get list of coordinates to check
            coord_list = xlsx_data[,data_validate$xlsx_fields[c(i)]]
            
            # # ------- NOT TO BE INCLUDED --------------------
            # coord_list = str_replace(coord_list, "E", "")
            # # -----------------------------------------------
            
            coord_list <- as.character(coord_list, stringsAsFactors = FALSE)
            coord_list[which(coord_list == "")] <- "No Value"
            coord_list <- as.data.frame(coord_list, stringsAsFactors = FALSE)
            
            #bind the zone and coordinate lists
            coord_list = cbind(zone_set, coord_list)
            
            # colnames(coord_list)
            
            #group the coordinates and filter out nulls
            coord_list = dplyr::select(coord_list, zone_set, coord_list) %>% filter(zone_set != "No Value" & coord_list != "No Value") %>% group_by(zone_set, coord_list) %>% summarise(count = n(), .groups = 'drop')
            
            coord_list$zone_set_num = as.numeric(coord_list$zone_set)
            coord_list$coord_list_num = as.numeric(coord_list$coord_list)
            
            #Non-numeric coords reported separately
            # coord_list_error = coord_list[is.na(coord_list$zone_set_num) | is.na(coord_list$coord_list_num), ]
            
            coord_list_pass = coord_list[!is.na(coord_list$zone_set_num) & !is.na(coord_list$coord_list_num), ]
            
            coord_list_fail = coord_list_pass[coord_list_pass$count < 0,]
            
            
            for(j in 1:nrow(coord_list_pass)){
              
              #test if X or Y
              if(data_validate[i,]$fields == "y_coordinate") #Northing
              {
                if(!test_northing(coord_list_pass$zone_set_num[j], coord_list_pass$coord_list_num[j])){
                  coord_list_fail = rbind(coord_list_fail, coord_list_pass[c(j),])
                }
                
              }else #Easting
              {
                if(!test_easting(coord_list_pass$zone_set_num[j], coord_list_pass$coord_list_num[j])){
                  coord_list_fail = rbind(coord_list_fail, coord_list_pass[c(j),])
                }   
              }
            }
            
            if(nrow(coord_list_fail) > 0){
              cat(colourise(paste('**[',data_validate$xlsx_fields[i],'] (coordinate)** has incorrect values. Check they are correctly labelled Northing (Y) or Easting (X) are  within the general vicinity of Victoria. Also check the attributed MGA Zone. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "red"))
              print(kable(coord_list_fail[,c(1,2,3)], col.names = c("Recorded MGA Zone", "Invalid Data Values",paste0("Number of Records of ", nrow(xlsx_data)))))
              cat("\n")
              cat("\n\n ---- \n\n")
              errors_found = 1   
              
            }else{
              
              cat(colourise(paste('**[',data_validate$xlsx_fields[i],'] (coordinate)** has ' , sub_row_count , ' correct values of ' , full_row_count , ' records. Matched to standard name: **[',data_validate$fields[i],']** \n\n', sep=""), "darkorange"))
              cat("\n")
              cat("\n\n ---- \n\n")
              errors_found = 0       
            }
          }
        }
        
      }   
      
    }
    
  }
  # else{
  #   
  #   # cat(paste0(i, " ", '[',data_validate$xlsx_fields[i],"] test fail numeric  \n\n"))
  #   # errors_found <- numeric_qaqc(data_validate, test_set, i)
  #   # cat(paste0(i, " Error ", '[',errors_found,"] test fail numeric  \n\n"))
  # }
  
  errors_found = errors_found + errors_found_numeric
  return(errors_found)
  
  #if not numeric
  #remove characters and keep spacing
  #check for space padding and remove
  #check if degrees decimal minutes
  #convert to decimal degrees
  #Convert to numeric
  
}

waterbody_summary <- function(xlsx_data, data_validate){
  wtbdy <- xlsx_data %>% select(data_validate[data_validate$fields == "waterbody",]$xlsx_fields) %>% distinct()
  cat(colourise(paste('**Dataset waterbodies in [',data_validate[data_validate$fields == "waterbody",]$xlsx_fields,']**  \n\n', sep=""), "green"))               
  print(kable(wtbdy))
  cat("\n")    
  cat("\n\n ---- \n\n")
}

survey_date_summary <- function(xlsx_data, data_validate){
  dts <- xlsx_data %>% select(survey_date_formatted) %>% distinct() %>% arrange(survey_date_formatted)
  cat(colourise(paste('**Dataset dates in [',data_validate[data_validate$fields == "survey_date",]$xlsx_fields,']**  \n\n', sep=""), "green"))               
  print(kable(dts), col.names = c(data_validate[data_validate$fields == "survey_date",]$xlsx_fields))
  cat("\n")    
  cat("\n\n ---- \n\n")
}

survey_date_summary_OLD <- function(xlsx_data, data_validate){
  dts <- xlsx_data %>% select(formatted_date) %>% distinct() %>% arrange(formatted_date)
  cat(colourise(paste('**Dataset dates in [',data_validate[data_validate$fields == "survey_date",]$xlsx_fields,']**  \n\n', sep=""), "green"))               
  print(kable(dts), col.names = c(data_validate[data_validate$fields == "survey_date",]$xlsx_fields))
  cat("\n")    
  cat("\n\n ---- \n\n")
}


gear_type_summary <- function(xlsx_data, data_validate){
  test_set <- xlsx_data[,data_validate[data_validate$fields == "gear_type",]$xlsx_fields]
  test_set <- as.character(test_set, stringsAsFactors = FALSE)
  test_set[which(test_set == "")] <- "No Value"
  test_set <- as.data.frame(test_set)
  gt <- test_set %>% distinct()
  cat(colourise(paste('**Dataset gear types in [',data_validate[data_validate$fields == "gear_type",]$xlsx_fields,']**  \n\n', sep=""), "green"))               
  print(kable(gt, col.names = c("Gear Types")))
  cat("\n")    
  cat("\n\n ---- \n\n")
}

species_summary <- function(xlsx_data, data_validate){
  
  test_set <- data.frame(File=character(), stringsAsFactors=FALSE)
  
  if(nrow(data_validate[data_validate$fields == "species",]) > 0){
    
    test_set <- mutate_all(xlsx_data, funs(tolower)) %>% select("species" = data_validate[data_validate$fields == "species",]$xlsx_fields)  %>% distinct(species) %>% arrange(species)
    
  }   
  
  if(nrow(data_validate[data_validate$fields == "total_length",]) > 0 & nrow(test_set) > 0){
    
    total_length_summary(xlsx_data, data_validate, test_set)
    
  }
  
  if(nrow(data_validate[data_validate$fields == "fork_length",]) > 0 & nrow(test_set) > 0){
    
    fork_length_summary(xlsx_data, data_validate, test_set)
  }
  
  if(nrow(data_validate[data_validate$fields == "weight",]) > 0 & nrow(test_set) > 0){
    
    weight_summary(xlsx_data, data_validate, test_set)
    
  } 
  
  if(nrow(test_set) > 0){
    cat(colourise(paste('**Dataset species in [',data_validate[data_validate$fields == "species",]$xlsx_fields,']**  \n\n', sep=""), "green"))               
    print(kable(test_set))
    cat("\n")    
    cat("\n\n ---- \n\n") 
  }
}

coordinate_summary <- function(xlsx_data, data_validate, field_i){

  test_set <- xlsx_data[,data_validate[field_i,]$xlsx_fields]
  test_set <- as.character(test_set, stringsAsFactors = FALSE)
  test_set[which(test_set == "")] <- "No Value"
  test_set <- as.data.frame(test_set)
  gt <- test_set %>% distinct()
  cat(colourise(paste('**Dataset coordinates in [',data_validate[field_i,]$xlsx_fields ,']**  \n\n', sep=""), "green"))               
  print(kable(gt, col.names = c("Coordinates")))
  cat("\n")    
  cat("\n\n ---- \n\n")
}

total_length_summary <- function(xlsx_data, data_validate, test_set){
  
  test_set2 <- mutate_all(xlsx_data, funs(tolower)) %>% select("species" = data_validate[data_validate$fields == "species",]$xlsx_fields, "length" = data_validate[data_validate$fields == "total_length",]$xlsx_fields)  %>% group_by(species) %>% summarise("min_total_length_mm" = min(as.numeric(length), na.rm = TRUE), "max_total_length_mm" = max(as.numeric(length), na.rm = TRUE))
  
  if(nrow(test_set) > 0){
    test_set <- full_join(test_set, test_set2, by='species')
    test_set[sapply(test_set, is.infinite)] <- 0
    
  }
}

fork_length_summary <- function(xlsx_data, data_validate, test_set){
  
  test_set2 <- mutate_all(xlsx_data, funs(tolower)) %>% select("species" = data_validate[data_validate$fields == "species",]$xlsx_fields, "length" = data_validate[data_validate$fields == "fork_length",]$xlsx_fields)  %>% group_by(species) %>% summarise("min_fork_length_mm" = min(as.numeric(length), na.rm = TRUE), "max_fork_length_mm" = max(as.numeric(length), na.rm = TRUE))      
  
  if(nrow(test_set) > 0){
    test_set <- full_join(test_set, test_set2, by='species')
    test_set[sapply(test_set, is.infinite)] <- 0
    
  }
  
}

weight_summary <- function(xlsx_data, data_validate, test_set){
  
  test_set2 <- mutate_all(xlsx_data, funs(tolower)) %>% select("species" = data_validate[data_validate$fields == "species",]$xlsx_fields, "weight" = data_validate[data_validate$fields == "weight",]$xlsx_fields)  %>% group_by(species) %>% summarise("min_weight_gm" = min(as.numeric(weight), na.rm = TRUE), "max_weight_gm" = max(as.numeric(weight), na.rm = TRUE))      
  
  if(nrow(test_set) > 0){
    test_set <- full_join(test_set, test_set2, by='species')
    test_set[sapply(test_set, is.infinite)] <- 0
    
  } 
  
}



