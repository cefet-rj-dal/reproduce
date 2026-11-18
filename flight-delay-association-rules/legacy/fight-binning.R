# Delay percentages

k_f$delay_level_dep<-ifelse(k_f$percatrasop>=0 & 
                              k_f$percatrasop<0.1538462, "low",
                            ifelse(k_f$percatrasop>=0.1538462 & 
                                     k_f$percatrasop<0.3333333, "medium",      
                                   ifelse(k_f$percatrasop>=0.3333333 & 
                                            k_f$percatrasop<=1, "high","NA")))


k_f$delay_level_arr<-ifelse(k_f$percatrasoc>=0 & 
                              k_f$percatrasoc<0.1538462, "low",
                            ifelse(k_f$percatrasoc>=0.1538462 & 
                                     k_f$percatrasoc<0.3333333, "medium",      
                                   ifelse(k_f$percatrasoc>=0.3333333 & 
                                            k_f$percatrasoc<=1, "high","NA")))
