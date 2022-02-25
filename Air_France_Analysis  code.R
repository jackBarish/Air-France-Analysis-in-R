######################################################################
# Created by                                                        ##
#   Alejandro Soto                                                  ##
#   Daniel Gonzalez                                                 ##
#   Don-Moses Chiemela                                              ##
#   Jack Barish                                                     ##
#   Rebecca Medeiros                                                ##
# Team 9                                                            ##
#                                                                   ##
# Presentation Video : https://www.youtube.com/watch?v=SfAQNMP84-Y  ##
#                                                                   ##
# A1: Business Case Presentation                                    ##
######################################################################


#getting necessary libraries
library(readxl)
library(Hmisc)
library(plotly)
library(ggplot2)
# Getting main data set
my_data <- read_excel("Air France Case Spreadsheet Supplement.xls", 
                      sheet = 2)
# getting information from kayak
My_data_k <- read_excel("Air France Case Spreadsheet Supplement.xls",  
                        range = "Kayak!B8:H9")
# getting information from kayak

################################################################################
################################################################################


#return on advert calculation
my_data$Revenue <- my_data$Amount - my_data$`Total Cost`
my_data$ROA <- as.numeric(my_data$Revenue / my_data$`Total Cost`)
my_data$prob_of_booking<-(my_data$`Trans. Conv. %`*my_data$`Engine Click Thru %`) /100


##################################
#Key Factor Summary              #
##################################
# Getting summary statistics for whole data set

Key_factors<- c("Min","Mean","SD","Median","Mode","Max")
clicks<- c(min(my_data$Clicks),round(mean(my_data$Clicks),2),
           round(sd(my_data$Clicks),2),median(my_data$Clicks),
           mode(my_data$Clicks),max(my_data$Clicks))
Impressions<- c(min(my_data$Impressions),round(mean(my_data$Impressions),2),
                round(sd(my_data$Impressions),2),median(my_data$Impressions),
                mode(my_data$Impressions),max(my_data$Impressions))
Amount<- c(min(my_data$Amount),round(mean(my_data$Amount),2),
           round(sd(my_data$Amount),2),median(my_data$Amount),
           mode(my_data$Amount),max(my_data$Amount))
Total_Cost<- c(min(my_data$`Total Cost`),round(mean(my_data$`Total Cost`),2),
               round(sd(my_data$`Total Cost`),2),
               round(median(my_data$`Total Cost`),2),mode(my_data$`Total Cost`),
               round(max(my_data$`Total Cost`),2))
Revenue<- c(round(min(my_data$Revenue),2),round(mean(my_data$Revenue),2),
            round(sd(my_data$Revenue),2),round(median(my_data$Revenue),2),
            mode(my_data$Revenue),round(max(my_data$Revenue),2))


#Making summary statistics into a data frame
key_factor_summary <- as.data.frame(cbind(Key_factors, clicks, Impressions, 
                                          Amount, Total_Cost,Revenue))
key_factor_summary



##################################
#sub setting publisher revenue   #
##################################

#return on advert by yahoo 
Yahoo_rev<-my_data[which(my_data$`Publisher Name` =="Yahoo - US"),]
yahoo_revenue<-sum(Yahoo_rev$Amount)
yahoo_cost<-sum(Yahoo_rev$'Total Cost')
yahoo_Net <- yahoo_revenue-yahoo_cost
yahoo_ROA <- yahoo_Net/yahoo_cost
yahoo_CTR <- round(mean(Yahoo_rev$`Engine Click Thru %`),2)
yahoo_TCR <- round(mean(Yahoo_rev$`Trans. Conv. %` ),2)
yahoo_prob_of_booking <- round(mean((Yahoo_rev$`Trans. Conv. %` * 
                                       Yahoo_rev$`Engine Click Thru %`) 
                                    /100),2)

#return on advert by MSN 
MSN_rev<-my_data[which(my_data$`Publisher Name` =="MSN - Global"),]
MSN_revenue<-sum(MSN_rev$Amount)
MSN_cost<-sum(MSN_rev$'Total Cost')
MSN_Net <-MSN_revenue-MSN_cost
MSN_ROA <- MSN_Net/MSN_cost
MSN_CTR <- round(mean(MSN_rev$`Engine Click Thru %`),2)
MSN_TCR <- round(mean(MSN_rev$`Trans. Conv. %` ),2)
MSN_prob_of_booking <- round(mean(( MSN_rev$`Trans. Conv. %` * 
                                      MSN_rev$`Engine Click Thru %`) 
                                  /100),2)

#return on advert by Ovature 
ovature_rev<-my_data[which(my_data$`Publisher Name` =="Overture - Global"),]
ovature_revenue<-sum(ovature_rev$Amount)
ovature_cost<-sum(ovature_rev$'Total Cost')
ovature_Net <-ovature_revenue-ovature_cost
ovature_ROA <- ovature_Net/ovature_cost
ovature_CTR <- round(mean(ovature_rev$`Engine Click Thru %`),2)
ovature_TCR <- round(mean(ovature_rev$`Trans. Conv. %` ),2)
ovature_prob_of_booking <- round(mean(( ovature_rev$`Trans. Conv. %` * 
                                          ovature_rev$`Engine Click Thru %`) 
                                      /100),2)

#return on advert by Google Global 
Google_Global_rev<-my_data[which(my_data$`Publisher Name` =="Google - Global"),]
Google_Global_revenue<-sum(Google_Global_rev$Amount)
Google_Global_cost<-sum(Google_Global_rev$'Total Cost')
Google_Global_Net <-Google_Global_revenue-Google_Global_cost
Google_Global_ROA <- Google_Global_Net/Google_Global_cost
Google_Global_CTR <- round(mean(Google_Global_rev$`Engine Click Thru %`),2)
Google_Global_TCR <- round(mean(Google_Global_rev$`Trans. Conv. %` ),2)
Google_Global_prob_of_booking <- round(mean(( Google_Global_rev$`Trans. Conv. %` 
                                              *Google_Global_rev$`Engine Click Thru %`) 
                                            /100),2)

#return on advert by Google Us
Google_Us_rev<-my_data[which(my_data$`Publisher Name` =="Google - US"),]
Google_Us_revenue<-sum(Google_Us_rev$Amount)
Google_Us_cost<-sum(Google_Us_rev$'Total Cost')
Google_Us_Net <- Google_Us_revenue-Google_Us_cost
Google_Us_ROA <- Google_Us_Net/Google_Us_cost
Google_Us_CTR <- round(mean(Google_Us_rev$`Engine Click Thru %`),2)
Google_Us_TCR <- round(mean(Google_Us_rev$`Trans. Conv. %` ),2)
Google_Us_prob_of_booking <- round(mean(( Google_Us_rev$`Trans. Conv. %` * 
                                            Google_Us_rev$`Engine Click Thru %`) 
                                        /100),2)

#return on advert by Ovature us
Ov_US_rev<-my_data[which(my_data$`Publisher Name` =="Overture - US"),]
Ov_us_revenue<-sum(Ov_US_rev$Amount)
Ov_us_cost<-sum(Ov_US_rev$'Total Cost')
Ov_us_Net <- Ov_us_revenue-Ov_us_cost
Ov_us_ROA <- Ov_us_Net/Ov_us_cost
Ov_us_CTR <- round(mean(Ov_US_rev$`Engine Click Thru %`),2)
Ov_us_TCR <- round(mean(Ov_US_rev$`Trans. Conv. %` ),2)
Ov_us_prob_of_booking <- round(mean(( Ov_US_rev$`Trans. Conv. %` * 
                                        Ov_US_rev$`Engine Click Thru %`) 
                                    /100),2)

#return on advert by MSN US
MSN_US_rev<-my_data[which(my_data$`Publisher Name` =="MSN - US"),]
MSN_US_revenue<-sum(MSN_US_rev$Amount)
MSN_US_cost<-sum(MSN_US_rev$'Total Cost')
MSN_US_Net <- MSN_US_revenue-MSN_US_cost
MSN_US_ROA <- MSN_US_Net/MSN_US_cost
MSN_US_CTR <- round(mean(MSN_US_rev$`Engine Click Thru %`),2)
MSN_US_TCR <- round(mean(MSN_US_rev$`Trans. Conv. %` ),2)
MSN_US_prob_of_booking <- round(mean(( MSN_US_rev$`Trans. Conv. %` * 
                                         MSN_US_rev$`Engine Click Thru %`) 
                                     /100),2)
#return on advert by Kayak
Kayak_rev<-my_data[which(my_data$`Publisher Name` =="MSN - US"),]
Kayak_revenue<- round(My_data_k$`Total Revenue`,2)
Kayak_cost<- round(My_data_k$`Media Cost`, 2)
Kayak_Net <- round((Kayak_revenue-Kayak_cost),2)
Kayak_ROA <- Kayak_Net/Kayak_cost
Kayak_TCR <- round((My_data_k$`Total Bookings`/My_data_k$Clicks)*100,2)


##################################
#plotting the results    #########
##################################

#plotting results for publisher revenue
x <- c('Google - Global','Google - US','MSN - Global' ,
       'MSN - US','Overture - Global',' Overture - US','Yahoo - US', 'Kayak')
y <- c(Google_Global_Net,Google_Us_Net,MSN_Net,MSN_US_Net,ovature_Net,Ov_us_Net,
       yahoo_Net, Kayak_Net)
revenue_publisher_name <- data.frame(x, y)

Revenue_by_Publisher <- plot_ly(revenue_publisher_name, x= ~x, y= ~y, type = "bar", 
                                name = "Revenue by Publisher($)", 
                                color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Revenue by Publisher($)",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

Revenue_by_Publisher

#plotting results for publisher ROA
x <- c('Google - Global','Google - US','MSN - Global' ,
       'MSN - US','Overture - Global',' Overture - US','Yahoo - US', 'Kayak')
y <- c(Google_Global_ROA,Google_Us_ROA,MSN_ROA,MSN_US_ROA,ovature_ROA,Ov_us_ROA,
       yahoo_ROA, Kayak_ROA)
ROA_publisher_name <- data.frame(x, y)

ROA_by_Publisher <- plot_ly(ROA_publisher_name , x= ~x, y= ~y, type = "bar", 
                            name = "ROA by Publisher", 
                            color = I("blue"), alpha = 0.5, width = 0.5) %>%
  layout(title = "ROA by Publisher",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

ROA_by_Publisher 


# Ploting probability of booking
x <- c('Google - Global','Google - US','MSN - Global' ,'MSN - US',
       'Overture - Global',' Overture - US','Yahoo - US')
y <- c(Google_Global_prob_of_booking,Google_Us_prob_of_booking,
       MSN_prob_of_booking,MSN_US_prob_of_booking,ovature_prob_of_booking,
       Ov_us_prob_of_booking,yahoo_prob_of_booking)
prob_of_booking_publisher_name <- data.frame(x, y)

prob_of_booking <- plot_ly(prob_of_booking_publisher_name, x= ~x, y= ~y, type = "bar",  
                           name = "Average Probability of booking", 
                           color = I("blue4"), alpha = 0.5, 
                           width = 0.5) %>%
  layout(title = "Average Probability of booking",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

prob_of_booking

# Plotting Engine click through rate
x <- c('Google - Global','Google - US','MSN - Global' ,'MSN - US',
       'Overture - Global',' Overture - US','Yahoo - US')
y <- c(Google_Global_CTR,Google_Us_CTR,MSN_CTR,MSN_US_CTR,ovature_CTR,
       Ov_us_CTR, yahoo_CTR)
ctrate <- data.frame(x, y)

Click_through_rate <- plot_ly(ctrate, x= ~x, y= ~y, type = "bar",  
                              name = " Average Click through rate", 
                              color = I("green4"), alpha = 0.5, 
                              width = 0.5) %>%
  layout(title = "Average Click through rate",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

Click_through_rate

# Plotting Transaction conversion rate
x <- c('Google - Global','Google - US','MSN - Global' ,'MSN - US',
       'Overture - Global',' Overture - US','Yahoo - US', 'Kayak')
y <- c(Google_Global_TCR,Google_Us_TCR,MSN_TCR,MSN_US_TCR,ovature_TCR,
       Ov_us_TCR, yahoo_TCR, Kayak_TCR)
tcrate <- data.frame(x, y)

Transaction_conversion_rate <- plot_ly(tcrate, x= ~x, y= ~y, type = "bar",  
                                       name = "Average Transaction conversion rate", 
                                       color = I("purple"), alpha = 0.5, 
                                       width = 0.5) %>%
  layout(title = "Average Transaction conversion rate",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

Transaction_conversion_rate





##################################
##sub setting by Campaign Region #
##################################

##European / general campaign####

Other<- c('Business Class','Google_Yearlong 2006','Unassigned','General Terms')
France<- c('Air France Brand & French Destinations','Paris & France Terms',
           'Air France Branded','French Destinations','Air France Global Campaign')
west_euro<- c('Outside Western Europe','Western Europe Destinations')



western_euro_rev_sub<-my_data[which(my_data$Campaign =="Outside Western Europe"|
                                      my_data$Campaign =="Western Europe Destinations" ),]
western_revenue_sub<-sum(western_euro_rev_sub$Revenue)
western_revenue_sub
#calculating Roa
western_costs <- sum(western_euro_rev_sub$`Total Cost`)
western_ROA<- round((western_revenue_sub / western_costs)*100,2)
western_ROA 
#ctr & tCr
EU_western_CTR<-mean(western_euro_rev_sub$`Trans. Conv. %`)
EU_western_TCR<-mean(western_euro_rev_sub$`Engine Click Thru %`)

france_rev_sub<-my_data[which(my_data$Campaign == 'Air France Brand & French Destinations' | 
                                my_data$Campaign =='Paris & France Terms' |
                                my_data$Campaign =='Air France Branded' |
                                my_data$Campaign =='French Destinations' |
                                my_data$Campaign =='Air France Global Campaign'),]
france_revenue_sub<-sum(france_rev_sub$Revenue)
france_revenue_sub
#calculating Roa
france_costs_sub<- sum(france_rev_sub$`Total Cost`)
france_ROA<- (france_revenue_sub / france_costs_sub)*100
france_ROA
#ctr & tCr
EU_france_CTR<-mean(france_rev_sub$`Trans. Conv. %`)
EU_france_TCR<-mean(france_rev_sub$`Engine Click Thru %`)

Other_rev_sub<-my_data[which(my_data$Campaign == 'Business Class' | 
                               my_data$Campaign =='Google_Yearlong 2006' |
                               my_data$Campaign =='Unassigned' |
                               my_data$Campaign =='General Terms'),]
other_revenue_sub<-sum(Other_rev_sub$Revenue)
other_revenue_sub
#calculating Roa
other_costs_sub <- sum(Other_rev_sub$`Total Cost`)
other_ROA<- (other_revenue_sub / other_costs_sub)*100
other_ROA
#ctr & tCr
EU_other_CTR<-mean(Other_rev_sub$`Trans. Conv. %`)
EU_other_TCR<-mean(Other_rev_sub$`Engine Click Thru %`)

### U.S regions ###
east<- c('Geo Targeted New York','Geo Targeted Boston','Geo Targeted Philadelphia')
midwest<- c('Geo Targeted Cincinnati','Geo Targeted Chicago','Geo Targeted Detroit')
south<- c('Geo Targeted Atlanta','Geo Targeted Houston','Geo Targeted DC,Geo Targeted Miami')
west<- c('Geo Targeted Seattle','Geo Targeted Los Angeles','Geo Targeted San Francisco')


#####------WEST US
west_us_sub<-my_data[which(my_data$Campaign =='Geo Targeted Seattle'|
                             my_data$Campaign =='Geo Targeted Los Angeles'|
                             my_data$Campaign == 'Geo Targeted San Francisco'),]


west_us_costs<- sum(west_us_sub$`Total Cost`)
west_us_revenue<- sum(west_us_sub$Revenue) 
west_us_ROA<- (west_us_revenue /west_us_costs)*100
west_us_ROA
#ctr & tCr
US_west_CTR<- mean(west_us_sub$`Trans. Conv. %`)
US_West_ECT<-mean(west_us_sub$`Engine Click Thru %`)

#####------East US
east_us_sub<-my_data[which(my_data$Campaign =='Geo Targeted New York'|
                             my_data$Campaign =='Geo Targeted Boston'|
                             my_data$Campaign =='Geo Targeted Philadelphia'),]

east_us_costs<- sum(east_us_sub$`Total Cost`)
east_us_revenue<- sum(east_us_sub$Revenue)
east_us_ROA<- (east_us_revenue / east_us_costs)*100
east_us_ROA
#ctr & tCr
US_east_CTR<- mean(east_us_sub$`Trans. Conv. %`)
US_east_ECT<-mean(east_us_sub$`Engine Click Thru %`)

#####------midwest US

midwest_us_sub<-my_data[which(my_data$Campaign =='Geo Targeted Cincinnati'|
                                my_data$Campaign =='Geo Targeted Chicago'|
                                my_data$Campaign =='Geo Targeted Detroit'),]

midwest_us_costs<- sum(midwest_us_sub$`Total Cost`)
midwest_us_revenue<- sum(midwest_us_sub$Revenue)
midwest_us_ROA<- (midwest_us_revenue / midwest_us_costs)*100
midwest_us_ROA
#ctr & tCr
US_midwest_CTR<- mean(midwest_us_sub$`Trans. Conv. %`)
US_midwest_ECT<-mean(midwest_us_sub$`Engine Click Thru %`)

#####------south US

south_us_sub<-my_data[which(my_data$Campaign =='Geo Targeted Atlanta'|
                              my_data$Campaign =='Geo Targeted Houston'|
                              my_data$Campaign =='Geo Targeted DC'|
                              my_data$Campaign =='Geo Targeted Miami'),]

south_us_costs<- sum(south_us_sub$`Total Cost`)
south_us_revenue<- sum(south_us_sub$Revenue)
south_us_ROA<- (south_us_revenue / south_us_costs)*100
south_us_ROA
#ctr & tCr
US_south_CTR<- mean(south_us_sub$`Trans. Conv. %`)
US_south_ECT<-mean(south_us_sub$`Engine Click Thru %`)

##################################
#plotting the results    #########
##################################

#plotting results for revenue by region Europe
p <- c("Western_Europe","France","Other")
l <- c(western_revenue_sub,france_revenue_sub,other_revenue_sub)
revenue_region <- data.frame(p,l)


#plotting results for campaign revenue Europe
Campaign_Region_Revenue <- plot_ly(revenue_region, x= ~p, y= ~l, type = "bar", 
                                   name = "Campaign Region Revenue Europe($)", 
                                   color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Campaign Region Revenue Europe($)",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#calling to plot the graph
Campaign_Region_Revenue

#plotting results for revenue by region US
p <- c("Us-east","Us-midwest","Us-south","Us-west")
l <- c(east_us_revenue,midwest_us_revenue,south_us_revenue,west_us_revenue)
revenue_regionx <- data.frame(p,l)


#plotting results for campaign revenue US
Campaign_Region_Revenue_f <- plot_ly(revenue_regionx, x= ~p, y= ~l, type = "bar", 
                                     name = "Campaign Region Revenue US($)", 
                                     color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Campaign Region Revenue US($)",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#calling to plot the graph
Campaign_Region_Revenue_f



#plotting results for ROA by region
p <- c("Us-east","Us-midwest","Us-south","Us-west","Western_Europe","France","Other")
l <- c(east_us_ROA,midwest_us_ROA,south_us_ROA,west_us_ROA,
       western_ROA,france_ROA,other_ROA)
roa_region <- data.frame(p,l)


#plotting results for campaign revenue
Campaign_Region_ROA <- plot_ly(roa_region, x= ~p, y= ~l, type = "bar", 
                               name = "Campaign Region ROA", 
                               color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Campaign Region ROA",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#calling to plot the graph
Campaign_Region_ROA











###############################################################################
###############################################################################
###############################################################################
###############################################################################
####### Analyzing the PROFITABLE Keywords
profitable <-my_data[which(my_data$Revenue > 0 & my_data$ROA > 3 ),]
profitable_num <- nrow(profitable)
total_num <- nrow(my_data)
profit_ratio_keywords <- (profitable_num/total_num) * 100



##################################
#Key Factor Summary              #
##################################
# Getting summary statistics for whole data set

Key_factorsx<- c("Min","Mean","SD","Median","Mode","Max")
clicksx<- c(min(profitable$Clicks),round(mean(profitable$Clicks),2),
           round(sd(profitable$Clicks),2),median(profitable$Clicks),
           mode(profitable$Clicks),max(profitable$Clicks))
Impressionsx<- c(min(profitable$Impressions),round(mean(profitable$Impressions),2),
                round(sd(profitable$Impressions),2),median(profitable$Impressions),
                mode(profitable$Impressions),max(profitable$Impressions))
Amountx<- c(min(profitable$Amount),round(mean(profitable$Amount),2),
           round(sd(profitable$Amount),2),median(profitable$Amount),
           mode(profitable$Amount),max(profitable$Amount))
Total_Costx<- c(min(profitable$`Total Cost`),round(mean(profitable$`Total Cost`),2),
               round(sd(profitable$`Total Cost`),2),
               round(median(profitable$`Total Cost`),2),mode(profitable$`Total Cost`),
               round(max(profitable$`Total Cost`),2))
Revenuex<- c(round(min(profitable$Revenue),2),round(mean(profitable$Revenue),2),
            round(sd(profitable$Revenue),2),round(median(profitable$Revenue),2),
            mode(profitable$Revenue),round(max(profitable$Revenue),2))
#Making summary statistics into a data frame
key_factor_summaryx <- as.data.frame(cbind(Key_factorsx, clicksx, Impressionsx, 
                                          Amountx, Total_Costx,Revenuex))
key_factor_summaryx




##################################
#sub setting publisher revenue   #
##################################

#return on advert by yahoo 
Yahoo_revx<-profitable[which(profitable$`Publisher Name` =="Yahoo - US"),]
yahoo_revenuex<-sum(Yahoo_revx$Amount)
yahoo_costx<-sum(Yahoo_revx$'Total Cost')
yahoo_Netx <- yahoo_revenuex-yahoo_costx
yahoo_ROAx <- yahoo_Netx/yahoo_costx
yahoo_CTRx <- round(mean(Yahoo_revx$`Engine Click Thru %`),2)
yahoo_TCRx <- round(mean(Yahoo_revx$`Trans. Conv. %` ),2)
yahoo_prob_of_bookingx <- round(mean((Yahoo_revx$`Trans. Conv. %` * 
                                       Yahoo_revx$`Engine Click Thru %`) 
                                    /100),2)

#return on advert by MSN 
MSN_revx<-profitable[which(profitable$`Publisher Name` =="MSN - Global"),]
MSN_revenuex<-sum(MSN_revx$Amount)
MSN_costx<-sum(MSN_revx$'Total Cost')
MSN_Netx <-MSN_revenuex-MSN_costx
MSN_ROAx <- MSN_Netx/MSN_costx
MSN_CTRx <- round(mean(MSN_revx$`Engine Click Thru %`),2)
MSN_TCRx <- round(mean(MSN_revx$`Trans. Conv. %` ),2)
MSN_prob_of_bookingx <- round(mean(( MSN_revx$`Trans. Conv. %` * 
                                      MSN_revx$`Engine Click Thru %`) 
                                  /100),2)

#return on advert by Ovature 
ovature_revx<-profitable[which(profitable$`Publisher Name` =="Overture - Global"),]
ovature_revenuex<-sum(ovature_revx$Amount)
ovature_costx<-sum(ovature_revx$'Total Cost')
ovature_Netx <-ovature_revenuex-ovature_costx
ovature_ROAx <- ovature_Netx/ovature_costx
ovature_CTRx <- round(mean(ovature_revx$`Engine Click Thru %`),2)
ovature_TCRx <- round(mean(ovature_revx$`Trans. Conv. %` ),2)
ovature_prob_of_bookingx <- round(mean(( ovature_revx$`Trans. Conv. %` * 
                                          ovature_revx$`Engine Click Thru %`) 
                                      /100),2)

#return on advert by Google Global 
Google_Global_revx<-profitable[which(profitable$`Publisher Name` =="Google - Global"),]
Google_Global_revenuex<-sum(Google_Global_revx$Amount)
Google_Global_costx<-sum(Google_Global_revx$'Total Cost')
Google_Global_Netx <-Google_Global_revenuex-Google_Global_costx
Google_Global_ROAx <- Google_Global_Netx/Google_Global_costx
Google_Global_CTRx <- round(mean(Google_Global_revx$`Engine Click Thru %`),2)
Google_Global_TCRx <- round(mean(Google_Global_revx$`Trans. Conv. %` ),2)
Google_Global_prob_of_bookingx <- round(mean(( Google_Global_revx$`Trans. Conv. %` 
                                              *Google_Global_revx$`Engine Click Thru %`) 
                                            /100),2)

#return on advert by Google Us
Google_Us_revx<-profitable[which(profitable$`Publisher Name` =="Google - US"),]
Google_Us_revenuex<-sum(Google_Us_revx$Amount)
Google_Us_costx<-sum(Google_Us_revx$'Total Cost')
Google_Us_Netx <- Google_Us_revenuex-Google_Us_costx
Google_Us_ROAx <- Google_Us_Netx/Google_Us_costx
Google_Us_CTRx <- round(mean(Google_Us_revx$`Engine Click Thru %`),2)
Google_Us_TCRx <- round(mean(Google_Us_revx$`Trans. Conv. %` ),2)
Google_Us_prob_of_bookingx <- round(mean(( Google_Us_revx$`Trans. Conv. %` * 
                                            Google_Us_revx$`Engine Click Thru %`) 
                                        /100),2)

#return on advert by Ovature us
Ov_US_revx<-profitable[which(profitable$`Publisher Name` =="Overture - US"),]
Ov_us_revenuex<-sum(Ov_US_revx$Amount)
Ov_us_costx<-sum(Ov_US_revx$'Total Cost')
Ov_us_Netx <- Ov_us_revenuex-Ov_us_costx
Ov_us_ROAx <- Ov_us_Netx/Ov_us_costx
Ov_us_CTRx <- round(mean(Ov_US_revx$`Engine Click Thru %`),2)
Ov_us_TCRx <- round(mean(Ov_US_revx$`Trans. Conv. %` ),2)
Ov_us_prob_of_bookingx <- round(mean(( Ov_US_revx$`Trans. Conv. %` * 
                                        Ov_US_revx$`Engine Click Thru %`) 
                                    /100),2)

#return on advert by MSN US
MSN_US_revx<-profitable[which(profitable$`Publisher Name` =="MSN - US"),]
MSN_US_revenuex<-sum(MSN_US_revx$Amount)
MSN_US_costx<-sum(MSN_US_revx$'Total Cost')
MSN_US_Netx <- MSN_US_revenuex-MSN_US_costx
MSN_US_ROAx <- MSN_US_Netx/MSN_US_costx
MSN_US_CTRx <- round(mean(MSN_US_revx$`Engine Click Thru %`),2)
MSN_US_TCRx <- round(mean(MSN_US_revx$`Trans. Conv. %` ),2)
MSN_US_prob_of_bookingx <- round(mean(( MSN_US_revx$`Trans. Conv. %` * 
                                         MSN_US_revx$`Engine Click Thru %`) 
                                     /100),2)


##################################
#plotting the results    #########
##################################

#plotting results for publisher revenue -profitable
x <- c('Google - Global','Google - US','MSN - Global' ,
       'MSN - US','Overture - Global',' Overture - US','Yahoo - US', 'Kayak')
y <- c(Google_Global_Netx,Google_Us_Netx,MSN_Netx,MSN_US_Netx,ovature_Netx,
       Ov_us_Netx,yahoo_Netx, Kayak_Net)
revenue_publisher_namex <- data.frame(x, y)

Revenue_by_Publisher_profitable <- plot_ly(revenue_publisher_namex, x= ~x, y= ~y, type = "bar", 
                                name = "Revenue by Publisher - Profitable($)", 
                                color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Revenue by Publisher- Profitable($)",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

Revenue_by_Publisher_profitable

#plotting results for publisher ROA - profitable
x <- c('Google - Global','Google - US','MSN - Global' ,
       'MSN - US','Overture - Global',' Overture - US','Yahoo - US', 'Kayak')
y <- c(Google_Global_ROAx,Google_Us_ROAx,MSN_ROAx,MSN_US_ROAx,ovature_ROAx,
       Ov_us_ROAx,yahoo_ROAx, Kayak_ROA)
ROA_publisher_namex <- data.frame(x, y)

ROA_by_Publisher_profitable <- plot_ly(ROA_publisher_namex , x= ~x, y= ~y, type = "bar", 
                            name = "ROA by Publisher - Profitable", 
                            color = I("blue"), alpha = 0.5, width = 0.5) %>%
  layout(title = "ROA by Publisher - Profitable",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

ROA_by_Publisher_profitable 


# Plotting probability of booking
x <- c('Google - Global','Google - US','MSN - Global' ,'MSN - US',
       'Overture - Global',' Overture - US','Yahoo - US')
y <- c(Google_Global_prob_of_bookingx,Google_Us_prob_of_bookingx,
       MSN_prob_of_bookingx,MSN_US_prob_of_bookingx,ovature_prob_of_bookingx,
       Ov_us_prob_of_bookingx,yahoo_prob_of_bookingx)
prob_of_booking_publisher_namex <- data.frame(x, y)

prob_of_booking_profitable <- plot_ly(prob_of_booking_publisher_namex, x= ~x, y= ~y, type = "bar",  
                           name = "Average Probability of booking - Profitable", 
                           color = I("blue4"), alpha = 0.5, 
                           width = 0.5) %>%
  layout(title = "Average Probability of booking - Profitable",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

prob_of_booking_profitable

# Plotting Engine click through rate -profitable
x <- c('Google - Global','Google - US','MSN - Global' ,'MSN - US',
       'Overture - Global',' Overture - US','Yahoo - US')
y <- c(Google_Global_CTRx,Google_Us_CTRx,MSN_CTRx,MSN_US_CTRx,ovature_CTRx,
       Ov_us_CTRx, yahoo_CTRx)
ctratex <- data.frame(x, y)

Click_through_rate_profitable <- plot_ly(ctratex, x= ~x, y= ~y, type = "bar",  
                              name = " Average Click through rate - Profitable", 
                              color = I("green4"), alpha = 0.5, 
                              width = 0.5) %>%
  layout(title = "Average Click through rate - Profitable",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

Click_through_rate_profitable

# Plotting Transaction conversion rate profitable
x <- c('Google - Global','Google - US','MSN - Global' ,'MSN - US',
       'Overture - Global',' Overture - US','Yahoo - US', 'Kayak')
y <- c(Google_Global_TCRx,Google_Us_TCRx,MSN_TCRx,MSN_US_TCRx,ovature_TCRx,
       Ov_us_TCRx, yahoo_TCRx, Kayak_TCR)
tcratex <- data.frame(x, y)

Transaction_conversion_rate_profitable <- plot_ly(tcratex, x= ~x, y= ~y, type = "bar",  
                                       name = "Average Transaction conversion rate - Profitable", 
                                       color = I("purple"), alpha = 0.5, 
                                       width = 0.5) %>%
  layout(title = "Average Transaction conversion rate - Profitable",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

Transaction_conversion_rate_profitable





##################################
##sub setting by Campaign Region #
##################################

##European / general campaign####profitable


western_euro_rev_subx<-profitable[which(profitable$Campaign =="Outside Western Europe"|
                                          profitable$Campaign =="Western Europe Destinations" ),]
western_revenue_subx<-sum(western_euro_rev_subx$Revenue)
western_revenue_subx
#calculating Roa profitable
western_costsx <- sum(western_euro_rev_subx$`Total Cost`)
western_ROAx<- round((western_revenue_subx / western_costsx)*100,2)
western_ROAx 
#ctr & tCr profitable
EU_western_CTRx<-mean(western_euro_rev_subx$`Trans. Conv. %`)
EU_western_TCRx<-mean(western_euro_rev_subx$`Engine Click Thru %`)

france_rev_subx<-profitable[which(profitable$Campaign == 'Air France Brand & French Destinations' | 
                                    profitable$Campaign =='Paris & France Terms' |
                                    profitable$Campaign =='Air France Branded' |
                                    profitable$Campaign =='French Destinations' |
                                    profitable$Campaign =='Air France Global Campaign'),]
france_revenue_subx<-sum(france_rev_subx$Revenue)
france_revenue_subx
#calculating Roa profitable
france_costs_subx<- sum(france_rev_subx$`Total Cost`)
france_ROAx<- (france_revenue_subx / france_costs_subx)*100
france_ROAx
#ctr & tCr profitable
EU_france_CTRx<-mean(france_rev_subx$`Trans. Conv. %`)
EU_france_TCRx<-mean(france_rev_subx$`Engine Click Thru %`)

Other_rev_subx<-profitable[which(profitable$Campaign == 'Business Class' | 
                                   profitable$Campaign =='Google_Yearlong 2006' |
                                   profitable$Campaign =='Unassigned' |
                                   profitable$Campaign =='General Terms'),]
other_revenue_subx<-sum(Other_rev_subx$Revenue)
other_revenue_subx
#calculating Roa profitable
other_costs_subx <- sum(Other_rev_subx$`Total Cost`)
other_ROAx<- (other_revenue_subx / other_costs_subx)*100
other_ROAx
#ctr & tCr profitable
EU_other_CTRx<-mean(Other_rev_subx$`Trans. Conv. %`)
EU_other_TCRx<-mean(Other_rev_subx$`Engine Click Thru %`)


#####------WEST US profitable
west_us_subx<-profitable[which(profitable$Campaign =='Geo Targeted Seattle'|
                                 profitable$Campaign =='Geo Targeted Los Angeles'|
                                 profitable$Campaign == 'Geo Targeted San Francisco'),]


west_us_costsx<- sum(west_us_subx$`Total Cost`)
west_us_revenuex<- sum(west_us_subx$Revenue) 
west_us_ROAx<- (west_us_revenuex /west_us_costsx)*100
west_us_ROAx
#ctr & tCr profitable
US_west_CTRx<- mean(west_us_subx$`Trans. Conv. %`)
US_West_ECTx<-mean(west_us_subx$`Engine Click Thru %`)

#####------xEast US profitable
east_us_subx<-profitable[which(profitable$Campaign =='Geo Targeted New York'|
                                 profitable$Campaign =='Geo Targeted Boston'|
                                 profitable$Campaign =='Geo Targeted Philadelphia'),]

east_us_costsx<- sum(east_us_subx$`Total Cost`)
east_us_revenuex<- sum(east_us_subx$Revenue)
east_us_ROAx<- (east_us_revenuex / east_us_costsx)*100
east_us_ROAx
#ctr & tCr profitable
US_east_CTRx<- mean(east_us_subx$`Trans. Conv. %`)
US_east_ECTx<-mean(east_us_subx$`Engine Click Thru %`)

#####------midwest US profitable

midwest_us_subx<-profitable[which(profitable$Campaign =='Geo Targeted Cincinnati'|
                                   profitable$Campaign =='Geo Targeted Chicago'|
                                   profitable$Campaign =='Geo Targeted Detroit'),]

midwest_us_costsx<- sum(midwest_us_subx$`Total Cost`)
midwest_us_revenuex<- sum(midwest_us_subx$Revenue)
midwest_us_ROAx<- (midwest_us_revenuex / midwest_us_costsx)*100
midwest_us_ROAx
#ctr & tCr
US_midwest_CTRx<- mean(midwest_us_subx$`Trans. Conv. %`)
US_midwest_ECTx<-mean(midwest_us_subx$`Engine Click Thru %`)

#####------south US profitable

south_us_subx<-profitable[which(profitable$Campaign =='Geo Targeted Atlanta'|
                                 profitable$Campaign =='Geo Targeted Houston'|
                                 profitable$Campaign =='Geo Targeted DC'|
                                 profitable$Campaign =='Geo Targeted Miami'),]

south_us_costsx<- sum(south_us_subx$`Total Cost`)
south_us_revenuex<- sum(south_us_subx$Revenue)
south_us_ROAx<- (south_us_revenuex / south_us_costsx)*100
south_us_ROAx
#ctr & tCr
US_south_CTRx<- mean(south_us_subx$`Trans. Conv. %`)
US_south_ECTx<-mean(south_us_subx$`Engine Click Thru %`)

##################################
#plotting the results    #########
##################################

#plotting results for revenue by region profitable
p <- c("Us-east","Us-midwest","Us-south","Us-west","Western_Europe","France","Other")
l <- c(east_us_revenuex,midwest_us_revenuex,south_us_revenuex,west_us_revenuex,
       western_revenue_subx,france_revenue_subx,other_revenue_subx)
revenue_regionx <- data.frame(p,l)


#plotting results for campaign revenue
Campaign_Region_Revenue_profitable <- plot_ly(revenue_regionx, x= ~p, y= ~l, type = "bar", 
                                   name = "Campaign Region Revenue - Profitable($)", 
                                   color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Campaign Region Revenue - Profitable($)",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#calling to plot the graph profitable
Campaign_Region_Revenue_profitable




#plotting results for ROA by region profitable
p <- c("Us-east","Us-midwest","Us-south","Us-west","Western_Europe","France","Other")
l <- c(east_us_ROAx,midwest_us_ROAx,south_us_ROAx,west_us_ROAx,
       western_ROAx,france_ROAx,other_ROAx)
roa_regionx <- data.frame(p,l)


#plotting results for campaign revenue
Campaign_Region_ROA_profitable <- plot_ly(roa_regionx, x= ~p, y= ~l, type = "bar", 
                               name = "Campaign Region ROA - Profitable", 
                               color = I("orange"), alpha = 0.5, width = 0.5) %>%
  layout(title = "Campaign Region ROA - Profitable",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

#calling to plot the graph profitable
Campaign_Region_ROA_profitable






