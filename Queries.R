library(dplyr)
library(writexl)

#reading data from datasets
continents <- read.csv('datasets/Countries-Continents.csv')
heights <- read.csv('datasets/height_weight_data.csv')
iq <- read.csv('datasets/iq.csv')
life_exp <- read.csv('datasets/life_expectancy.csv')
pop_dens <- read.csv('datasets/population_density.csv')
pop_change <- read.csv('datasets/pop_change.csv')

#joining the datasets on basis of country
df1 <- merge(x=heights,y=pop_dens,by="country",all.x=TRUE)
df2 <- merge(x=df1,y=iq,by="country",all.x=TRUE)
df3 <- merge(x=df2,y=life_exp,by="country",all.x=TRUE)
df4 <- merge(x=df3,y=continents,by="country",all.x=TRUE)
df <- merge(x=df4,y=pop_change,by="country",all.x=TRUE)

#listing the countries whose continent data is not available
df$country[is.na(df$continent)]

#manually adding information of countries whose continent data is missing
df$continent[df$country=="Czechia"]="Europe"
df$continent[df$country=="Bermuda"]="North America"
df$continent[df$country=="Hong Kong"]="Asia"
df$continent[df$country=="Russia"]="Asia"
df$continent[df$country=="American Samoa"]="Oceania"
df$continent[df$country=="Cook Islands"]="Oceania"
df$continent[df$country=="French Polynesia"]="Oceania"
df$continent[df$country=="Taiwan"]="Asia"
df$continent[df$country=="Niue"]="Oceania"
df$continent[df$country=="Puerto Rico"]="North America"
df$continent[df$country=="Tokelau"]="Oceania"

#removing the unit information from population
#For example the population data of India before preprocessing is "1393.41 M" 
#but after running the query it will convert to "1393.41"
df$population <- sapply(df$population, function(x) substring(x, 1, nchar(x) - 2))

#converting population data type from string to double format
df$population <- as.double(df$population)

#
df$population[df$country=="China"]=1412.36
#
df$population[df$country=="India"]=1393.41

#grouping countries by continent
df_cont <- group_by(df,continent)

#replacing the NA values of male height with mean value of male heights w.r.t continent
df_male_height <- summarise(df_cont,height = mean(male_height, na.rm = TRUE))
for(x in df_male_height$continent)
{
  a <- is.na(df$male_height)
  b <- df$continent==x 
  df$male_height[a&b] = trunc(df_male_height$height[df_male_height$continent==x])
}

#replacing the NA values of female height with mean value of female heights w.r.t continent
df_female_height<-summarise(df_cont,height=mean(female_height, na.rm = TRUE))
for(x in df_female_height$continent)
{
  a<-is.na(df$female_height)
  b<-df$continent==x 
  df$female_height[a&b]=trunc(df_female_height$height[df_female_height$continent==x])
}

#replacing the NA values of male weight with mean value of male weights w.r.t continent
df_male_weight<-summarise(df_cont,weight=mean(male_weight, na.rm = TRUE))
for(x in df_male_weight$continent)
{
  a<-is.na(df$male_weight)
  b<-df$continent==x 
  df$male_weight[a&b]=round(df_male_weight$weight[df_male_weight$continent==x],1)
}

#replacing the NA values of female weight with mean value of female weights w.r.t continent
df_female_weight<-summarise(df_cont,weight=mean(female_weight, na.rm = TRUE))
for(x in df_female_weight$continent)
{
  a<-is.na(df$female_weight)
  b<-df$continent==x 
  df$female_weight[a&b]=round(df_female_weight$weight[df_female_weight$continent==x],1)
}

#adding female BMI information by using the formula: weight/(height)^2
df <- mutate(df,female_bmi=round(female_weight*100*100/(female_height*female_height),1))

#replacing the NA values of male BMI with the the value calculated using the formula: weight/(height)^2
df$male_bmi <- round(mapply(function(x,y,z) if(is.na(x)){y*100*100/(z*z)} else {x},df$male_bmi,df$male_weight,df$male_height),1)

#replacing the NA values of IQ with its mean value across the world
df$iq[is.na(df$iq)] <- round(mean(df$iq, na.rm=TRUE), 1)

#converting education expenditure from string to numeric format
df$education_expenditure_per_inhabitant <- as.numeric(df$education_expenditure_per_inhabitant)

#replacing the NA values of education expenditure with its median value across the world
df$education_expenditure_per_inhabitant[is.na(df$education_expenditure_per_inhabitant)] <- 
  round(median(df$education_expenditure_per_inhabitant, na.rm=TRUE),0)

#replacing the NA values of max temperature with mean value of max temperature w.r.t continent
df_temp<-summarise(df_cont,temp=mean(daily_max_temp, na.rm = TRUE))
for(x in df_temp$continent)
{
  a<-is.na(df$daily_max_temp)
  b<-df$continent==x 
  df$daily_max_temp[a&b]=round(df_temp$temp[df_temp$continent==x],1)
}

#replacing the NA values of male life expectancy with its median value across the world
df$male_life_expectancy[is.na(df$male_life_expectancy)] <- round(median(df$male_life_expectancy, na.rm=TRUE),1)

#replacing the NA values of female life expectancy with its median value across the world
df$female_life_expectancy[is.na(df$female_life_expectancy)] <- round(median(df$female_life_expectancy, na.rm=TRUE),1)

#replacing the NA values of birth rate with its median value across the world
df$birth_rate[is.na(df$birth_rate)] <- round(median(df$birth_rate, na.rm=TRUE),1)

#replacing the NA values of death rate with its median value across the world
df$death_rate[is.na(df$death_rate)] <- round(median(df$death_rate, na.rm=TRUE),1)

#replacing the NA values of area of a country with its mean of total area covered by countries across the world
df$area[is.na(df$area)] <- round(mean(df$area, na.rm=TRUE),1)

#replacing the NA values of a country's population with mean of country population w.r.t the continent in which it is situated
df_pop<-summarise(df_cont,pop=mean(population, na.rm = TRUE))
for(x in df_pop$continent)
{
  a<-is.na(df$population)
  b<-df$continent==x 
  df$population[a&b]=round(df_pop$pop[df_pop$continent==x],1)
}

#replacing the missing values of population per sq km with the the value calculated using country population and its area 
df$pop_per_km_sq <- round(mapply(function(x,y,z) if(is.na(x)){y*1000000/z} else {x},df$pop_per_km_sq,df$population,df$area),1)

#reordering the columns 
df <- df[,c(1,17,7:9,18,12,2:6,19,13:16,10)]

sheet1 <- df[,c('continent', 'country', 'birth_rate', 'death_rate')]
write_xlsx(sheet1,"sheet1.xlsx")

sheet2 <- df[,c('continent', 'country', 'population')]
write_xlsx(sheet2,"sheet2.xlsx")

sheet3 <- df[,c('continent', 'country', 'area')]
write_xlsx(sheet3,"sheet3.xlsx")

sheet4 <- df[,c('continent', 'country', 'male_height', 'female_height', 'male_weight', 'female_weight', 'male_bmi', 'female_bmi', 'male_life_expectancy', 'female_life_expectancy', 'iq')]
write_xlsx(sheet4,"sheet4.xlsx")

write_xlsx(sheet5,"sheet5.xlsx")
sheet5 <- df[,c('continent', 'country', 'area', 'daily_max_temp')]

write_xlsx(sheet6,"sheet6.xlsx")
sheet6 <- df[,c('continent', 'country', 'population', 'population_change')]

write_xlsx(sheet7,"sheet7.xlsx")
sheet7 <- df[,c('continent', 'country', 'population', 'population_change')]










