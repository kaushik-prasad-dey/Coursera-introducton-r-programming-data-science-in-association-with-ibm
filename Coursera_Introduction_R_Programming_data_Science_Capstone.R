
## Required Library Loaded for Calling Http Request in R.
library(tidyverse)
library(httr)
library(rvest)
## TASK 1: Get a COVID-19 pandemic Wiki page using HTTP request.
get_wiki_covid19_page <- function() {
  
  # Our target COVID-19 wiki page URL is: https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country  
  # Which has two parts: 
  # 1) base URL `https://en.wikipedia.org/w/index.php  
  # 2) URL parameter: `title=Template:COVID-19_testing_by_country`, seperated by question mark ?
  
  # Wiki page base
  wiki_base_url <-  "https://en.wikipedia.org/w/index.php"
  
  # You will need to create a List which has an element called `title` to specify which page you want to get from Wiki
  # in our case, it will be `Template:COVID-19_testing_by_country`
  wiki_params <- list(title = "Template:COVID-19_testing_by_country")
  
  
  # - Use the `GET` function in httr library with a `url` argument and a `query` arugment to get a HTTP response
  wiki_response <- GET(wiki_base_url, query = wiki_params) 
  
  # Use the `return` function to return the response
  return(wiki_response)
}
### Call the get_wiki_covid19_page function to get a http response with the target html page
wiki_covid19_page_response <- get_wiki_covid19_page()
print(wiki_covid19_page_response)
## Response [https://en.wikipedia.org/w/index.php?title=Template%3ACOVID-19_testing_by_country]
## Date: 2022-08-28 07:27
## Status: 200
## Content-Type: text/html; charset=UTF-8
## Size: 417 kB
## <!DOCTYPE html>
## <html class="client-nojs" lang="en" dir="ltr">
## <head>
## <meta charset="UTF-8"/>
## <title>Template:COVID-19 testing by country - Wikipedia</title>
## <script>document.documentElement.className="client-js";RLCONF={"wgBrea...
## "CS1 German-language sources (de)","CS1 Azerbaijani-language sources (...
## "CS1 uses Japanese-language script (ja)","CS1 Japanese-language source...
## "COVID-19 pandemic templates"],"wgPageContentLanguage":"en","wgPageCon...
## "wgEditSubmitButtonLabelPublish":true,"wgCentralAuthMobileDomain":fals...
## ...
## Task 2: Extract COVID-19 testing data table from the wiki HTML page.
## Get the root html node from the http response in task 1.
wiki_covid19_page_root_node <- read_html(wiki_covid19_page_response)
### Get the first table in the HTML root node using html_node function
# Get the table node from the root html node
wiki_covid19_page_table_node <- html_node(wiki_covid19_page_root_node, "table")
### Read the table node as a data frame using html_table function 
# Read the table node and convert it into a data frame, and print the data frame for review
wiki_covid19_page_data_frame <- html_table(wiki_covid19_page_table_node )
print(wiki_covid19_page_data_frame)
## # A tibble: 173 x 9
##    `Country or region` `Date[a]`   Tested      `Units[b]` `Confirmed(cases)`
##    <chr>               <chr>       <chr>       <chr>      <chr>             
##  1 Afghanistan         17 Dec 2020 154,767     samples    49,621            
##  2 Albania             18 Feb 2021 428,654     samples    96,838            
##  3 Algeria             2 Nov 2020  230,553     samples    58,574            
##  4 Andorra             23 Feb 2022 300,307     samples    37,958            
##  5 Angola              2 Feb 2021  399,228     samples    20,981            
##  6 Antigua and Barbuda 6 Mar 2021  15,268      samples    832               
##  7 Argentina           27 Mar 2022 35,271,802  samples    9,026,075         
##  8 Armenia             27 Mar 2022 2,966,388   samples    422,458           
##  9 Australia           28 Mar 2022 66,168,140  samples    4,138,296         
## 10 Austria             27 Mar 2022 173,506,959 samples    3,771,238         
## # ... with 163 more rows, and 4 more variables: `Confirmed /tested,%` <chr>,
## #   `Tested /population,%` <chr>, `Confirmed /population,%` <chr>, Ref. <chr>
## TASK 3: Pre-process and export the extracted data frame
summary(wiki_covid19_page_data_frame)
##  Country or region    Date[a]             Tested            Units[b]        
##  Length:173         Length:173         Length:173         Length:173        
##  Class :character   Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
##  Confirmed(cases)   Confirmed /tested,% Tested /population,%
##  Length:173         Length:173          Length:173          
##  Class :character   Class :character    Class :character    
##  Mode  :character   Mode  :character    Mode  :character    
##  Confirmed /population,%     Ref.          
##  Length:173              Length:173        
##  Class :character        Class :character  
##  Mode  :character        Mode  :character
preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
  # Remove the World row
  data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units[b]"] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}
#### Call the preprocess covid data frame function
new_covid_data_frame <- preprocess_covid_data_frame(wiki_covid19_page_data_frame)
head(new_covid_data_frame)
## # A tibble: 6 x 7
##   country             date    tested confirmed confirmed.teste~ tested.populati~
##   <fct>               <fct>    <dbl>     <dbl>            <dbl>            <dbl>
## 1 Afghanistan         17 Dec~ 154767     49621             32.1             0.4 
## 2 Albania             18 Feb~ 428654     96838             22.6            15   
## 3 Algeria             2 Nov ~ 230553     58574             25.4             0.53
## 4 Andorra             23 Feb~ 300307     37958             12.6           387   
## 5 Angola              2 Feb ~ 399228     20981              5.3             1.3 
## 6 Antigua and Barbuda 6 Mar ~  15268       832              5.4            15.9 
## # ... with 1 more variable: confirmed.population.ratio <dbl>
### Get the summary of the processed data frame again
# Print the summary of the processed data frame again
summary(new_covid_data_frame)
##                 country             date         tested         
##  Afghanistan        :  1   27 Mar 2022: 19   Min.   :     3880  
##  Albania            :  1   25 Mar 2022: 17   1st Qu.:   508141  
##  Algeria            :  1   24 Mar 2022: 10   Median :  2973786  
##  Andorra            :  1   28 Mar 2022:  7   Mean   : 25000460  
##  Angola             :  1   26 Mar 2022:  5   3rd Qu.: 11623846  
##  Antigua and Barbuda:  1   23 Mar 2022:  4   Max.   :553621766  
##  (Other)            :166   (Other)    :110                      
##    confirmed        confirmed.tested.ratio tested.population.ratio
##  Min.   :       0   Min.   : 0.00          Min.   :   0.0065      
##  1st Qu.:   36014   1st Qu.: 5.10          1st Qu.:   9.2000      
##  Median :  262046   Median : 9.45          Median :  42.9000      
##  Mean   : 1816861   Mean   :10.88          Mean   : 154.4450      
##  3rd Qu.: 1087568   3rd Qu.:15.32          3rd Qu.: 135.2500      
##  Max.   :35940893   Max.   :42.80          Max.   :2600.0000      
##                                                                   
##  confirmed.population.ratio
##  Min.   : 0.0000           
##  1st Qu.: 0.3925           
##  Median : 5.7000           
##  Mean   :10.1225           
##  3rd Qu.:14.0750           
##  Max.   :65.7000           
## 
### OK, we can call write.csv() function to save the csv file into a file.
# Export the data frame to a csv file
write.csv(new_covid_data_frame, "new_covid.csv")
## TASK 4: Get a subset of the extracted data frame
#### The goal of task 4 is to get the 5th to 10th rows from the data frame with only country and confirmed columns selected.
# Read covid_data_frame_csv from the csv file
covid_data_frame_csv <- read.csv("new_covid.csv", header=TRUE, sep=",")

# Get the 5th to 10th rows, with two "country" "confirmed" columns
covid_data_frame_row <- covid_data_frame_csv %>% select(country, confirmed)
covid_data_frame_row[c(5:10), ]
##                country confirmed
## 5               Angola     20981
## 6  Antigua and Barbuda       832
## 7            Argentina   9026075
## 8              Armenia    422458
## 9            Australia   4138296
## 10             Austria   3771238
## TASK 5: Calculate worldwide COVID testing positive ratio.
#### The goal of task 5 is to get the total confirmed and tested cases worldwide, and try to figure the overall positive ratio using confirmed cases / tested cases.
# Get the total confirmed cases worldwide
total_confirmed_cases <- sum(covid_data_frame_csv$confirmed)
total_confirmed_cases
## [1] 312500075
# Get the total tested cases worldwide
total_tested_cases <- sum(covid_data_frame_csv$tested)
total_tested_cases
## [1] 4300079068
# Get the positive ratio (confirmed / tested)
positive_ratio <- total_confirmed_cases / total_tested_cases
positive_ratio
## [1] 0.0726731
## TASK 6: Get a country list which reported their testing data.
#### The goal of task 6 is to get a catalog or sorted list of countries who have reported their COVID-19 testing data.
# Get the `country` column
head(covid_data_frame_csv$country)
## [1] "Afghanistan"         "Albania"             "Algeria"            
## [4] "Andorra"             "Angola"              "Antigua and Barbuda"
# Check its class (it is already character)
class(covid_data_frame_csv$country)
## [1] "character"
# Convert the country column into character so that you can easily sort them
covid_data_frame_csv$country <- as.character(covid_data_frame_csv$country)
covid_data_frame_csv$country <- covid_data_frame_csv$country %>% modify_if(is.factor, as.character)

# Sort the countries A to Z
covid_data_frame_csv %>% arrange(country)
##       X                country        date    tested confirmed
## 1     1            Afghanistan 17 Dec 2020    154767     49621
## 2     2                Albania 18 Feb 2021    428654     96838
## 3     3                Algeria  2 Nov 2020    230553     58574
## 4     4                Andorra 23 Feb 2022    300307     37958
## 5     5                 Angola  2 Feb 2021    399228     20981
## 6     6    Antigua and Barbuda  6 Mar 2021     15268       832
## 7     7              Argentina 27 Mar 2022  35271802   9026075
## 8     8                Armenia 27 Mar 2022   2966388    422458
## 9     9              Australia 28 Mar 2022  66168140   4138296
## 10   10                Austria 27 Mar 2022 173506959   3771238
## 11   11             Azerbaijan 27 Mar 2022   6685296    791750
## 12   12                Bahamas 23 Mar 2022    226185     33242
## 13   13                Bahrain 27 Mar 2022   9585397    551342
## 14   14             Bangladesh 24 Jul 2021   7417714   1151644
## 15   15               Barbados 24 Mar 2022    606962     58270
## 16   16                Belarus 27 Mar 2022  12815911    956497
## 17   17                Belgium 24 Mar 2022  32594459   3771233
## 18   18                 Belize 11 Mar 2022    510065     57034
## 19   19                  Benin  4 May 2021    595112      7884
## 20   20                 Bhutan 28 Feb 2022   1736168     12702
## 21   21                Bolivia 26 Mar 2022   4024845    901292
## 22   22 Bosnia and Herzegovina 24 Mar 2022   1729817    374696
## 23   23               Botswana 11 Jan 2022   2026898    232432
## 24   24                 Brazil 19 Feb 2021  23561497  10081676
## 25   25                 Brunei  2 Aug 2021    153804       338
## 26   26               Bulgaria 26 Mar 2022   9545600   1133102
## 27   27           Burkina Faso  4 Mar 2021    158777     12123
## 28   28                Burundi  5 Jan 2021     90019       884
## 29   29               Cambodia  1 Aug 2021   1812706     77914
## 30   30               Cameroon 18 Feb 2021    942685     32681
## 31   31                 Canada 24 Mar 2022  59103163   3429601
## 32   32                   Chad  2 Mar 2021     99027      4020
## 33   33                  Chile 25 Mar 2022  35092861   3434908
## 34   34               China[c] 31 Jul 2020 160000000     87655
## 35   35               Colombia 27 Mar 2022  33829312   6083643
## 36   36             Costa Rica  2 Nov 2021   2575363    561054
## 37   37                Croatia 27 Mar 2022   4658673   1094059
## 38   38                   Cuba 24 Mar 2022  13368420   1085404
## 39   39              Cyprus[d] 25 Mar 2022  22448165    400534
## 40   40                Czechia 25 Mar 2022  21259758   3784532
## 41   41             Denmark[e] 27 Mar 2022  65350732   3042563
## 42   42               Djibouti 27 Mar 2022    296053     15580
## 43   43               Dominica 24 Mar 2022    181700     11805
## 44   44     Dominican Republic 25 Mar 2022   3208952    577673
## 45   45               DR Congo 28 Feb 2021    124838     25961
## 46   46                Ecuador 23 Jul 2021   1627189    480720
## 47   47                  Egypt 23 Jul 2021   3137519    283947
## 48   48            El Salvador 18 Mar 2022   1847861    161052
## 49   49      Equatorial Guinea 24 Mar 2022    300567     15903
## 50   50                Estonia 28 Mar 2022   3230724    552287
## 51   51               Eswatini  8 Dec 2021    415110     49253
## 52   52               Ethiopia 24 Jun 2021   2981185    278446
## 53   53          Faroe Islands 27 Feb 2022    774000     34237
## 54   54                   Fiji 16 Mar 2022    502686     64154
## 55   55                Finland 14 Jan 2022   9042453    371135
## 56   56           France[f][g] 25 Mar 2022 255251436  24779882
## 57   57                  Gabon 23 Jul 2021    958807     25325
## 58   58                 Gambia 15 Feb 2021     43217      4469
## 59   59             Georgia[h]  3 Nov 2021   4888787    732965
## 60   60                Germany  7 Jul 2021  65247345   3733519
## 61   61                  Ghana  3 Jul 2021   1305749     96708
## 62   62                 Greece 27 Mar 2022  72938059   2930321
## 63   63              Greenland 30 Jan 2022    164573     10662
## 64   64                Grenada 11 May 2021     28684       161
## 65   65              Guatemala 24 Mar 2022   4191788    823145
## 66   66                 Guinea 21 Jul 2021    494898     24878
## 67   67          Guinea-Bissau 23 Mar 2022    128098      8126
## 68   68                 Guyana 15 Mar 2022    554991     63157
## 69   69                  Haiti 22 Mar 2022    186638     30522
## 70   70               Honduras 26 Nov 2021   1133782    377859
## 71   71                Hungary 24 Mar 2022  11040273   1836919
## 72   72                Iceland 28 Mar 2022   1877287    179813
## 73   73                  India 19 Sep 2021 553621766  33766707
## 74   74              Indonesia 28 Mar 2022  60191775   6001751
## 75   75                   Iran 27 Mar 2022  49176155   7151088
## 76   76                   Iraq 27 Mar 2022  18294182   2317977
## 77   77                Ireland 28 Mar 2022  11770347   1442877
## 78   78                 Israel 17 Jan 2022  41373364   1792137
## 79   79                  Italy 28 Mar 2022 199235675  14396283
## 80   80            Ivory Coast  3 Mar 2021    429177     33285
## 81   81                Jamaica 26 Mar 2022    917304    128679
## 82   82                  Japan  1 Mar 2021   8487288    432773
## 83   83                 Jordan  6 Jun 2021   7407053    739847
## 84   84             Kazakhstan 28 May 2021  11575012    385144
## 85   85                  Kenya  5 Mar 2021   1322806    107729
## 86   86                 Kosovo 31 May 2021    611357    107410
## 87   87                 Kuwait  9 Mar 2022   7754247    624573
## 88   88             Kyrgyzstan 10 Feb 2021    695415     85253
## 89   89                   Laos  1 Mar 2021    114030        45
## 90   90                 Latvia  5 Sep 2021   3630095    144518
## 91   91                Lebanon 14 Jun 2021   4599186    542649
## 92   92                Lesotho 20 Mar 2022    421681     32880
## 93   93                Liberia 17 Jul 2021    128246      5396
## 94   94                  Libya 23 Mar 2022   2549134    501379
## 95   95              Lithuania 16 Jan 2022   7222288    570602
## 96   96          Luxembourg[i] 27 Mar 2022   4149569    211280
## 97   97             Madagascar 19 Feb 2021    119608     19831
## 98   98                 Malawi 25 Mar 2022    559340     85596
## 99   99               Malaysia  7 Sep 2021  23705425   1880734
## 100 100               Maldives 13 Mar 2022   2216560    174658
## 101 101                   Mali  7 Jul 2021    322504     14449
## 102 102                  Malta  8 Sep 2021   1211456     36606
## 103 103             Mauritania 16 Apr 2021    268093     18103
## 104 104              Mauritius 22 Nov 2020    289552       494
## 105 105                 Mexico 15 Oct 2021  10503678   3749860
## 106 106             Moldova[j] 25 Mar 2022   3138622    512386
## 107 107               Mongolia 10 Jul 2021   3354200    136053
## 108 108             Montenegro 10 May 2021    394388     98449
## 109 109                Morocco 27 Mar 2022  12420590   1162974
## 110 110             Mozambique 22 Jul 2021    688570    105866
## 111 111                Myanmar 16 Sep 2021   4047680    440741
## 112 112                Namibia 26 Mar 2022    981047    157611
## 113 113                  Nepal  8 Mar 2022   5455858    977641
## 114 114            Netherlands  6 Jul 2021  14526293   1692834
## 115 115          New Caledonia  3 Sep 2021     41962       136
## 116 116            New Zealand 25 Mar 2022   6906054    586157
## 117 117                  Niger 22 Feb 2021     79321      4740
## 118 118                Nigeria 28 Feb 2021   1544008    155657
## 119 119            North Korea 25 Nov 2020     16914         0
## 120 120        North Macedonia  1 Jul 2021    881870    155689
## 121 121     Northern Cyprus[k] 25 Mar 2022   6275152     80687
## 122 122                 Norway 20 Jan 2022   9811888    554778
## 123 123                   Oman 28 Oct 2020    509959    114434
## 124 124               Pakistan  5 Mar 2021   9173593    588728
## 125 125              Palestine  5 Feb 2022   3078533    574105
## 126 126                 Panama 27 Mar 2022   5678637    763483
## 127 127       Papua New Guinea 17 Feb 2021     47490       961
## 128 128               Paraguay 23 Mar 2022   2602634    647538
## 129 129                   Peru 14 Mar 2022  27960358   3536842
## 130 130            Philippines 25 Mar 2022  28807213   3676230
## 131 131                 Poland 27 Mar 2022  35437379   5943227
## 132 132               Portugal  5 Jan 2022  27515490   1499976
## 133 133                  Qatar 27 Mar 2022   3405613    360868
## 134 134                Romania 29 Jan 2021   5405393    724250
## 135 135                 Russia 26 Mar 2022 283735193  17739462
## 136 136                 Rwanda  6 Oct 2021   2885812     98209
## 137 137  Saint Kitts and Nevis 26 Aug 2021     30231       995
## 138 138            Saint Lucia 17 Mar 2022    138699     22871
## 139 139          Saint Vincent 24 Feb 2022    102689      8307
## 140 140             San Marino 28 Mar 2022    144139     15181
## 141 141           Saudi Arabia 20 Mar 2022  41370138    749597
## 142 142                Senegal 12 Jul 2021    624502     46509
## 143 143                 Serbia 25 Mar 2022   9108141   1966078
## 144 144              Singapore  3 Aug 2021  16206203     65315
## 145 145               Slovakia 25 Mar 2022   6812898   1681865
## 146 146               Slovenia 25 Mar 2022   2608562    955473
## 147 147           South Africa 24 May 2021  11378282   1637848
## 148 148            South Korea  1 Mar 2021   6592010     90029
## 149 149            South Sudan 26 May 2021    164472     10688
## 150 150                  Spain  1 Jul 2021  54128524   3821305
## 151 151              Sri Lanka 30 Mar 2021   2384745     93128
## 152 152                  Sudan  7 Jan 2021    158804     23316
## 153 153                 Sweden 24 May 2021   9996795   1074751
## 154 154         Switzerland[l] 25 Mar 2022  20543650   3419525
## 155 155              Taiwan[m] 27 Mar 2022  12588940     22769
## 156 156               Tanzania 18 Nov 2020      3880       509
## 157 157               Thailand  4 Mar 2021   1579597     26162
## 158 158                   Togo 25 Mar 2022    714402     36911
## 159 159    Trinidad and Tobago  3 Jan 2022    512730     92997
## 160 160                Tunisia 23 Aug 2021   2893625    703732
## 161 161                 Turkey  2 Jul 2021  61236294   5435831
## 162 162                 Uganda 11 Feb 2021    852444     39979
## 163 163                Ukraine 24 Nov 2021  15648456   3367461
## 164 164   United Arab Emirates 14 Mar 2022 145740412    888718
## 165 165         United Kingdom 24 Mar 2022 499886168  20613817
## 166 166          United States  9 Aug 2021 512152348  35940893
## 167 167                Uruguay 25 Mar 2022   5949686    882379
## 168 168             Uzbekistan  7 Sep 2020   2630000     43975
## 169 169              Venezuela 30 Mar 2021   3179074    159149
## 170 170                Vietnam 20 Mar 2022  42707675   7958048
## 171 171                 Zambia 10 Mar 2022   3301860    314850
## 172 172               Zimbabwe 25 Mar 2022   2174205    245645
##     confirmed.tested.ratio tested.population.ratio confirmed.population.ratio
## 1                   32.100                  0.4000                    0.13000
## 2                   22.600                 15.0000                    3.40000
## 3                   25.400                  0.5300                    0.13000
## 4                   12.600                387.0000                   49.00000
## 5                    5.300                  1.3000                    0.06700
## 6                    5.400                 15.9000                    0.86000
## 7                   25.600                 77.7000                   19.90000
## 8                   14.200                100.5000                   14.30000
## 9                    6.300                264.0000                   16.50000
## 10                   2.200               1949.0000                   42.40000
## 11                  11.800                 67.5000                    8.00000
## 12                  14.700                 58.7000                    8.60000
## 13                   5.800                611.0000                   35.10000
## 14                  15.500                  4.5000                    0.70000
## 15                   9.600                211.0000                   20.30000
## 16                   7.500                135.0000                   10.10000
## 17                  11.600                283.0000                   32.70000
## 18                  11.200                125.0000                   14.00000
## 19                   1.300                  5.1000                    0.06700
## 20                   0.730                234.0000                    1.71000
## 21                  22.400                 35.2000                    7.90000
## 22                  21.700                 50.6000                   11.00000
## 23                  11.500                 89.9000                   10.30000
## 24                  42.800                 11.2000                    4.80000
## 25                   0.220                 33.5000                    0.07400
## 26                  11.900                137.0000                   16.30000
## 27                   7.600                  0.7600                    0.05800
## 28                   0.980                  0.7600                    0.00740
## 29                   4.300                 11.2000                    0.48000
## 30                   3.500                  3.6000                    0.12000
## 31                   5.800                156.0000                    9.10000
## 32                   4.100                  0.7200                    0.02900
## 33                   9.800                184.0000                   18.00000
## 34                   0.055                 11.1000                    0.00610
## 35                  18.000                 70.1000                   12.60000
## 36                  21.800                 51.5000                   11.20000
## 37                  23.500                114.0000                   26.80000
## 38                   8.100                118.0000                    9.60000
## 39                   1.800               2600.0000                   46.40000
## 40                  17.800                199.0000                   35.40000
## 41                   4.700               1122.0000                   52.20000
## 42                   5.300                 32.1000                    1.70000
## 43                   6.500                254.0000                   16.50000
## 44                  18.000                 29.5000                    5.30000
## 45                  20.800                  0.1400                    0.02900
## 46                  29.500                  9.5000                    2.80000
## 47                   9.100                  3.1000                    0.28000
## 48                   8.700                 28.5000                    2.50000
## 49                   5.300                 23.0000                    1.20000
## 50                  17.100                243.0000                   41.60000
## 51                  11.900                 36.5000                    4.30000
## 52                   9.300                  2.6000                    0.24000
## 53                   4.400               1493.0000                   65.70000
## 54                  12.800                 56.1000                    7.20000
## 55                   4.100                163.0000                    6.70000
## 56                   9.700                391.0000                   38.00000
## 57                   2.600                  3.1000                    0.08200
## 58                  10.300                  2.0000                    0.21000
## 59                  15.000                132.0000                   19.70000
## 60                   5.700                 77.8000                    4.50000
## 61                   7.400                  4.2000                    0.31000
## 62                   4.000                677.0000                   27.20000
## 63                   6.500                293.0000                   19.00000
## 64                   0.560                 25.7000                    0.14000
## 65                  19.600                 24.3000                    4.80000
## 66                   5.000                  3.8000                    0.19000
## 67                   6.300                  6.8000                    0.43000
## 68                  11.400                 70.6000                    8.00000
## 69                  16.400                  1.6000                    0.27000
## 70                  33.300                 11.8000                    3.90000
## 71                  16.600                114.0000                   19.00000
## 72                   9.600                515.0000                   49.40000
## 73                   6.100                 40.1000                    2.40000
## 74                  10.000                 22.3000                    2.20000
## 75                  14.500                 59.1000                    8.60000
## 76                  12.700                 45.5000                    5.80000
## 77                  12.300                239.0000                   29.30000
## 78                   4.300                451.0000                   19.50000
## 79                   7.200                330.0000                   23.90000
## 80                   7.800                  1.6000                    0.13000
## 81                  14.000                 33.7000                    4.70000
## 82                   5.100                  6.7000                    0.34000
## 83                  10.000                 69.5000                    6.90000
## 84                   3.300                 62.1000                    2.10000
## 85                   8.100                  2.8000                    0.23000
## 86                  17.600                 33.8000                    5.90000
## 87                   8.100                181.0000                   14.60000
## 88                  12.300                 10.7000                    1.30000
## 89                   0.039                  1.6000                    0.00063
## 90                   4.000                189.0000                    7.50000
## 91                  11.800                 67.4000                    8.00000
## 92                   7.800                 21.0000                    1.60000
## 93                   4.200                  2.5000                    0.11000
## 94                  19.700                 37.1000                    7.30000
## 95                   7.900                258.0000                   20.40000
## 96                   5.100                663.0000                   33.70000
## 97                  16.600                  0.4600                    0.07600
## 98                  15.300                  2.9000                    0.45000
## 99                   7.900                 72.3000                    5.70000
## 100                  7.900                398.0000                   31.30000
## 101                  4.500                  1.6000                    0.07100
## 102                  3.000                245.0000                    7.40000
## 103                  6.800                  6.1000                    0.41000
## 104                  0.170                 22.9000                    0.03900
## 105                 35.700                  8.2000                    2.90000
## 106                 16.300                119.0000                   19.40000
## 107                  4.100                100.0000                    4.10000
## 108                 25.000                 62.5000                   15.60000
## 109                  9.400                 33.7000                    3.20000
## 110                 15.400                  2.2000                    0.34000
## 111                 10.900                  7.4000                    0.81000
## 112                 16.100                 35.7000                    5.70000
## 113                 17.900                 19.4000                    3.50000
## 114                 11.700                 83.4000                    9.70000
## 115                  0.320                 15.7000                    0.05000
## 116                  8.500                139.0000                   11.80000
## 117                  6.000                  0.3500                    0.02100
## 118                 10.100                  0.7500                    0.07600
## 119                  0.000                  0.0660                    0.00000
## 120                 17.700                 42.5000                    7.50000
## 121                 12.000               1925.0000                   24.80000
## 122                  5.700                183.0000                   10.30000
## 123                 22.400                 11.0000                    2.50000
## 124                  6.400                  4.2000                    0.27000
## 125                 18.600                 60.9000                   11.40000
## 126                 13.400                136.0000                   18.30000
## 127                  2.000                  0.5300                    0.01100
## 128                 24.900                 36.5000                    9.10000
## 129                 12.600                 85.2000                   10.80000
## 130                 12.800                 28.5000                    3.60000
## 131                 16.800                 92.3000                   15.50000
## 132                  5.500                268.0000                   14.60000
## 133                 10.600                118.0000                   12.50000
## 134                 13.400                 27.9000                    3.70000
## 135                  6.300                193.0000                   12.10000
## 136                  3.400                 22.3000                    0.76000
## 137                  3.300                 57.6000                    1.90000
## 138                 16.500                 76.3000                   12.60000
## 139                  8.100                 93.2000                    7.50000
## 140                 10.500                421.0000                   44.30000
## 141                  1.800                119.0000                    2.20000
## 142                  7.400                  3.9000                    0.29000
## 143                 21.600                131.0000                   28.20000
## 144                  0.400                284.0000                    1.10000
## 145                 24.700                125.0000                   30.80000
## 146                 36.600                125.0000                   45.60000
## 147                 14.400                 19.2000                    2.80000
## 148                  1.400                 12.7000                    0.17000
## 149                  6.500                  1.3000                    0.08400
## 150                  7.100                116.0000                    8.20000
## 151                  3.900                 10.9000                    0.43000
## 152                 14.700                  0.3600                    0.05300
## 153                 10.800                 96.8000                   10.40000
## 154                 16.600                239.0000                   39.70000
## 155                  0.180                 53.3000                    0.09600
## 156                 13.100                  0.0065                    0.00085
## 157                  1.700                  2.3000                    0.03800
## 158                  5.200                  8.3000                    0.43000
## 159                 18.100                 37.6000                    6.80000
## 160                 24.300                 24.5000                    6.00000
## 161                  8.900                 73.6000                    6.50000
## 162                  4.700                  1.9000                    0.08700
## 163                 21.500                 37.2000                    8.00000
## 164                  0.610               1518.0000                    9.30000
## 165                  4.100                740.0000                   30.50000
## 166                  7.000                155.0000                   10.90000
## 167                 14.800                171.0000                   25.40000
## 168                  1.700                  7.7000                    0.13000
## 169                  5.000                 11.0000                    0.55000
## 170                 18.600                 43.3000                    8.10000
## 171                  9.500                 19.0000                    1.80000
## 172                 11.300                 14.6000                    1.70000

# Sort the countries Z to A
sort(covid_data_frame_csv$country, decreasing = TRUE)
##   [1] "Zimbabwe"               "Zambia"                 "Vietnam"               
##   [4] "Venezuela"              "Uzbekistan"             "Uruguay"               
##   [7] "United States"          "United Kingdom"         "United Arab Emirates"  
##  [10] "Ukraine"                "Uganda"                 "Turkey"                
##  [13] "Tunisia"                "Trinidad and Tobago"    "Togo"                  
##  [16] "Thailand"               "Tanzania"               "Taiwan[m]"             
##  [19] "Switzerland[l]"         "Sweden"                 "Sudan"                 
##  [22] "Sri Lanka"              "Spain"                  "South Sudan"           
##  [25] "South Korea"            "South Africa"           "Slovenia"              
##  [28] "Slovakia"               "Singapore"              "Serbia"                
##  [31] "Senegal"                "Saudi Arabia"           "San Marino"            
##  [34] "Saint Vincent"          "Saint Lucia"            "Saint Kitts and Nevis" 
##  [37] "Rwanda"                 "Russia"                 "Romania"               
##  [40] "Qatar"                  "Portugal"               "Poland"                
##  [43] "Philippines"            "Peru"                   "Paraguay"              
##  [46] "Papua New Guinea"       "Panama"                 "Palestine"             
##  [49] "Pakistan"               "Oman"                   "Norway"                
##  [52] "Northern Cyprus[k]"     "North Macedonia"        "North Korea"           
##  [55] "Nigeria"                "Niger"                  "New Zealand"           
##  [58] "New Caledonia"          "Netherlands"            "Nepal"                 
##  [61] "Namibia"                "Myanmar"                "Mozambique"            
##  [64] "Morocco"                "Montenegro"             "Mongolia"              
##  [67] "Moldova[j]"             "Mexico"                 "Mauritius"             
##  [70] "Mauritania"             "Malta"                  "Mali"                  
##  [73] "Maldives"               "Malaysia"               "Malawi"                
##  [76] "Madagascar"             "Luxembourg[i]"          "Lithuania"             
##  [79] "Libya"                  "Liberia"                "Lesotho"               
##  [82] "Lebanon"                "Latvia"                 "Laos"                  
##  [85] "Kyrgyzstan"             "Kuwait"                 "Kosovo"                
##  [88] "Kenya"                  "Kazakhstan"             "Jordan"                
##  [91] "Japan"                  "Jamaica"                "Ivory Coast"           
##  [94] "Italy"                  "Israel"                 "Ireland"               
##  [97] "Iraq"                   "Iran"                   "Indonesia"             
## [100] "India"                  "Iceland"                "Hungary"               
## [103] "Honduras"               "Haiti"                  "Guyana"                
## [106] "Guinea-Bissau"          "Guinea"                 "Guatemala"             
## [109] "Grenada"                "Greenland"              "Greece"                
## [112] "Ghana"                  "Germany"                "Georgia[h]"            
## [115] "Gambia"                 "Gabon"                  "France[f][g]"          
## [118] "Finland"                "Fiji"                   "Faroe Islands"         
## [121] "Ethiopia"               "Eswatini"               "Estonia"               
## [124] "Equatorial Guinea"      "El Salvador"            "Egypt"                 
## [127] "Ecuador"                "DR Congo"               "Dominican Republic"    
## [130] "Dominica"               "Djibouti"               "Denmark[e]"            
## [133] "Czechia"                "Cyprus[d]"              "Cuba"                  
## [136] "Croatia"                "Costa Rica"             "Colombia"              
## [139] "China[c]"               "Chile"                  "Chad"                  
## [142] "Canada"                 "Cameroon"               "Cambodia"              
## [145] "Burundi"                "Burkina Faso"           "Bulgaria"              
## [148] "Brunei"                 "Brazil"                 "Botswana"              
## [151] "Bosnia and Herzegovina" "Bolivia"                "Bhutan"                
## [154] "Benin"                  "Belize"                 "Belgium"               
## [157] "Belarus"                "Barbados"               "Bangladesh"            
## [160] "Bahrain"                "Bahamas"                "Azerbaijan"            
## [163] "Austria"                "Australia"              "Armenia"               
## [166] "Argentina"              "Antigua and Barbuda"    "Angola"                
## [169] "Andorra"                "Algeria"                "Albania"               
## [172] "Afghanistan"

## TASK 7: Identify countries names with a specific pattern.
#### The goal of task 7 is using a regular expression to find any countries start with United.
# Use a regular expression `United.+` to find matches
matched_country <- grep("United.+", covid_data_frame_csv$country)
# Print the matched country names
covid_data_frame_csv$country[matched_country]
## [1] "United Arab Emirates" "United Kingdom"       "United States"

## TASK 8: Pick two countries you are interested, and then review their testing data.

#### The goal of task 8 is to compare the COVID-19 test data between two countires, you will need to select two rows from the dataframe, and select country, confirmed, confirmed-population-ratio columns.
# Select a subset (should be only one row) of data frame based on a selected country name and columns
covid_data_frame_Angola <- covid_data_frame_csv %>% select(country, confirmed,  confirmed.population.ratio) %>% filter(country == "Angola")

# Select a subset (should be only one row) of data frame based on a selected country name and columns
covid_data_frame_Argentina <- covid_data_frame_csv %>% select(country, confirmed,  confirmed.population.ratio) %>% filter(country == "Argentina")
covid_data_frame_Angola
##   country confirmed confirmed.population.ratio
## 1  Angola     20981                      0.067
print(covid_data_frame_Argentina)
##     country confirmed confirmed.population.ratio
## 1 Argentina   9026075                       19.9

## TASK 9: Compare which one of the selected countries has a larger ratio of confirmed cases to population.

#### The goal of task 9 is to find out which country you have selected before has larger ratio of confirmed cases to population, which may indicate that country has higher COVID-19 infection risk.

# Use if-else statement
if (covid_data_frame_Argentina$confirmed.population > covid_data_frame_Angola$confirmed.population) {
  print("Argentina has higher COVID-19 infection risk")
} else {
  print("Angola has higher COVID-19 infection risk")
}

## [1] "Argentina has higher COVID-19 infection risk"

## TASK 10: Find countries with confirmed to population ratio rate less than a threshold.

#### The goal of task 10 is to find out which countries have the confirmed to population ratio less than 1%, it may indicate the risk of those countries are relatively low.

# Get a subset of any countries with `confirmed.population.ratio` less than the threshold
subset(covid_data_frame_csv, subset = confirmed.population.ratio < 1)
##       X             country        date    tested confirmed
## 1     1         Afghanistan 17 Dec 2020    154767     49621
## 3     3             Algeria  2 Nov 2020    230553     58574
## 5     5              Angola  2 Feb 2021    399228     20981
## 6     6 Antigua and Barbuda  6 Mar 2021     15268       832
## 14   14          Bangladesh 24 Jul 2021   7417714   1151644
## 19   19               Benin  4 May 2021    595112      7884
## 25   25              Brunei  2 Aug 2021    153804       338
## 27   27        Burkina Faso  4 Mar 2021    158777     12123
## 28   28             Burundi  5 Jan 2021     90019       884
## 29   29            Cambodia  1 Aug 2021   1812706     77914
## 30   30            Cameroon 18 Feb 2021    942685     32681
## 32   32                Chad  2 Mar 2021     99027      4020
## 34   34            China[c] 31 Jul 2020 160000000     87655
## 45   45            DR Congo 28 Feb 2021    124838     25961
## 47   47               Egypt 23 Jul 2021   3137519    283947
## 52   52            Ethiopia 24 Jun 2021   2981185    278446
## 57   57               Gabon 23 Jul 2021    958807     25325
## 58   58              Gambia 15 Feb 2021     43217      4469
## 61   61               Ghana  3 Jul 2021   1305749     96708
## 64   64             Grenada 11 May 2021     28684       161
## 66   66              Guinea 21 Jul 2021    494898     24878
## 67   67       Guinea-Bissau 23 Mar 2022    128098      8126
## 69   69               Haiti 22 Mar 2022    186638     30522
## 80   80         Ivory Coast  3 Mar 2021    429177     33285
## 82   82               Japan  1 Mar 2021   8487288    432773
## 85   85               Kenya  5 Mar 2021   1322806    107729
## 89   89                Laos  1 Mar 2021    114030        45
## 93   93             Liberia 17 Jul 2021    128246      5396
## 97   97          Madagascar 19 Feb 2021    119608     19831
## 98   98              Malawi 25 Mar 2022    559340     85596
## 101 101                Mali  7 Jul 2021    322504     14449
## 103 103          Mauritania 16 Apr 2021    268093     18103
## 104 104           Mauritius 22 Nov 2020    289552       494
## 110 110          Mozambique 22 Jul 2021    688570    105866
## 111 111             Myanmar 16 Sep 2021   4047680    440741
## 115 115       New Caledonia  3 Sep 2021     41962       136
## 117 117               Niger 22 Feb 2021     79321      4740
## 118 118             Nigeria 28 Feb 2021   1544008    155657
## 119 119         North Korea 25 Nov 2020     16914         0
## 124 124            Pakistan  5 Mar 2021   9173593    588728
## 127 127    Papua New Guinea 17 Feb 2021     47490       961
## 136 136              Rwanda  6 Oct 2021   2885812     98209
## 142 142             Senegal 12 Jul 2021    624502     46509
## 148 148         South Korea  1 Mar 2021   6592010     90029
## 149 149         South Sudan 26 May 2021    164472     10688
## 151 151           Sri Lanka 30 Mar 2021   2384745     93128
## 152 152               Sudan  7 Jan 2021    158804     23316
## 155 155           Taiwan[m] 27 Mar 2022  12588940     22769
## 156 156            Tanzania 18 Nov 2020      3880       509
## 157 157            Thailand  4 Mar 2021   1579597     26162
## 158 158                Togo 25 Mar 2022    714402     36911
## 162 162              Uganda 11 Feb 2021    852444     39979
## 168 168          Uzbekistan  7 Sep 2020   2630000     43975
## 169 169           Venezuela 30 Mar 2021   3179074    159149
##     confirmed.tested.ratio tested.population.ratio confirmed.population.ratio
## 1                   32.100                  0.4000                    0.13000
## 3                   25.400                  0.5300                    0.13000
## 5                    5.300                  1.3000                    0.06700
## 6                    5.400                 15.9000                    0.86000
## 14                  15.500                  4.5000                    0.70000
## 19                   1.300                  5.1000                    0.06700
## 25                   0.220                 33.5000                    0.07400
## 27                   7.600                  0.7600                    0.05800
## 28                   0.980                  0.7600                    0.00740
## 29                   4.300                 11.2000                    0.48000
## 30                   3.500                  3.6000                    0.12000
## 32                   4.100                  0.7200                    0.02900
## 34                   0.055                 11.1000                    0.00610
## 45                  20.800                  0.1400                    0.02900
## 47                   9.100                  3.1000                    0.28000
## 52                   9.300                  2.6000                    0.24000
## 57                   2.600                  3.1000                    0.08200
## 58                  10.300                  2.0000                    0.21000
## 61                   7.400                  4.2000                    0.31000
## 64                   0.560                 25.7000                    0.14000
## 66                   5.000                  3.8000                    0.19000
## 67                   6.300                  6.8000                    0.43000
## 69                  16.400                  1.6000                    0.27000
## 80                   7.800                  1.6000                    0.13000
## 82                   5.100                  6.7000                    0.34000
## 85                   8.100                  2.8000                    0.23000
## 89                   0.039                  1.6000                    0.00063
## 93                   4.200                  2.5000                    0.11000
## 97                  16.600                  0.4600                    0.07600
## 98                  15.300                  2.9000                    0.45000
## 101                  4.500                  1.6000                    0.07100
## 103                  6.800                  6.1000                    0.41000
## 104                  0.170                 22.9000                    0.03900
## 110                 15.400                  2.2000                    0.34000
## 111                 10.900                  7.4000                    0.81000
## 115                  0.320                 15.7000                    0.05000
## 117                  6.000                  0.3500                    0.02100
## 118                 10.100                  0.7500                    0.07600
## 119                  0.000                  0.0660                    0.00000
## 124                  6.400                  4.2000                    0.27000
## 127                  2.000                  0.5300                    0.01100
## 136                  3.400                 22.3000                    0.76000
## 142                  7.400                  3.9000                    0.29000
## 148                  1.400                 12.7000                    0.17000
## 149                  6.500                  1.3000                    0.08400
## 151                  3.900                 10.9000                    0.43000
## 152                 14.700                  0.3600                    0.05300
## 155                  0.180                 53.3000                    0.09600
## 156                 13.100                  0.0065                    0.00085
## 157                  1.700                  2.3000                    0.03800
## 158                  5.200                  8.3000                    0.43000
## 162                  4.700                  1.9000                    0.08700
## 168                  1.700                  7.7000                    0.13000
## 169                  5.000                 11.0000                    0.55000