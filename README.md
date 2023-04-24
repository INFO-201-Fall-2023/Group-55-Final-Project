# Group 55 Final Project
final-project-repositories-ishavk1348 created by GitHub Classroom

Aiman Hazmi Shamsul, Alisa Tcurko, and Isha Kshirsagar
INFO 201 | Section AA | Spring 2023
Group 55
April 23, 2023

## Netflix’s Global Content Expansion and Country-Based Economic Growth

### Story Pitch:

In the past couple of years during and after the COVID-19 pandemic, Netflix has quickly become one of the most prominent media corporations across the globe, with many millions of subscribers and a massive content library of diverse movies and television shows. During this time, the streaming giant has seen its fair share of tumultuous growth and loss cycles, but nevertheless, many sources indicate Netflix’s plans for a large-scale expansion into global markets outside of the United States where it originated from. 

For our INFO 201 Final Project, we would like to investigate Netflix’s global expansion strategy further and answer the question, “Do countries with the fastest growing economies correspond with the global regions from which Netflix is investing in the most streaming content?” Using data wrangling and analysis methods, we will use country-specific per-capita GDP (gross domestic product) values over time and compare that to global Netflix data about the release dates and countries of origin of newly added movies and episodic series. With these learnings, we seek to determine whether it is possible to predict future loci of high economic growth based on current Netflix content investment trends. 

Netflix plans for its global expansion scheme to be accomplished by targeting subscribers from a careful selection of local markets around the world. By focusing on countries outside of the United States, Netflix has quickly begun to reach a wider, international audience, leading to its unprecedented subscriber growth as a global media giant in recent years. To reach geographically distant countries, the company began licensing mass amounts of locally-produced shows and movies from hundreds of different languages in order to attract native speakers onto the platform. The COVID-19 pandemic and sudden transition to online, stay-at-home lifestyles only accelerated the growth of streaming services on a global scale, especially that of Netflix. During this time, many foreign-language Netflix series - such as the Korean “Squid Game”, for instance - made a breakthrough in the international pop-culture scene. The massive revenue generated by this series alone was a big catalyst for Netflix's largescale global investment in content produced for and from the Asia-Pacific region. Coincidentally, Asian countries such as China, Vietnam, Indonesia, and India are among the fastest-growing economies based on their Economic Complexity Index (ECI), according to researchers from Harvard University. Netflix’s choice of investment in this region appears indicative of the high possibility of securing the most profitable returns, as predicted by the rapidly growing economies of these countries. Under this premise, we seek to draw a relationship between individual countries' economic growth with the influx of new streaming content originating from said countries.

By investigating the question of whether Netflix’s global content expansion strategy corresponds with country-specific GDP growth, we will provide valuable insights for other similar companies who wish to expand their own operations into international consumer markets. Additionally, our findings can help predict potential increases in employment rates across the world, as Netflix will undoubtedly create many local employment opportunities in the countries where it invests into the most content from, further boosting the economical environments of said countries. From a business angle, understanding how global markets are performing is essential for profit optimization, and from a political perspective, tracking foreign investments into one’s own country can serve as predictors for future local economic growth. As such, our research is both interesting and important due to its wide range of applications in many sectors of our international society.

### Finding Data: Dataset Sources

#### Dataset #1: “Netflix Movies and TV Shows”
##### Source Link: https://www.kaggle.com/datasets/shivamb/netflix-shows

For our first source of data, we are using a comprehensive dataset of all Netflix movie and episodic series titles added to the platform between December 31st, 2007, and November 25th, 2021. The data table includes twelve columns with content-identifying features such as type of content (movie or television series), unique ID number, title, cast and directors, country of production, date added onto Netflix, the actual date of release, rating, and duration. It also includes 8,807 unique rows of values. For the purposes of this data analysis project, we will primarily be utilizing the columns of type of content, country of production, and date added to Netflix.

We sourced this set from Kaggle, an online platform for uploading and downloading open-source datasets. It was available in a .csv (comma-separated values) format in a Microsoft Excel spreadsheet. The dataset was uploaded onto the platform by user Shivam Bansal - a certified “Datasets Grandmaster” with a rich portfolio of uploaded datasets on Kaggle - but unfortunately, there is little information available regarding who actually collected or generated the data and how it was sourced. Despite the limited source information we have, we decided to move forward with this dataset based on factors such as Shivam Bansal’s perceived credibility as a top Kaggle member and the fact that this same dataset has been used and referenced by many other users for their own data projects. 

#### Dataset #2: “International Macroeconomic Dataset: Real Per-Capita GDP (2015 Base) Projected”
##### Source Link: https://ers.usda.gov/data-products/international-macroeconomic-data-set.aspx

For our second source of data, we are using an expansive dataset of annual country-specific per-capita GDP values from across the world, ranging between the years 1970 and 2032. This dataset also includes separate columns that show the year-over-year GDP growth rates for each country starting from 1971 and continuing to 2032 as before. This dataset was most recently updated on November 15th, 2022, and all GDP values between 1970 and 2022 are real observed values. GDP values from 2023 onwards are forecasted predictions based on previous growth trends and current economic conditions for each country. Each GDP value has been adjusted for inflation according to the U.S. dollar’s worth in 2015. In total, the data table includes 127 columns - one of which includes the country names, and the rest is split equally between annual per-capita GDP values and annual GDP growth rates. It also includes unique rows for 216 countries, in addition to 51 other rows that include summary statistics for specific groups or regions of countries, for 267 unique rows in total

We sourced this dataset from the U.S. Department of Agriculture’s Economic Research Service via the data.gov federal website for open-source databases. It was available as a Microsoft Excel spreadsheet, but unfortunately not as a .csv file, unlike the first Netflix dataset. This collection of global GDP values over time was created to “serve as underlying assumptions for the annual USDA agricultural supply and demand projections”, and “the macroeconomic projections describe the long-term, 10-year scenario that is used as a benchmark for analyzing the impacts of alternative scenarios and macroeconomic shocks”. Although we are unsure how the USDA collected or generated the data, we are sure that a government research organization such as this would be a trustworthy and accurate data source for our project.

### Background Research / Inspiration (5+ sources)

#### Netflix Global Expansion:
* https://variety.com/2023/tv/news/netflix-content-spending-asia-pacific-1235543665/
  + Netflix is to spend $1.9 billion in the Asia-Pacific region as the revenue rises to 12% from 2022 (9% from 2021-2022).
  + Revenue growth will depend on 1) advertising growth in Australian market; 2) growth from Japan and South Korea; 3) content from India, Indonesia, Philippines, and Thailand.
  + Price adjustments in Southeast Asia, according to Netflix in Feb 2023. Basic plan subscription was cut up to 46% ([link](https://www.contentasia.tv/news/netflix-slashes-southeast-asia-subscription-pricing-46#:~:text=Netflix%20slashes%20Southeast%20Asia%20subscription%20pricing%20by%20up%20to%2046%25%20%7C%20ContentAsia&text=Netflix%20slashed%20subscription%20fees%20for,%2C%20Philippines%2C%20Malaysia%20and%20Vietnam.)).
  + This article explains the uprising of new content from the region which is heavily influenced by Netflix’s global investment plan.
  
* https://hbr.org/2018/10/how-netflix-expanded-to-190-countries-in-7-years
  + Growth in three phases.
    1) Entering the market gradually based on geographic distance from U.S., and “perceived differences between markets” (e.g: how similar the two countries are in terms of culture, language, etc.)
    2) Expansion into more diverse markets. In short, Netflix expands further geographically.
    3) Netflix adopts advanced targeted content with emphasized localization (local subbing, dubbing, etc.).
  + Partnership with local companies.
  + The strategies implemented by Netflix as they are expanding give meaningful insight into how they impact the local economy that relates to employment, etc.
  
* https://www.hollywoodreporter.com/business/business-news/netflix-top-region-for-subscribers-1235321144/
  + Netflix has ~2 million more subscribers in Europe, the Middle East and Africa (EMEA) than in N. America. 
  + COVID-19 accelerated the growth.
  + Average revenue per user (ARPU) of NA is still higher than the EMEA region, due to the cheaper price there.
  + The growing number of subscribers in this region also helps reinforce Netflix's expansion. In addition to heavy investment in the Asia-Pacific region, we can assume that a good amount of investment, which then translates to content should be coming from this EMEA as well.
  
* https://www.centuroglobal.com/article/the-remarkable-netflix-global-expansion-journey-a-case-study#:~:text=As%20of%20the%20beginning%20of,the%20business%20a%20fantastic%20success.&text=The%20Netflix%20global%20expansion%20journey%20into%20over,markets%20is%20a%20phenomenal%20success
  + Netflix produces original content based on cultural preferences while also adjusting it so it might go global.
  + This article elaborates further on the strategies mentioned in the Havard Business Review article above. 
  
##### Netflix's Next Steps:
* https://www.investmentmonitor.ai/features/what-will-netflix-do-next/#:~:text=From%20November%202022%2C%20Netflix%20will,giant%20would%20never%20include%20advertising.
  + Include adds to cut subscription costs in some regions
  + More global investment. Heavy push to Asia-Pacific (especially S. Korea).
  + The article estimates Netflix's future plan after the wavy 2022 results. The more recent article from Variety (the first link) supports the prediction. 
  
* https://mashable.com/article/netflix-password-sharing-crackdown
  + Paid password-sharing is to be launched in the U.S. later this year.
  + Despite the backlash, Netflix is pleased with the result of the launch in  Canada, New Zealand, Spain, and Portugal earlier this year.
  + This is one of the latest articles on how Netflix will eventually do to the subscription plan. The reception from other countries could be insightful to our research and how Netflix’s business model is perceived by different countries.
  
##### Countries With Most Netflix Content:
* https://www.whats-on-netflix.com/news/what-countries-produce-the-most-popular-netflix-shows-and-movies/ (2020-2021)
* https://www.whats-on-netflix.com/news/what-countries-produce-most-popular-content-for-netflix/ (2022)
  + Comparing Q4 2021 and 2022, the U.S., S. Korea, and Colombia dominated the Top 3.
  + The comparison between the highly viewed shows/series in each quarter is a good predictor of Netflix’s investment in relevant regions.

#### Fastest Growing Global Economies:
* https://news.harvard.edu/gazette/story/newsplus/harvard-growth-lab-projects-fastest-growing-economies-to-2030/
  + China, Vietnam, Uganda, Indonesia, and India are among the fastest-growing economies by 2030, according to the Economic Complexity Index (ECI). 
  + ECI records the diversity and sophistication of the export products; i.e: countries with diversified production in complex sectors will experience rapid growth.
  + The fastest-growing countries coincide with Netflix’s investment plan from before. Although deeper analysis is required, this assumption will be our baseline.
  
* https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD
  + The largest GDP growth is dominated by African countries. 
  + European countries experience the most negative growth.
  + This website keeps up-to-date GDP growth which will be monitored as we continue our project.
  
##### Additional Info:
* https://www.oxfordmartin.ox.ac.uk/publications/a-new-interpretation-of-the-economic-complexity-index/
  + Explains that GDP is influenced by ECI.
  + “(ECI) has been successful at explaining cross-country differences in GDP/capita and economic growth.”
