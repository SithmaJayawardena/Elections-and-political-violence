# Elections-and-political-violence
## Identifying a statistically significant politically critical period surrounding African elections

An election is a sensitive event with meaningful political and economic implications for many people. It represents an opportunity for the redistribution of power and a shift in priorities for a nation. As such, it invites extreme responses that sometimes can be translated to political violence. By examining a longitudinal sample of 49 African countries’ election cycles between the years 1997-2020 in this project, I assessed the association between elections and political violence. I used Armed Conflict Location and Event Data (ACLED) that collects real-time data on the locations, dates, actors, fatalities, and types of all reported political violence and protest events around the world. The data includes several other control variables to account for factors that may affect political and electoral violence, extracted from the Varieties of Democracy dataset and World Bank.

The questions explored in this project are as follows:

1. Is there a trend in incidence of political violence closer to election days?
2. Is the above trend statistically significant? In other words, is there a statistically significant association in between elections and political violence?
3. What is the length of the period where there exist a statistically significant trend?

The goals of this study:

1. Use negative binomial regression models to test the association between elections and political violence.
2. Use two-sample difference of mean tests; t-test, Welch test, and Wilcoxon test, to test the hypothesis that the means of a monthly number of violent events in the election and non-election periods are different.
3. Data visualization techniques for a better understanding about the data.
4. Practice tabular data extraction and manipulation for an efficient analysis in R.
5. Use different R Studio libraries. (e.g. MASS, dplyr, ggplot2 etc.)
