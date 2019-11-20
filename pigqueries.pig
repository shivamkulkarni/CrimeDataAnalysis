Crime_Data = LOAD '/user/maria_dev/new_clean_data_final.csv' USING PigStorage(',')
AS (
no:int, id:int, case_number:chararray, date_field:chararray,
block:chararray, IUCR:chararray, primary_type:chararray,
description:chararray, location_description:chararray,
arrest:chararray, domestic:chararray, beat:int,
district:int, ward:int, community_area:int,
fbi_code:chararray, x_coordinate:int, y_coordinate:int,
year:int, updated_on:chararray, latitude:double,
longitude:double, location:chararray, hour_of_the_day:int, season:chararray);
DESCRIBE Crime_Data;

all_theft = FILTER Crime_Data BY (primary_type MATCHES 'THEFT');
grouped_theft = GROUP all_theft BY year;
count_crimes_per_year = foreach grouped_theft GENERATE group, COUNT(all_theft.id);
STORE count_crimes_per_year INTO 'crime_per_year' using PigStorage(',');

Group_By_Season_Primary = GROUP Crime_Data BY (season, primary_type);
Count_Season = foreach Group_By_Season_Primary GENERATE group, COUNT(Crime_Data.id);
STORE Count_Season INTO 'count_season.csv' using PigStorage(',');

Group_By_Season_Location = GROUP Crime_Data BY (season,location_description);
Count_location = foreach Group_By_Season_Location GENERATE group, COUNT(Crime_Data.id);
STORE Count_location INTO 'count_location' using PigStorage(',');

