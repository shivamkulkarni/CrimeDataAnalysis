CREATE DATABASE IF NOT EXISTS MyDb;

CREATE TABLE IF NOT EXISTS MyDb.CrimeData
(no INT,
id INT,
case_number VARCHAR(10000),
date_field STRING,
block VARCHAR(10000),
IUCR VARCHAR(10000),
primary_type VARCHAR(10000),
description VARCHAR(10000),
location_description VARCHAR(10000),
arrest VARCHAR(10000),
domestic VARCHAR(10000),
beat INT,
district INT,
ward INT,
community_area INT,
fbi_code VARCHAR(10000),
x_coordinate INT,
y_coordinate INT,
year INT,
updated_on STRING,
latitude DOUBLE,
longitude DOUBLE,
hour_of_the_day INT,
location STRING)
ROW FORMAT DELIMITED FIELDS TERMINATED BY ',' STORED AS textfile LOCATION
'/user/maria_dev/data.csv'
TBLPROPERTIES("skip.header.line.count"="1");

LOAD DATA LOCAL INPATH './cleaned_data.csv' OVERWRITE INTO TABLE MyDb.CrimeData;

SELECT  primary_type, COUNT(id) as cnt from MyDb.CrimeData GROUP BY primary_type ORDER BY cnt DESC LIMIT 3;

SELECT location_description, COUNT(id) as cnt from MyDb.CrimeData GROUP BY location_description ORDER BY cnt DESC LIMIT 10;

SELECT hour_of_the_day, primary_type, COUNT(id) as cnt from MyDb.CrimeData GROUP BY hour_of_the_day, primary_type ORDER BY hour_of_the_day;

SELECT hour_of_the_day, location_description, COUNT(id) as cnt from MyDb.CrimeData WHERE hour_of_the_day > 0 AND hour_of_the_day < 25 GROUP BY hour_of_the_day, location_description ORDER BY cnt DESC LIMIT 15;
