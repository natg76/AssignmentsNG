##  Assignment - Hive - NYT TLC TRIP DATA ##



## Create External Table for Nov & Dec Trip Data 

drop table ng_nyc_trip_small;

create external table if not exists ng_nyc_trip_small
(
	vendorid	string,
	tpep_pickup_datetime	string,
	tpep_dropoff_datetime	string,
	passenger_count	int,
	trip_distance	float,
	ratecodeid	int,
	store_and_fwd_flag	string,
	pulocationid	string,
	dolocationid	string,
	payment_type	int,
	fare_amount	float,
	extra	float,
	mta_tax	float,
	tip_amount	float,
	tolls_amount	float,
	improvement_surcharge	float,
	total_amount	float
)
	ROW format delimited fields terminated by ','
	location 's3://ng-pgdss-in-1/TLCTripData/'
--	tblproperties ("skip.header.line.count"="1")	
	tblproperties ("skip.header.line.count"="2")
	
;


-- Query The data to check if records looks fine by visual inspection

Select * from ng_nyc_trip_small;


-- Issue 1:  Both files were having headers, which are removed during table creation itself.
-- Issue 2: There is a blank row at the top of both files. So set the "skip.header.line.count = 2"


-- Data Quality Analysis

-- Q1: How many records has each TPEP provider provided? Write a query that summarises the number of records of each provider.

Select VendorID, count(1) 
From NG_TLC_TRIP_DATA
group by VendorID;


-- Q2: The data provided is for months November and December only. 
--     Check whether the data is consistent, and if not, identify the data quality issues. Mention all data quality issues in comments.

-- ANS:  Following DQ Checks are done on the data.
-- 1. On string data types, number of null values in the table
-- 2. On ID fields also, checks are done for NULL values.
-- 3. On Amount fields, checking for 0 or NULL values. Fare Amount and Total AMount are expected to be non-zero positive values.
--    where as tip, extra, mta_tax etc can be 0, but not expected tto be negative.
-- 4. pickup & dropoff time are not expected to be blank and dropoff time is expected to be greater than pickup time.
-- 5. Location id is not expected to be blank and Pickup Location and Dropoff locations are expected to be different

Select 
    SUM(case when vendorid is null then 1 else 0 end) as VendorID_IS_Blank,
    SUM(case when tpep_pickup_datetime is null then 1 else 0 end) as PUTime_IS_Blank,
    SUM(case when tpep_dropoff_datetime is null then 1 else 0 end) as DOTime_IS_Blank,
    SUM(case when pulocationid is null then 1 else 0 end) as pulocationid_IS_Blank,
    SUM(case when dolocationid is null then 1 else 0 end) as dolocationid_IS_Blank,
    SUM(case when payment_type is null then 1 else 0 end) as payment_type_IS_Blank,
    SUM(case when ratecodeid is null then 1 else 0 end) as ratecodeid_IS_Blank,
    SUM(case when store_and_fwd_flag is null then 1 else 0 end) as store_and_fwd_flag_IS_Blank,	
    SUM(case when passenger_count <= 0 then 1 else 0 end) as passenger_count_IS_invalid,
    SUM(case when trip_distance <= 0 then 1 else 0 end) as trip_distance_IS_Invalid,
    SUM(case when fare_amount <= 0 then 1 else 0 end) as fare_amount_is_Invalid,
    SUM(case when extra < 0 then 1 else 0 end) as extra_is_Invalid,
    SUM(case when mta_tax < 0 then 1 else 0 end) as mta_tax_is_Invalid,
    SUM(case when tip_amount < 0 then 1 else 0 end) as tip_amount_is_Invalid,
    SUM(case when tolls_amount < 0 then 1 else 0 end) as tolls_amount_is_Invalid,
	SUM(case when improvement_surcharge < 0 then 1 else 0 end) as improvement_surcharge_is_Invalid,
    SUM(case when total_amount <= 0 then 1 else 0 end) as total_amount_is_Invalid
From ng_tlc_trip_data
where vendorid is null  or
	tpep_pickup_datetime is null or
	tpep_dropoff_datetime is null or
	passenger_count	<= 0 or
	trip_distance <= 0 or
	ratecodeid	 is null or
	store_and_fwd_flag	 is null or
	pulocationid is null or
	dolocationid is null or
	payment_type is null or
	fare_amount <= 0 or
	extra < 0 or 
	mta_tax	< 0 or
	tip_amount	< 0 or
	tolls_amount < 0 or
	improvement_surcharge < 0 or
	total_amount <= 0
;




-- Q3: You might have encountered unusual or erroneous rows in the dataset. 
--     Can you conclude which vendor is doing a bad job in providing the records?

SELECT
    VendorID,
	count(1) as ERROR_COUNT
From ng_tlc_trip_data_l
where vendorid is null  or
	tpep_pickup_datetime is null or
	tpep_dropoff_datetime is null or
	passenger_count	<= 0 or
	trip_distance <= 0 or
	ratecodeid	 is null or
	store_and_fwd_flag	 is null or
	pulocationid is null or
	dolocationid is null or
	payment_type is null or
	fare_amount <= 0 or
	extra < 0 or 
	mta_tax	< 0 or
	tip_amount	< 0 or
	tolls_amount < 0 or
	improvement_surcharge < 0 or
	total_amount <= 0
group by vendorid	
;

-- From the Results the Vendor ID 1 seems to have higher number of erroneous records compared to Error 2	178485
--	2	59111

-- Analysis-I

-- 1. Compare the average fare for November and December.

-- ANS:  Comparing the average fare month by Month: Results are in different rows format.
--       Pickup Date contains a record with pickup date on 31-Oct also. 
--       Hence it can be changed based on the drop timestamp.

Select 
    month(tpep_pickup_datetime) as Trip_Month, 
	avg(fare_amount) as AVG_FARE_AMOUNT
From NG_TLC_TRIP_DATA
group by month(tpep_pickup_datetime);

Select 
    month(tpep_dropoff_datetime) as Trip_Month, 
	avg(fare_amount) as AVG_FARE_AMOUNT
From NG_TLC_TRIP_DATA_
group by month(tpep_dropoff_datetime);

-- Both Pickup Date and Drop off date contains some records for 31-oct.

	

-- 2. Explore the ‘number of passengers per trip’ - how many trips are made by each level of ‘Passenger_count’? 
--    Do most people travel solo or with other people?

-- ANS: Obtaining no_of_trips by passenger count will provide a view on whether people travel solo or with others.


Select 
    passenger_count,
	count(*) as no_of_trips,
From NG_TLC_TRIP_DATA
group by passenger_count;


-- 3. Which is the most preferred mode of payment?

-- ANS: Obtaining no_of_trips by payment_type will give information on most preferred payment mode. 

Select 
    payment_type,
	count(*) as no_of_trips_by_payment_type
From NG_TLC_TRIP_DATA
group by payment_type
order by no_of_trips_by_payment_type desc;
	


-- 4. What is the average tip paid? Compare the average tip with the 25th, 50th and 75th percentiles and 
--    comment whether the ‘average tip’ is a representative statistic (of the central tendency) of ‘tip amount paid’.

Select 
	avg(tip_amount) Average_Tip_Paid, 
	percentile_approx(tip_amount,0.25) Percentile_25th, 
	percentile_approx(tip_amount,0.5) Percentile_50th, 
	percentile_approx(tip_amount,0.75) Percentile_75th
from NG_TLC_TRIP_DATA;

-- Had to use the percentile_approx function as the the data type is float.
-- From the results, it looks like the Average Tip Paid is 

-- 5. Explore the ‘Extra’ (charge) variable - what is the fraction of total trips where an extra charge is levied?

Select 
    sum(case when extra > 0 then 1 else 0 end) / count(*) as fraction_extra
From NG_TLC_TRIP_DATA;

-- Analysis-II

-- 1. What is the correlation between the number of passengers and tip paid? Do multiple travellers pay more compared to solo travellers?

Select 
   corr(passenger_count, tip_amount), 
   corr(passenger_count, total_amount) 
from NG_TLC_TRIP_DATA;

-- 2. Create five buckets of ‘tip paid’: [0-5), [5-10), [10-15) , [15-20) and >=20. 
--    Calculate the percentage share of each bucket (i.e. the fraction of trips falling in each bucket).


Select
    sum(case when tip_amount <= 5 then 1 else 0 end) / count(*) as fr_0to5_bucket ,
	sum(case when tip_amount > 5 and tip_amount <= 10 then 1 else 0 end) / count(*) as fr_5to10_bucket ,
	sum(case when tip_amount > 10 and tip_amount <= 15 then 1 else 0 end) / count(*) as fr_10_15_bucket,
	sum(case when tip_amount > 15 and tip_amount <= 20 then 1 else 0 end) / count(*) as fr_0_5_bucket ,
	sum(case when tip_amount >= 20 then 1 else 0 end) / count(*) as fr_above20_bucket
from NG_TLC_TRIP_DATA;


-- 3. Which month has a greater average ‘speed’ - November or December? Note that the variable ‘speed’ will have to be derived from other metrics.

select 
     trip_month,
	 avg(speed_mph) as average_speed
from (
     SELECT 
	     month(from_unixtime(tpep_pickup_datetime)) as trip_month,
		 trip_distance,
	     (unix_timestamp(tpep_dropoff_datetime)  - unix_timestamp(tpep_pickup_datetime))/(60*60) as trip_duration_hrs,
		 trip_distance / ((unix_timestamp(tpep_dropoff_datetime)  - unix_timestamp(tpep_pickup_datetime))/(60*60) ) as speed_mph,
	 From NG_TLC_TRIP_DATA
	 )
group by trip_month;

-- 4. Analyse the average speed of the most happening days of the year 
--    i.e. 31st December (New year’s eve) and 25th December (Christmas Eve) and compare it with the overall average. 

-- ANS: 
-- There is a mistake in Question as to the the day for which Analysis is required is mentioned as 25-Dec but in brackets it is mentioned as Christmas Eve which is on 24th.
-- For answer, processing with 25-Dec.

select 
     avg(speed_mph) as speed_NY_eve	 	 
from (
     SELECT 
	     to_date(tpep_pickup_datetime) as date_trip,
		 trip_distance / ((unix_timestamp(tpep_dropoff_datetime)  - unix_timestamp(tpep_pickup_datetime))/(60*60) ) as speed_mph,
	 From NG_TLC_TRIP_DATA
	 where date_trip = ('2017-12-31') -- '2017-12-25'
	 );

	 
select 
     avg(speed_mph) as speed_xmas	 	 
from (
     SELECT 
	     to_date(tpep_pickup_datetime) as date_trip,
		 trip_distance / ((unix_timestamp(tpep_dropoff_datetime)  - unix_timestamp(tpep_pickup_datetime))/(60*60) ) as speed_mph,
	 From NG_TLC_TRIP_DATA
	 where date_trip = ('2017-12-25') -- '2017-12-25'
	 );


select 
     avg(speed_mph) as speed_average	 	 
from (
     SELECT 
	     to_date(tpep_pickup_datetime) as date_trip,
		 trip_distance / ((unix_timestamp(tpep_dropoff_datetime)  - unix_timestamp(tpep_pickup_datetime))/(60*60) ) as speed_mph,
	 From NG_TLC_TRIP_DATA
	 );



-- If results required in a single Query:
	 
	 
select 
     avg(case when date_trip = '2017-12-31' then speed_mph end) new_year_speed,
	 avg(case when date_trip = '2017-12-25' then speed_mph end) new_year_speed,
	 avg(speed_mph) as average_speed,
	 
from (
     -- Sum the trip duration and distiance covered at daily level in inner query
     SELECT 
	     to_date(tpep_pickup_datetime) as date_trip,
		 sum(trip_distance) as trip_distance_in_the_day,
		 sum ((unix_timestamp(tpep_dropoff_datetime)  - unix_timestamp(tpep_pickup_datetime))/(60*60) ) as trip_duration_hrs,
	 From NG_TLC_TRIP_DATA
	 where month(unix_timestamp(tpep_pickup_datetime)) = '12'
	 group by to_date(tpep_pickup_datetime)
	 );