-- List all the orders that were made in Taiwan
SELECT *
FROM food_panda.orders
WHERE country_name = 'Taiwan'



-- Find the Total GMV by country
SELECT country_name, SUM(gmv_local) AS total_gmv
FROM food_panda.orders
GROUP BY country_name



-- Find the top active vendor by GMV in each country
SELECT sub2.country_name, sub2.vendor_name, ROUND(sub2.total_gmv,2) AS total_gmv
FROM
(SELECT sub.country_name, sub.vendor_name, sub.total_gmv,
       RANK() OVER (PARTITION BY sub.country_name ORDER BY sub.total_gmv DESC) AS rank
FROM
  (SELECT orders.country_name, vendors.vendor_name, SUM(orders.gmv_local) AS total_gmv   
  FROM food_panda.orders orders
  LEFT JOIN food_panda.vendors vendors
  ON orders.vendor_id = vendors.id
  GROUP BY 1,2) sub ) sub2
WHERE sub2.rank = 1
ORDER BY sub2.country_name




-- - Find the top 2 vendors per country, in each year available in the dataset
SELECT sub2.year, sub2.country_name, sub2.vendor_name, sub2.total_gmv
FROM
  (SELECT sub.year, sub.country_name, sub.vendor_name, sub.total_gmv,
        RANK() OVER (PARTITION BY sub.year, sub.country_name ORDER BY sub.total_gmv DESC) AS rank
  FROM
    (SELECT CAST(orders.date_local AS datetime) AS year, 
          orders.country_name, 
          vendors.vendor_name, 
          ROUND(SUM(orders.gmv_local),2) AS total_gmv
    FROM food_panda.orders orders
    LEFT JOIN food_panda.vendors vendors
    ON orders.vendor_id = vendors.id
    GROUP BY 1,2,3) sub
  ) sub2
WHERE sub2.rank <3
ORDER BY sub2.year, sub2.country_name

