SELECT
  CASE
    WHEN EXTRACT(MONTH FROM `Order Date`) IN (3,4,5) THEN 'Spring'
    WHEN EXTRACT(MONTH FROM `Order Date`) IN (6,7,8) THEN 'Summer'
    WHEN EXTRACT(MONTH FROM `Order Date`) IN (9,10,11) THEN 'Autumn'
    ELSE 'Winter'
  END AS Season,  
  ROUND(AVG(CAST(Discount AS FLOAT64)),2) AS Average_Discount
FROM 
  `casestudy-435401.SuperStore.Superstore`
WHERE
  REGEXP_CONTAINS(Discount, r'^[0-9]+(\.[0-9]+)?$')  
GROUP BY
  Season
ORDER BY
  Season;