SELECT 
  CASE # Months are extracted for seasonal sales comparisons.
    WHEN EXTRACT(MONTH FROM `Order Date`) IN (3,4,5) THEN 'Spring'
    WHEN EXTRACT(MONTH FROM `Order Date`) IN (6,7,8) THEN 'Summer'
    WHEN EXTRACT(MONTH FROM `Order Date`) IN (9,10,11) THEN 'Autumn'
    ELSE 'Winter'
  END AS Season,
  country,
  Category,
  `sub-Category`,
  SUM(CAST(Sales AS FLOAT64)) AS total_sales,
  SUM(CAST(Quantity AS FLOAT64)) AS total_Quantity,
  SUM(Profit) AS totla_Profit
FROM `casestudy-435401.SuperStore.Superstore` 
WHERE 
  SAFE_CAST(Sales AS FLOAT64) IS NOT NULL
GROUP BY
  season,country,Category,`sub-Category`
ORDER BY 
  total_sales DESC

