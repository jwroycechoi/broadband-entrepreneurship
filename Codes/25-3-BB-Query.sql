# What are the median download speeds in US states by county?
WITH
county_dl AS (
  SELECT
    test_date,
    client.Geo.region AS state,
    counties.county_name AS county_name,
    a.MeanThroughputMbps as mbps,
    NET.SAFE_IP_FROM_STRING(Client.IP) as ip
  FROM `measurement-lab.ndt.unified_downloads`,
        `bigquery-public-data.geo_us_boundaries.counties` counties
  WHERE client.Geo.country_name = "United States"
  AND ST_WITHIN(
    ST_GeogPoint(client.Geo.longitude, client.Geo.latitude), counties.county_geom
  )
  AND client.Geo.region IS NOT NULL
  AND client.Geo.region != ""
),
# With good locations and valid IPs
county_dl_cleaned AS (
  SELECT
    test_date,
    state,
    county_name,
    mbps,
    ip
  FROM county_dl
  WHERE ip IS NOT NULL AND state IS NOT NULL AND state != "" AND county_name != ""
),
county_dl_sample AS (
  SELECT
    COUNT(*) AS dl_sample_size,
    state AS dl_sample_state,
    county_name AS dl_sample_county_name
  FROM county_dl_cleaned
  GROUP BY state, county_name
),
county_median_per_ip_dl AS (
  SELECT test_date, ip, state, county_name,
    APPROX_QUANTILES(mbps, 100)[SAFE_ORDINAL(50)] AS mbps
  FROM county_dl_cleaned
  WHERE ip IS NOT NULL
  AND test_date >= '2019-12-01'
  AND test_date <= '2019-12-31'
  GROUP BY test_date, ip, state, county_name
),
county_ul AS (
  SELECT
    test_date,
    client.Geo.region AS state,
    counties.county_name AS county_name,
    a.MeanThroughputMbps AS mbps,
    NET.SAFE_IP_FROM_STRING(Client.IP) AS ip
  FROM `measurement-lab.ndt.unified_uploads`,
        `bigquery-public-data.geo_us_boundaries.counties` counties
  WHERE client.Geo.country_name = "United States"
  AND ST_WITHIN(
    ST_GeogPoint(client.Geo.longitude, client.Geo.latitude), counties.county_geom
  )
  AND client.Geo.region IS NOT NULL
  AND client.Geo.region != ""
),
# With good locations and valid IPs
county_ul_cleaned AS (
  SELECT
    test_date,
    county_name,
    state,
    mbps,
    ip
  FROM county_ul
  WHERE ip IS NOT NULL AND state IS NOT NULL AND state != "" AND county_name != ""
),
county_ul_sample AS (
  SELECT
    COUNT(*) AS ul_sample_size,
    state AS ul_sample_state,
    county_name AS ul_sample_county_name
  FROM county_ul_cleaned
  GROUP BY state, county_name
),
county_median_per_ip_ul AS (
  SELECT test_date, state, county_name, ip,
    APPROX_QUANTILES(mbps, 100)[SAFE_ORDINAL(50)] AS mbps
  FROM county_ul_cleaned
  WHERE ip IS NOT NULL
  AND test_date >= '2019-12-01'
  AND test_date <= '2019-12-31'
  GROUP BY test_date, ip, state, county_name
),
county_med_daily_ip_dl_medians AS (
  SELECT state AS DLmed_state, county_name AS DLmed_county_name, APPROX_QUANTILES(mbps, 100)[SAFE_ORDINAL(50)] AS DLmbps
  FROM county_median_per_ip_dl
  WHERE test_date >= '2019-12-01'
  AND test_date <= '2019-12-31'
GROUP BY state, county_name
ORDER BY state, county_name ASC
),
county_med_daily_ip_ul_medians AS (
  SELECT state AS ULmed_state, county_name AS ULmed_county_name, APPROX_QUANTILES(mbps, 100)[SAFE_ORDINAL(50)] AS ULmbps
  FROM county_median_per_ip_ul
  WHERE test_date >= '2019-12-01'
  AND test_date <= '2019-12-31'
GROUP BY state, county_name
ORDER BY state, county_name ASC
),
frac_25_3_BB AS (
  SELECT d.state AS BB_state, d.county_name AS BB_county_name, COUNTIF(d.mbps >= 25 AND u.mbps >= 3) / COUNT(*) AS frac_BB,
  COUNT(*) AS BB_samples
  FROM `county_median_per_ip_dl` AS d, `county_median_per_ip_ul` AS u
  GROUP BY d.state, d.county_name
),
frac_under_25_DL AS (
  SELECT state, county_name, COUNTIF(mbps < 25) / COUNT(*) AS frac_under_25mbpsDL,
    COUNT(*) AS samples
  FROM county_median_per_ip_dl
  GROUP BY state, county_name
)
SELECT * FROM frac_under_25_DL
JOIN frac_25_3_BB ON frac_25_3_BB.BB_state = frac_under_25_DL.state
  AND frac_25_3_BB.BB_county_name = frac_under_25_DL.county_name
JOIN county_med_daily_ip_dl_medians ON county_med_daily_ip_dl_medians.DLmed_state = frac_under_25_DL.state
  AND county_med_daily_ip_dl_medians.DLmed_county_name = frac_under_25_DL.county_name
JOIN county_med_daily_ip_ul_medians ON county_med_daily_ip_ul_medians.ULmed_state = frac_under_25_DL.state
  AND county_med_daily_ip_ul_medians.ULmed_county_name = frac_under_25_DL.county_name
JOIN county_ul_sample ON county_ul_sample.ul_sample_state = frac_under_25_DL.state
  AND county_ul_sample.ul_sample_county_name = frac_under_25_DL.county_name
JOIN county_dl_sample ON county_dl_sample.dl_sample_state = frac_under_25_DL.state
  AND county_dl_sample.dl_sample_county_name = frac_under_25_DL.county_name
