# Querying average download speed per country in 2015
require(bigrquery);
downquery_template <- "SELECT
connection_spec.client_geolocation.country_code AS country,  
AVG(8 * web100_log_entry.snap.HCThruOctetsAcked/ (web100_log_entry.snap.SndLimTimeRwin +
web100_log_entry.snap.SndLimTimeCwnd + web100_log_entry.snap.SndLimTimeSnd)) AS downloadThroughput,
COUNT(DISTINCT test_id) AS tests,
FROM
plx.google:m_lab.ndt.all
WHERE
IS_EXPLICITLY_DEFINED(web100_log_entry.connection_spec.remote_ip)
AND IS_EXPLICITLY_DEFINED(web100_log_entry.connection_spec.local_ip)
AND IS_EXPLICITLY_DEFINED(web100_log_entry.snap.HCThruOctetsAcked)
AND IS_EXPLICITLY_DEFINED(web100_log_entry.snap.SndLimTimeRwin)
AND IS_EXPLICITLY_DEFINED(web100_log_entry.snap.SndLimTimeCwnd)
AND IS_EXPLICITLY_DEFINED(web100_log_entry.snap.SndLimTimeSnd)
AND project = 0
AND IS_EXPLICITLY_DEFINED(connection_spec.data_direction)
AND connection_spec.data_direction = 1
AND web100_log_entry.snap.HCThruOctetsAcked >= 8192
AND (web100_log_entry.snap.SndLimTimeRwin +
web100_log_entry.snap.SndLimTimeCwnd +
web100_log_entry.snap.SndLimTimeSnd) >= 9000000
AND (web100_log_entry.snap.SndLimTimeRwin +
web100_log_entry.snap.SndLimTimeCwnd +  
web100_log_entry.snap.SndLimTimeSnd) < 3600000000
AND IS_EXPLICITLY_DEFINED(web100_log_entry.snap.CongSignals)
AND web100_log_entry.snap.CongSignals > 0
AND (web100_log_entry.snap.State == 1 OR (web100_log_entry.snap.State >= 5 AND web100_log_entry.snap.State <= 11))
AND web100_log_entry.log_time >= PARSE_UTC_USEC('2015-01-01 00:00:00') / POW(10, 6) 
AND web100_log_entry.log_time < PARSE_UTC_USEC('2016-01-01 00:00:00') / POW(10, 6)
GROUP BY country
ORDER BY country ASC;";
downresult <- query_exec(downquery_template, project = "measurement-lab", max_pages = Inf);
require(rworldmap);
require(classInt);
require(RColorBrewer);

downloadmap <- joinCountryData2Map(downresult, joinCode = 'ISO2', nameJoinColumn = 'country', verbose = 'TRUE');
par(mai = c(0, 0, 0.2, 0), xaxs = "i", yaxs = "i");

#getting class intervals using a 'jenks' classification in classInt package
classInt <- classInt::classIntervals(downloadmap[["downloadThroughput"]], n = 5, style = "jenks");
catMethod = classInt[["brks"]];
#getting a colour scheme from the RColorBrewer package
colourPalette <- RColorBrewer::brewer.pal(5, 'RdPu');
mapParams <- mapCountryData(downloadmap, nameColumnToPlot = "downloadThroughput",
							mapTitle = "Download Speed (mbps)", catMethod = catMethod,
					 							colourPalette = colourPalette,
								   							addLegend = FALSE);
do.call(addMapLegend, c(mapParams, legendWidth = 0.5, legendLabels = "all",
						 legendIntervals = "data", legendMar = 2));