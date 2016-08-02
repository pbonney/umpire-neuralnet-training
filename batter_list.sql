SELECT	batter as id, substr(gameName,5,4) as year
FROM	gameday.atbats
WHERE   substr(gameName,5,4)>2016
GROUP BY 1,2
;
