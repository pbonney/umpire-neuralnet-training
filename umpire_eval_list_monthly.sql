SELECT	x.name,
	u.id,
  a.stand,
  CAST(substr(u.gameName,10,2) as UNSIGNED) as month,
	CAST(substr(u.gameName,5,4) as UNSIGNED) as year,
	count(*) as pcount
FROM	umpires u, games g, pitches p, atbats a,
(SELECT id,name FROM umpires GROUP BY 1) x
WHERE	u.position='home'
AND	u.gameName=g.gameName
AND	u.gameName=p.gameName
AND	p.gameName=a.gameName
AND	a.num=p.gameAtBatID
AND	x.id=u.id
AND	g.type='R'
AND	p.des in ('Ball','Ball In Dirt','Called Strike')
AND	a.b_height_in is not null
AND	a.b_height_in != 0
AND	p.px is not null
AND	p.pz is not null
AND	substr(u.gameName,5,4)<=2016
GROUP BY 1,2,3,4,5
;
