SELECT u.*,
      cast((p.px > -0.85 AND p.px < 0.85 AND p.pz > 1.5 AND p.pz < 3.6) as signed integer) as naive
FROM umpire_roc u, pitches p
WHERE u.gamedayPitchID=p.gamedayPitchID
AND   nn_ind != -999
AND   nn_gen != -999
AND   nn_pdata != -999
AND   roeg != -999
AND   month(date) > 4
AND   year(date) > 2007
;
