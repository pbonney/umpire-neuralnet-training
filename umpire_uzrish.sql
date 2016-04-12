USE gameday;
SELECT  u.id,
        uu.stand,
        count(*) as n,
        sum(uu.c-uu.p) as x,
        sum(uu.c-uu.p)/count(*) as x_over_n,
        u.name
FROM    umpire_ucs_generic uu, 
        (select id,name from umpires where name!="" group by 1) as u 
WHERE   u.id=uu.umpire 
GROUP BY 1,2;
