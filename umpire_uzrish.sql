USE gameday;
SELECT  year(uu.date),
        u.id,
        uu.stand,
        count(*) as n,
        sum(uu.c) as cs,
        sum(uu.p) as p,
        sum(uu.c-uu.p) as x,
        sum(uu.c-uu.p)/count(*) as x_over_n,
        u.name
FROM    umpire_ucs_generic uu, 
        (select id,name from umpires where name!="" group by 1) as u 
WHERE   u.id=uu.umpire 
AND     year(uu.date)=2016
GROUP BY 1,2,3;
