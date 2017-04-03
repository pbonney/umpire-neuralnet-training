SELECT  year(uu.date) as year,
        uu.stand,
        count(*) as n,
        sum(uu.c) as cs,
        sum(uu.p) as p,
        sum(uu.c-uu.p) as x,
        sum(uu.c-uu.p)/count(*) as x_over_n
FROM    umpire_ucs_generic uu
WHERE   year(uu.date)=2017
GROUP BY 1,2;
