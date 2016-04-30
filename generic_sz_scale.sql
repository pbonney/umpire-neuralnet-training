use gameday;

select  z.year, 
        a.stand, 
        count(*), 
        sum(z.sz_top)/count(*) as top, 
        sum(z.sz_bot)/count(*) as bot 
from    atbats a, 
        pitches p, 
        batter_sz_top_bot z 
where   a.GameName=p.GameName 
and     a.num=p.gameAtBatID 
and     a.batter=z.batter 
and     substr(a.gameName,5,4)=z.year 
and     (p.des like "Called%" or p.des like "Ball%") 
group by 1, 2;
