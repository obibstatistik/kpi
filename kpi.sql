drop table if exists datamart.kpi_visits;

select month, location, visits2017,
sum(visits2017) OVER (PARTITION BY location ORDER BY month) AS visits2017cum,
 
(
case
when visits2017 = 0 or visits2016 = 0 then 0
else ((visits2017-visits2016)/visits2016)
end
) as diff1716,
visits2016, 
sum(visits2016) OVER (PARTITION BY location ORDER BY month) AS visits2016cum,
(
case
when visits2016 = 0 or visits2015 = 0 then 0
else ((visits2016-visits2015)/visits2015)
end
) as diff1615,
visits2015,
sum(visits2015) OVER (PARTITION BY location ORDER BY month) AS visits2015cum

into datamart.kpi_visits from 

(select month, location, sum(antal2017) as visits2017, sum(antal2016) as visits2016, sum(antal2015) as visits2015 from 

(select month, location,  
  (case
  when count > 0 and location in ('hb','bo','vo','mus','kor','ta','da','kor') and year = 2017 then count
  when count > 0 and location in ('hoj') and year = 2017 then (sum(count) + 1527)
  when count > 0 and location in ('ho') and year = 2017 then (sum(count) + 2490)
  when count <0 then 0
  else 0	 	
  end
  ) as antal2017,
  (case
  when count > 0 and location in ('hb','bo','vo','mus','kor','ta','da','kor') and year = 2016 then count
  when count > 0 and location in ('hoj') and year = 2016 then (sum(count) + 1583)
  when count > 0 and location in ('ho') and year = 2016 then (sum(count) + 2420)
  when count <0 then 0
  else 0	 	
  end
  ) as antal2016,
  (case
  when count > 0 and location in ('hb','bo','vo','mus','kor','ta','da','kor') and year = 2015 then count
  when count > 0 and location in ('hoj') and year = 2015 then (sum(count) + 1647)
  when count > 0 and location in ('ho') and year = 2015 then (sum(count) + 2423)
  when count <0 then 0
  else 0	 	
  end
  ) as antal2015 
from 
  (select extract(month from date) as month, extract(year from date) as year, location, sum(count) as count
  from public.people_counter group by month, location, year order by month desc) as y
group by month, location, year, y.count order by location, month) as z

group by month, location
order by location, month) as x;

 