drop table if exists datamart.kpi_visits;

select month, location,  
  (case
  when count > 0 and location in ('hb','bo','vo','mus','kor','ta','da','kor') then count
  when count > 0 and location in ('hoj') and extract(year from month) = 2017 then (sum(count) + 1527)
  when count > 0 and location in ('ho') and extract(year from month) = 2017 then (sum(count) + 2490)
  when count > 0 and location in ('hoj') and extract(year from month) = 2016 then (sum(count) + 1583)
  when count > 0 and location in ('ho') and extract(year from month) = 2016 then (sum(count) + 2420)
  when count > 0 and location in ('hoj') and extract(year from month) = 2015 then (sum(count) + 1647)
  when count > 0 and location in ('ho') and extract(year from month) = 2015 then (sum(count) + 2423)
  when count <0 then 0	 	
  
end
) as antal into datamart.kpi_visits
from 
  (select (date_trunc('month', date))::date as month, location, sum(count) as count
  from public.people_counter group by month, location order by month desc) as y
group by month, location, y.count order by location, month;