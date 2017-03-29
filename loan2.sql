drop table if exists datamart.kpi_loan;

select month, library, sum(count2017) as loan2017, sum(count2016) as loan2016, sum(count2015) as loan2015 from 

(select * from (

(SELECT '2017' as year, EXTRACT(MONTH FROM dato) as month, library, count(*) as count2017 FROM 
  (SELECT
      (CASE      
      WHEN ID LIKE 'Z39.50' THEN 'Fornyelser'
      WHEN ID LIKE 'NCIPOC' THEN 'Fornyelser'
      WHEN ID LIKE 'WODENSE' OR ID LIKE 'WODENSEF' THEN 'Fornyelser'
      WHEN ID LIKE 'ALMA' THEN 'Fornyelser'
      WHEN ID LIKE 'hb%' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'SELVHB%' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'SELVSORT' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'lok%' THEN 'Historiens Hus'
      WHEN ID LIKE 'mus%' THEN 'Musikbiblioteket'
      WHEN ID LIKE 'SELVMUS%' THEN 'Musikbiblioteket'
      WHEN ID LIKE 'kor%' THEN 'Korup Bibliotek'
      WHEN ID LIKE 'SELVKOR%' THEN 'Korup Bibliotek'
      WHEN ID LIKE 'ho-%' THEN 'Holluf Pile Bibliotek'
      WHEN ID LIKE 'SELVHO1' OR ID LIKE 'SELVHO2' THEN 'Holluf Pile Bibliotek'
      WHEN ID LIKE 'hoj%' THEN 'Højby Bibliotek'
      WHEN ID LIKE 'SELVHØJ%' OR ID LIKE 'SELVHOJ%' THEN 'Højby Bibliotek'
      WHEN ID LIKE 'bo-%' THEN 'Bolbro Bibliotek'
      WHEN ID = 'SELVBO1' THEN 'Bolbro Bibliotek'
      WHEN ID LIKE 'da%' THEN 'Dalum Bibliotek'
      WHEN ID LIKE 'SELVDA%' THEN 'Dalum Bibliotek'
      WHEN ID LIKE 'ta%' THEN 'Tarup Bibliotek'
      WHEN ID LIKE 'SELVTA%' THEN 'Tarup Bibliotek'
      WHEN ID LIKE 'vo%' THEN 'Vollsmose Bibliotek'
      WHEN ID LIKE 'SELVVO%' THEN 'Vollsmose Bibliotek'
      WHEN ID LIKE 'ops%' THEN 'Opsøgende'	
      
             END) AS library, stdate as dato
         FROM (
    SELECT * FROM stat2017 JOIN LOGIN ON "login"."login#" = "stat2017"."login#" WHERE TYPE IN ('1','2') 
) AS indre ) AS ydre
WHERE library in ('Fornyelser','Hovedbiblioteket','Historiens Hus','Musikbiblioteket','Korup Bibliotek','Holluf Pile Bibliotek','Højby Bibliotek','Bolbro Bibliotek','Dalum Bibliotek','Tarup Bibliotek','Vollsmose Bibliotek','Opsøgende')
GROUP BY month, library
ORDER BY month) AS a

UNION

(SELECT '2016' as year, EXTRACT(MONTH FROM dato) as month, library, count(*) as count2016 FROM 
  (SELECT
      (CASE      
      WHEN ID LIKE 'Z39.50' THEN 'Fornyelser'
      WHEN ID LIKE 'NCIPOC' THEN 'Fornyelser'
      WHEN ID LIKE 'WODENSE' OR ID LIKE 'WODENSEF' THEN 'Fornyelser'
      WHEN ID LIKE 'ALMA' THEN 'Fornyelser'
      WHEN ID LIKE 'hb%' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'SELVHB%' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'SELVSORT' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'lok%' THEN 'Historiens Hus'
      WHEN ID LIKE 'mus%' THEN 'Musikbiblioteket'
      WHEN ID LIKE 'SELVMUS%' THEN 'Musikbiblioteket'
      WHEN ID LIKE 'kor%' THEN 'Korup Bibliotek'
      WHEN ID LIKE 'SELVKOR%' THEN 'Korup Bibliotek'
      WHEN ID LIKE 'ho-%' THEN 'Holluf Pile Bibliotek'
      WHEN ID LIKE 'SELVHO1' OR ID LIKE 'SELVHO2' THEN 'Holluf Pile Bibliotek'
      WHEN ID LIKE 'hoj%' THEN 'Højby Bibliotek'
      WHEN ID LIKE 'SELVHØJ%' OR ID LIKE 'SELVHOJ%' THEN 'Højby Bibliotek'
      WHEN ID LIKE 'bo-%' THEN 'Bolbro Bibliotek'
      WHEN ID = 'SELVBO1' THEN 'Bolbro Bibliotek'
      WHEN ID LIKE 'da%' THEN 'Dalum Bibliotek'
      WHEN ID LIKE 'SELVDA%' THEN 'Dalum Bibliotek'
      WHEN ID LIKE 'ta%' THEN 'Tarup Bibliotek'
      WHEN ID LIKE 'SELVTA%' THEN 'Tarup Bibliotek'
      WHEN ID LIKE 'vo%' THEN 'Vollsmose Bibliotek'
      WHEN ID LIKE 'SELVVO%' THEN 'Vollsmose Bibliotek'
      WHEN ID LIKE 'ops%' THEN 'Opsøgende'	
      
             END) AS library, stdate as dato
         FROM (
    SELECT * FROM stat2016 JOIN LOGIN ON "login"."login#" = "stat2016"."login#" WHERE TYPE IN ('1','2')
) AS indre ) AS ydre
WHERE library in ('Fornyelser','Hovedbiblioteket','Historiens Hus','Musikbiblioteket','Korup Bibliotek','Holluf Pile Bibliotek','Højby Bibliotek','Bolbro Bibliotek','Dalum Bibliotek','Tarup Bibliotek','Vollsmose Bibliotek','Opsøgende')
GROUP BY month, library
ORDER BY month) AS b

UNION

(SELECT '2015' as year, EXTRACT(MONTH FROM dato) as month, library, count(*) as count2015 FROM 
  (SELECT
      (CASE      
      WHEN ID LIKE 'Z39.50' THEN 'Fornyelser'
      WHEN ID LIKE 'NCIPOC' THEN 'Fornyelser'
      WHEN ID LIKE 'WODENSE' OR ID LIKE 'WODENSEF' THEN 'Fornyelser'
      WHEN ID LIKE 'ALMA' THEN 'Fornyelser'
      WHEN ID LIKE 'hb%' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'SELVHB%' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'SELVSORT' THEN 'Hovedbiblioteket'
      WHEN ID LIKE 'lok%' THEN 'Historiens Hus'
      WHEN ID LIKE 'mus%' THEN 'Musikbiblioteket'
      WHEN ID LIKE 'SELVMUS%' THEN 'Musikbiblioteket'
      WHEN ID LIKE 'kor%' THEN 'Korup Bibliotek'
      WHEN ID LIKE 'SELVKOR%' THEN 'Korup Bibliotek'
      WHEN ID LIKE 'ho-%' THEN 'Holluf Pile Bibliotek'
      WHEN ID LIKE 'SELVHO1' OR ID LIKE 'SELVHO2' THEN 'Holluf Pile Bibliotek'
      WHEN ID LIKE 'hoj%' THEN 'Højby Bibliotek'
      WHEN ID LIKE 'SELVHØJ%' OR ID LIKE 'SELVHOJ%' THEN 'Højby Bibliotek'
      WHEN ID LIKE 'bo-%' THEN 'Bolbro Bibliotek'
      WHEN ID = 'SELVBO1' THEN 'Bolbro Bibliotek'
      WHEN ID LIKE 'da%' THEN 'Dalum Bibliotek'
      WHEN ID LIKE 'SELVDA%' THEN 'Dalum Bibliotek'
      WHEN ID LIKE 'ta%' THEN 'Tarup Bibliotek'
      WHEN ID LIKE 'SELVTA%' THEN 'Tarup Bibliotek'
      WHEN ID LIKE 'vo%' THEN 'Vollsmose Bibliotek'
      WHEN ID LIKE 'SELVVO%' THEN 'Vollsmose Bibliotek'
      WHEN ID LIKE 'ops%' THEN 'Opsøgende'	
      
             END) AS library, stdate as dato
         FROM (
    SELECT * FROM stat2015 JOIN LOGIN ON "login"."login#" = "stat2015"."login#" WHERE TYPE IN ('1','2')
) AS indre ) AS ydre
WHERE library in ('Fornyelser','Hovedbiblioteket','Historiens Hus','Musikbiblioteket','Korup Bibliotek','Holluf Pile Bibliotek','Højby Bibliotek','Bolbro Bibliotek','Dalum Bibliotek','Tarup Bibliotek','Vollsmose Bibliotek','Opsøgende')
GROUP BY month, library
ORDER BY month 

)) as n
group by month, library;

/* */

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

from 

(select month, location, sum(antal2017) as visits2017, sum(antal2016) as visits2016, sum(antal2015) as visits2015 from 

(select month, library,   
  (case
  when count > 0 and year = '2017' then count	 	
  end
  ) as antal2017,
  (case
  when count > 0 and year = '2016' then count	 	
  end
  ) as antal2016,
  (case
  when count > 0 and year = '2015' then count	 	
  end
  ) as antal2015 
from datamart.kpi_loan
group by month, library, year order by location, month) as z

group by month, location
order by location, month) as x;
