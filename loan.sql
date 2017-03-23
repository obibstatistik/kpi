drop table if exists datamart.kpi_loan;

SELECT Bibliotek, EXTRACT(MONTH FROM dato) as month, count(*) as Antal into datamart.kpi_loan FROM 
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
      ELSE 'Andet blandt andet fynbib'
             END) AS Bibliotek, stdate as dato
         FROM (
    SELECT * FROM stat2017 JOIN LOGIN ON "login"."login#" = "stat2017"."login#" WHERE TYPE IN ('1','2')
) AS indre ) AS ydre
GROUP BY month, Bibliotek
ORDER BY month;

INSERT INTO datamart.kpi_loan (SELECT Bibliotek, EXTRACT(MONTH FROM dato) as month, count(*) as Antal FROM 
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
      ELSE 'Andet blandt andet fynbib'
             END) AS Bibliotek, stdate as dato
         FROM (
    SELECT * FROM stat2016 JOIN LOGIN ON "login"."login#" = "stat2016"."login#" WHERE TYPE IN ('1','2')
) AS indre ) AS ydre
GROUP BY month, Bibliotek
ORDER BY month);

INSERT INTO datamart.kpi_loan (SELECT Bibliotek, EXTRACT(MONTH FROM dato) as month, count(*) as Antal FROM 
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
      ELSE 'Andet blandt andet fynbib'
             END) AS Bibliotek, stdate as dato
         FROM (
    SELECT * FROM stat2015 JOIN LOGIN ON "login"."login#" = "stat2015"."login#" WHERE TYPE IN ('1','2')
) AS indre ) AS ydre
GROUP BY month, Bibliotek
ORDER BY month);