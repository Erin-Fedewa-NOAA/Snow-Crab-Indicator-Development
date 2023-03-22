/* Formatted on 4/21/2022 01:09:51 PM (QP5 v5.374) */
------------------------------/*BSAI snow crab metrics*/--------------------------------


/* Annual number of active vessels*/

SELECT YEAR,
       --source pre-ratz from ADF&G AMR
       'ANNUAL NUMBER OF ACTIVE VESSELS BSS FISHERY'     INDICATOR_NAME,
       VESSELS                                           AS DATA_VALUE
  FROM ESSRP.ADFGCRABAMR_TABDATA
 WHERE FISHERY = 'BSS' AND YEAR <= 2005 AND NOT SEASON = '2005/06'
UNION
  --source post-ratz from eLandings
  SELECT EXTRACT (YEAR FROM DATE_OF_LANDING)               AS YEAR,
         'ANNUAL NUMBER OF ACTIVE VESSELS BSS FISHERY'     INDICATOR_NAME,
         COUNT (DISTINCT VESSEL_ADFG_NUMBER)               AS DATA_VALUE
    FROM AKR.V_CRAT_EL_REPORT
   WHERE     CRAB_FISHERY_CODE = 'BSS'
         AND MANAGEMENT_PROGRAM_CODE IN ('CRAT', 'CDQ')
         AND DISPOSITION_CODE = 60
         AND EXTRACT (YEAR FROM DATE_OF_LANDING) >= 2006
GROUP BY EXTRACT (YEAR FROM DATE_OF_LANDING)
ORDER BY 1;


/*Annual snow crab ex-vessel value*/

WITH
    DAT
    AS
        (  SELECT TO_CHAR (akfin_year)                       AS YEAR,
                  COUNT (DISTINCT N_ADFG)                    AS VESSELS,
                  COUNT (DISTINCT ADFG_H_PROCESSOR_CODE)     AS PROCESSORS,
                  SUM (cfec_value * g.gdp_adj)               AS cfec_value_ADJ
             FROM council.comprehensive_fT F
                  LEFT JOIN
                  (SELECT *
                     FROM AFSC.GDP_ADJUST_V
                    WHERE INFLATION_YEAR =
                          (SELECT INFLATION_YEAR FROM AFSC.SAFE_INFLATIONYEAR))
                  G
                      ON F.AKFIN_YEAR = G.YEAR
            WHERE     CFEC_LANDING_STATUS = 'C'
                  AND CFEC_VALUE > 0
                  AND crab_fishery = 'BSS'
                  AND akfin_year >= 1991
         GROUP BY AKFIN_YEAR, CRAB_FISHERY)
  SELECT YEAR,
         'REAL ANNUAL EX-VESSEL VALUE BSS FISHERY'    AS INDICATOR_NAME,
         CASE
             WHEN VESSELS < 4 OR PROCESSORS < 4 THEN NULL
             ELSE ROUND (CFEC_VALUE_ADJ)
         END                                          AS DATA_VALUE
    FROM DAT
ORDER BY 1;


/*Annual snow crab ex-vessel price per pound*/

WITH
    DAT
    AS
        (  SELECT TO_CHAR (akfin_year)                                      AS YEAR,
                  COUNT (DISTINCT N_ADFG)                                   AS VESSELS,
                  COUNT (DISTINCT ADFG_H_PROCESSOR_CODE)                    AS PROCESSORS,
                  SUM (cfec_value) / SUM (CFEC_WHOLE_POUNDS) * G.gdp_adj    AS cfec_PRICE_ADJ
             FROM council.comprehensive_fT F
                  LEFT JOIN
                  (SELECT *
                     FROM AFSC.GDP_ADJUST_V
                    WHERE INFLATION_YEAR =
                          (SELECT INFLATION_YEAR FROM AFSC.SAFE_INFLATIONYEAR))
                  G
                      ON F.AKFIN_YEAR = G.YEAR
            WHERE     CFEC_LANDING_STATUS = 'C'
                  AND CFEC_VALUE > 0
                  AND crab_fishery = 'BSS'
                  AND akfin_year >= 1991
         GROUP BY AKFIN_YEAR, CRAB_FISHERY, G.GDP_ADJ)
  SELECT YEAR,
         'REAL ANNUAL EX-VESSEL PRICE BSS FISHERY'    AS INDICATOR_NAME,
         CASE
             WHEN VESSELS < 4 OR PROCESSORS < 4 THEN NULL
             ELSE ROUND (cfec_PRICE_ADJ, 2)
         END                                          AS DATA_VALUE
    FROM DAT
ORDER BY 1;


/*Annual snow crab ex-vessel revenue share*/

WITH
    DAT
    AS
        (  SELECT FT.akfin_year,
                  FT.n_adfg,
                  SUM (
                      CASE
                          WHEN crab_fishery = 'BSS' THEN CFEC_VALUE
                          ELSE NULL
                      END)            AS FISHERY_VAL,
                  SUM (CFEC_VALUE)    AS ALLFISHERY_VAL
             FROM COUNCIL.COMPREHENSIVE_FT FT
                  INNER JOIN
                  (SELECT DISTINCT AKFIN_YEAR, N_ADFG
                     FROM COUNCIL.COMPREHENSIVE_FT
                    WHERE     CFEC_LANDING_STATUS = 'C'
                          AND CFEC_VALUE > 0
                          AND CRAB_FISHERY = 'BSS') VES
                      ON     FT.AKFIN_YEAR = VES.AKFIN_YEAR
                         AND FT.N_ADFG = VES.N_ADFG
            WHERE FT.CFEC_LANDING_STATUS = 'C'
         GROUP BY FT.AKFIN_YEAR, FT.N_ADFG)
  SELECT AKFIN_YEAR                               AS YEAR,
         'EX-VESSEL REVENUE SHARE BSS FISHERY'    AS INDICATOR_NAME,
         CASE
             WHEN COUNT (DISTINCT N_ADFG) < 4 THEN NULL
             ELSE SUM (FISHERY_VAL) / SUM (ALLFISHERY_VAL)
         END                                      AS DATA_VALUE
    FROM DAT
GROUP BY AKFIN_YEAR
ORDER BY AKFIN_YEAR ASC;



/*Annual incidental catch of snow crab  in groundfish fishery*/

  SELECT --source pre<2010 from historical estimates
         TO_NUMBER (SUBSTR (CRAB_YEAR, 6, 4))     AS YEAR,
         'INCIDENTAL CATCH KG OF BSS CRAB'        AS indicator_name,
         ROUND (SUM (ESTIMATE_WT_KG), 2)          AS data_value
    FROM AKFIN_MARTS.CRAB_BYCATCH_19912009
   WHERE SPECIES_GROUP_CODE = 'OTCR'
GROUP BY SUBSTR (CRAB_YEAR, 6, 4)
UNION
  SELECT YEAR,
         'INCIDENTAL CATCH KG OF BSS CRAB'     AS INDICATOR_NAME,
         ROUND (SUM (ESTIMATE_WT), 2)          AS DATA_VALUE
    FROM AKFIN_MARTS.CRAB_BYCATCH_ESTIMATE
   WHERE FISHERY_CODE = 'BSS' AND YEAR >= 2010
   AND YEAR< EXTRACT(YEAR FROM SYSDATE)
GROUP BY YEAR
ORDER BY YEAR