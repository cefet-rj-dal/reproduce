CREATE OR REPLACE VIEW v_window AS 
 SELECT v.dia_hora,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBPA'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbpa_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBPA'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbpa_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBPA'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbpa_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBPA'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbpa_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBPA'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbpa_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBFL'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfl_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBFL'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfl_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBFL'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfl_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBFL'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfl_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBFL'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfl_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBCT'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbct_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBCT'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbct_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBCT'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbct_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBCT'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbct_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBCT'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbct_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBGR'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgr_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBGR'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgr_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBGR'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgr_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBGR'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgr_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBGR'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgr_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBSP'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsp_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBSP'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsp_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBSP'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsp_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBSP'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsp_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBSP'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsp_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBKP'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbkp_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBKP'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbkp_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBKP'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbkp_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBKP'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbkp_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBKP'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbkp_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBGL'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgl_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBGL'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgl_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBGL'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgl_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBGL'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgl_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBGL'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgl_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBRJ'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrj_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBRJ'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrj_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBRJ'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrj_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBRJ'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrj_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBRJ'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrj_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBCF'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbcf_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBCF'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbcf_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBCF'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbcf_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBCF'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbcf_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBCF'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbcf_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBVT'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbvt_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBVT'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbvt_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBVT'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbvt_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBVT'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbvt_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBVT'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbvt_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBGO'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgo_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBGO'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgo_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBGO'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgo_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBGO'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgo_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBGO'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbgo_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBBR'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbr_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBBR'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbr_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBBR'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbr_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBBR'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbr_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBBR'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbr_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBEG'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbeg_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBEG'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbeg_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBEG'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbeg_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBEG'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbeg_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBEG'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbeg_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBBE'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbe_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBBE'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbe_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBBE'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbe_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBBE'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbe_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBBE'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbbe_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBSV'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsv_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBSV'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsv_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBSV'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsv_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBSV'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsv_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBSV'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbsv_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBRF'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrf_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBRF'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrf_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBRF'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrf_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBRF'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrf_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBRF'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbrf_4,
    max(
        CASE
            WHEN v.aeroporto::text = 'SBFZ'::text AND v.voos <> 0::numeric THEN trunc(v.atraso / v.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfz_0,
    max(
        CASE
            WHEN v1.aeroporto::text = 'SBFZ'::text AND v1.voos <> 0::numeric THEN trunc(v1.atraso / v1.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfz_1,
    max(
        CASE
            WHEN v2.aeroporto::text = 'SBFZ'::text AND v2.voos <> 0::numeric THEN trunc(v2.atraso / v2.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfz_2,
    max(
        CASE
            WHEN v3.aeroporto::text = 'SBFZ'::text AND v3.voos <> 0::numeric THEN trunc(v3.atraso / v3.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfz_3,
    max(
        CASE
            WHEN v4.aeroporto::text = 'SBFZ'::text AND v4.voos <> 0::numeric THEN trunc(v4.atraso / v4.voos, 4)
            ELSE 0::numeric
        END) AS f_sbfz_4
   FROM v_window_departure v
     LEFT JOIN v_window_departure v1 ON v.dia_hora = (v1.dia_hora - '01:00:00'::interval) AND v.aeroporto::text = v1.aeroporto::text
     LEFT JOIN v_window_departure v2 ON v.dia_hora = (v2.dia_hora - '02:00:00'::interval) AND v.aeroporto::text = v2.aeroporto::text
     LEFT JOIN v_window_departure v3 ON v.dia_hora = (v3.dia_hora - '03:00:00'::interval) AND v.aeroporto::text = v3.aeroporto::text
     LEFT JOIN v_window_departure v4 ON v.dia_hora = (v4.dia_hora - '04:00:00'::interval) AND v.aeroporto::text = v4.aeroporto::text
  WHERE date_part('hour'::text, v.dia_hora) = ANY (ARRAY[6::double precision, 7::double precision, 8::double precision, 9::double precision, 10::double precision, 11::double precision, 12::double precision, 13::double precision, 14::double precision, 15::double precision, 16::double precision, 17::double precision, 18::double precision, 19::double precision, 20::double precision])
  GROUP BY v.dia_hora
  ORDER BY v.dia_hora;

ALTER TABLE v_window
  OWNER TO anac;
