CREATE OR REPLACE VIEW v_high_percentage AS 
 SELECT trunc(sum(
        CASE
            WHEN v_window.f_sbpa_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbpa,
    trunc(sum(
        CASE
            WHEN v_window.f_sbfl_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbfl,
    trunc(sum(
        CASE
            WHEN v_window.f_sbct_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbct,
    trunc(sum(
        CASE
            WHEN v_window.f_sbgr_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbgr,
    trunc(sum(
        CASE
            WHEN v_window.f_sbsp_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbsp,
    trunc(sum(
        CASE
            WHEN v_window.f_sbkp_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbkp,
    trunc(sum(
        CASE
            WHEN v_window.f_sbgl_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbgl,
    trunc(sum(
        CASE
            WHEN v_window.f_sbrj_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbrj,
    trunc(sum(
        CASE
            WHEN v_window.f_sbcf_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbcf,
    trunc(sum(
        CASE
            WHEN v_window.f_sbvt_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbvt,
    trunc(sum(
        CASE
            WHEN v_window.f_sbgo_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbgo,
    trunc(sum(
        CASE
            WHEN v_window.f_sbbr_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbbr,
    trunc(sum(
        CASE
            WHEN v_window.f_sbeg_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbeg,
    trunc(sum(
        CASE
            WHEN v_window.f_sbbe_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbbe,
    trunc(sum(
        CASE
            WHEN v_window.f_sbsv_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbsv,
    trunc(sum(
        CASE
            WHEN v_window.f_sbrf_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbrf,
    trunc(sum(
        CASE
            WHEN v_window.f_sbfz_0 >= 0.3333 THEN 1
            ELSE 0
        END)::numeric / count(*)::numeric, 4) AS sbfz
   FROM v_window;

ALTER TABLE v_high_percentage
  OWNER TO anac;
