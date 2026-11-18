CREATE OR REPLACE VIEW v_window_departure AS 
 SELECT date_trunc('hour'::text, voo_r.partidaprevista) AS dia_hora,
    voo_r.mes,
    voo_r.ano_partida AS ano,
    voo_r.aeroportopartida AS aeroporto,
    count(*)::numeric AS voos,
    sum(
        CASE
            WHEN voo_r.tempoatrasopartida::text <> '0'::text OR voo_r.partidareal IS NULL THEN 1
            ELSE 0
        END)::numeric AS atraso
   FROM voo_r
  GROUP BY date_trunc('hour'::text, voo_r.partidaprevista), voo_r.mes, voo_r.ano_partida, voo_r.aeroportopartida;

ALTER TABLE v_window_departure
  OWNER TO anac;
