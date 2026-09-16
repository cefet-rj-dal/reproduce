CREATE OR REPLACE VIEW v_travel_time AS 
 SELECT voo_r.aeroportopartida,
    voo_r.aeroportochegada,
    avg(voo_r.chegadaprevista - voo_r.partidaprevista) AS avg_travel_time
   FROM voo_r
  GROUP BY voo_r.aeroportopartida, voo_r.aeroportochegada
  ORDER BY voo_r.aeroportopartida;

ALTER TABLE v_travel_time
  OWNER TO anac;
