CREATE OR REPLACE VIEW v_travel_day AS 
 SELECT v.aeroporto,
    avg(v.sum_travel_day)::integer AS avg_travel_day
   FROM ( SELECT teste.day,
            teste.aeroporto,
            count(*) AS sum_travel_day
           FROM ( SELECT date_trunc('day'::text, voo_r.partidaprevista) AS day,
                    voo_r.aeroportopartida AS aeroporto
                   FROM voo_r
                  GROUP BY voo_r.partidaprevista, voo_r.aeroportopartida
                  ORDER BY voo_r.partidaprevista, voo_r.aeroportopartida) teste
          GROUP BY teste.day, teste.aeroporto
          ORDER BY teste.day, teste.aeroporto) v
  GROUP BY v.aeroporto
  ORDER BY v.aeroporto;

ALTER TABLE v_travel_day
  OWNER TO anac;
