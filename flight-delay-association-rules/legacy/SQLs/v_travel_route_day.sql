CREATE OR REPLACE VIEW v_travel_route_day AS 
 SELECT s.aeroportopartida,
    s.aeroportochegada,
    avg(s.total_voo_dia)::integer AS avg_voo_dia
   FROM ( SELECT r.data,
            r.aeroportopartida,
            r.aeroportochegada,
            count(r.aeroportochegada) AS total_voo_dia
           FROM ( SELECT date_trunc('day'::text, voo_r.partidaprevista) AS data,
                    voo_r.aeroportopartida,
                    voo_r.aeroportochegada
                   FROM voo_r
                  GROUP BY voo_r.partidaprevista, voo_r.aeroportopartida, voo_r.aeroportochegada
                  ORDER BY voo_r.partidaprevista, voo_r.aeroportopartida) r
          GROUP BY r.data, r.aeroportopartida, r.aeroportochegada
          ORDER BY r.aeroportopartida) s
  GROUP BY s.aeroportopartida, s.aeroportochegada
  ORDER BY s.aeroportopartida;

ALTER TABLE v_travel_route_day
  OWNER TO anac;
