market_geo <- tibble::tribble(
                                 ~market,       ~lat,      ~lon,
                           "Aden, Yemen",   12.77944,  45.03667,
                "Barranquilla, Colombia",   10.96389, -74.79639,
                      "Berbera, Somalia",   10.43959,  45.01432,
                      "Blantyre, Malawi",  -15.78499,  35.00854,
                "Chittagong, Bangladesh",    22.3384,  91.83168,
                        "Cotonou, Benin",    6.36536,   2.41833,
               "Dar es Salaam, Tanzania",   -6.81258, 39.274749,
                    "Djibouti, Djibouti",   11.58901,  43.14503,
                      "Douala, Cameroon",    4.04827,   9.70428,
                  "Durban, South Africa", -29.849386,  31.03366,
              "Fort Dauphin, Madagascar",  -25.03249,  46.98329,
                        "Hodeida, Yemen",   14.79781,  42.95452,
                        "Lagos, Nigeria",   6.550449,  3.574901,
                            "Lomé, Togo",    6.13748,   1.21227,
                    "Maputo, Mozambique",  -25.96553,  32.58322,
  "Matadi, Democratic Republic of Congo",   -5.79949,  13.44068,
                    "Mogadishu, Somalia",    2.03711,  45.34375,
                        "Mombasa, Kenya",  -3.999937, 39.669073,
                "Nouakchott, Mauritania",   18.08581,  -15.9785,
         "Pointe Noire, CONGO-REPUB. OF",   -4.77609,  11.86352,
                   "Fishing Port, Yemen",  15.319515, 42.680825,
                     "Port Sudan, Sudan",   19.47966, 37.199371,
                 "Toamasina, Madagascar", -17.898344, 48.314231,
                   "Toliara, Madagascar", -22.660554, 44.238102
  )

orig_port <- tibble::tribble(
  ~port_name, ~orig_lat, ~orig_lon,
  "Houston, USA", 29.6111, -95.0217
)
