
recode_asi <- function(df) {
  
  # labels ----
  
  # geographic 
  
  state_labels <- c("Jammu & Kashmir" = 1, "Himachal Pradesh" = 2, "Punjab" = 3, 
                    "Chandigarh" = 4, "Uttarakhand" = 5, "Haryana" = 6, "Delhi" = 7,
                    "Rajasthan" = 8, "Uttar Pradesh" = 9, "Bihar" = 10, "Sikkim" = 11,
                    "Arunachal Pradesh" = 12, "Nagaland" = 13, "Manipur" = 14, 
                    "Mizoram" = 15, "Tripura" = 16, "Meghalaya" = 17, "Assam" = 18, 
                    "West Bengal" = 19, "Jharkhand" = 20, "Odisha" = 21, "Chhattisgarh" = 22, 
                    "Madhya Pradesh" = 23, "Gujarat" = 24, "Dadra & Nagar Haveli & Daman & Diu" = 25, 
                    "Maharashtra" = 27, "Andhra Pradesh" = 28, "Karnataka" = 29, 
                    "Goa" = 30, "Lakshadweep" = 31, "Kerala" = 32, "Tamil Nadu" = 33, 
                    "Puducherry" = 34, "Andaman & Nicobar Islands" = 35, "Telangana" = 36, "Ladakh" = 37)
  
  state_abv_labels <- c("JK" = 1, "HP" = 2, "PB" = 3, "CH" = 4, "UK" = 5, "HR" = 6, "DL" = 7,
                        "RJ" = 8, "UP" = 9, "BR" = 10, "SK" = 11, "AR" = 12, "NL" = 13, "MN" = 14,
                        "MZ" = 15, "TR" = 16, "ML" = 17, "AS" = 18, "WB" = 19, "JH" = 20, "OD" = 21,
                        "CG" = 22, "MP" = 23, "GJ" = 24, "DNDD" = 25, "MH" = 27, "AP" = 28,
                        "KA" = 29, "GA" = 30, "LD" = 31, "KL" = 32, "TN" = 33, "PY" = 34,
                        "AN" = 35, "TS" = 36, "LA" = 37)
  
  sector_labels <- c("Rural" = 1, "Urban" = 2)
  
  # enterprise 
  
  scheme_labels <- c("Census" = 1, "Sample" = 2)
  
  status_labels <- c("Open" = 1, 
                     "Existing with fixed assets and maintaining staff but not having production" = 2,
                     "Existing with fixed assets but not maintaining staff and not having production" = 3,
                     "Deleted" = 4,
                     "Existing but non-response due to closure and owner / occupier is not traceable" = 5,
                     "Non-response due to production not yet started or accounting year not closed during the year" = 7,
                     "Non-response due to other reasons" = 8)
  
  org_labels <- c("Individual Proprietorship" = 1,
                  "Partnership" = 2,
                  "Limited Liability Partnership" = 3,
                  "Government Company - Public" = 4,
                  "Government Company - Private" = 5,
                  "Non-Government Company - Public" = 6,
                  "Non-Government Company - Private" = 7,
                  "Co-operative Society" = 8,
                  "Others" = 9)
  
  employment_range_labels <- c("0 - 14" = 1, "15 - 19" = 2, "20 - 29" = 3, "30 - 49" = 4, 
                               "50 - 99" = 5, "100 - 199" = 6, "200 - 499" = 7, "500 - 999" = 8, 
                               "1000 - 1999" = 9, "2000 - 4999" = 10, "5000 & Above" = 11)
  
  capital_range_labels <- c("Upto 25" = 1, "25 - 500" = 2, "500 - 1000" = 3, "1000 & above" = 4)
  
  output_range_labels <- c("Upto 25" = 1, "25 - 50" = 2, "50 - 100" = 3, "100 - 200" = 4,
                           "200 - 500" = 5, "500 - 1000" = 6, "1000 - 2000" = 7, "2000 - 5000" = 8,
                           "5000 - 10000" = 9, "10000 - 50000" = 10, "50000 & above" = 11)
  
  nva_range_labels <- c("Upto 2.5" = 1, "2.5 - 5" = 2, "5 - 10" = 3, "10 - 20" = 4, "20 - 50" = 5,
                        "50 - 100" = 6, "100 - 200" = 7, "200 - 500" = 8, "500 - 1000" = 9,
                        "1000 - 5000" = 10, "5000 & above" = 11)
  
  # industries
  
  nic2008_group_labels <- c("Growing of non-perennial crops" = 11,
                            "Animal production" = 14,
                            "Support activities to agriculture and post-harvest crop activities" = 16,
                            "Silviculture and other forestry activities" = 21,
                            "Fishing" = 31,
                            "Mining of hard coal" = 51,
                            "Extraction of natural gas" = 62,
                            "Quarrying of stone, sand and clay" = 81,
                            "Mining and quarrying n.e.c." = 89,
                            "Support activities for petroleum and natural gas mining" = 91,
                            "Processing and preserving of meat" = 101,
                            "Processing and preserving of fish, crustaceans and molluscs" = 102,
                            "Processing and preserving of fruit and vegetables" = 103,
                            "Manufacture of vegetable and animal oils and fats" = 104,
                            "Manufacture of dairy products" = 105,
                            "Manufacture of grain mill products, starches and starch products" = 106,
                            "Manufacture of other food products" = 107,
                            "Manufacture of prepared animal feeds" = 108,
                            "Manufacture of beverages" = 110,
                            "Manufacture of tobacco products" = 120,
                            "Spinning, weaving and finishing of textiles" = 131,
                            "Manufacture of other textiles" = 139,
                            "Manufacture of wearing apparel, except fur apparel" = 141,
                            "Manufacture of articles of fur" = 142,
                            "Manufacture of knitted and crocheted apparel" = 143,
                            "Tanning and dressing of leather; manufacture of luggage, handbags, saddlery and harness; dressing and dyeing of fur" = 151,
                            "Manufacture of footwear" = 152,
                            "Sawmilling and planing of wood" = 161,
                            "Manufacture of products of wood, cork, straw and plaiting materials" = 162,
                            "Manufacture of paper and paper products" = 170,
                            "Printing and service activities related to printing" = 181,
                            "Reproduction of recorded media" = 182,
                            "Manufacture of coke oven products" = 191,
                            "Manufacture of refined petroleum products" = 192,
                            "Manufacture of basic chemicals, fertilizer and nitrogen compounds, plastics and synthetic rubber in primary forms" = 201,
                            "Manufacture of other chemical products" = 202,
                            "Manufacture of man-made fibres" = 203,
                            "Manufacture of pharmaceuticals, medicinal chemical and botanical products" = 210,
                            "Manufacture of rubber products" = 221,
                            "Manufacture of plastics products" = 222,
                            "Manufacture of glass and glass products" = 231,
                            "Manufacture of non-metallic mineral products n.e.c." = 239,
                            "Manufacture of basic iron and steel" = 241,
                            "Manufacture of basic precious and other non-ferrous metals" = 242,
                            "Casting of metals" = 243,
                            "Manufacture of structural metal products, tanks, reservoirs and steam generators" = 251,
                            "Manufacture of weapons and ammunition" = 252,
                            "Manufacture of other fabricated metal products; metalworking service activities" = 259,
                            "Manufacture of electronic components" = 261,
                            "Manufacture of computers and peripheral equipment" = 262,
                            "Manufacture of communication equipment" = 263,
                            "Manufacture of consumer electronics" = 264,
                            "Manufacture of measuring, testing, navigating and control equipment; watches and clocks" = 265,
                            "Manufacture of irradiation, electromedical and electrotherapeutic equipment" = 266,
                            "Manufacture of optical instruments and equipment" = 267,
                            "Manufacture of magnetic and optical media" = 268,
                            "Manufacture of electric motors, generators, transformers and electricity distribution and control apparatus" = 271,
                            "Manufacture of batteries and accumulators" = 272,
                            "Manufacture of wiring and wiring devices" = 273,
                            "Manufacture of electric lighting equipment" = 274,
                            "Manufacture of domestic appliances" = 275,
                            "Manufacture of other electrical equipment" = 279,
                            "Manufacture of general purpose machinery" = 281,
                            "Manufacture of special-purpose machinery" = 282,
                            "Manufacture of motor vehicles" = 291,
                            "Manufacture of bodies (coachwork) for motor vehicles; manufacture of trailers and semi-trailers" = 292,
                            "Manufacture of parts and accessories for motor vehicles" = 293,
                            "Building of ships and boats" = 301,
                            "Manufacture of railway locomotives and rolling stock" = 302,
                            "Manufacture of air and spacecraft and related machinery" = 303,
                            "Manufacture of military fighting vehicles" = 304,
                            "Manufacture of transport equipment n.e.c." = 309,
                            "Manufacture of furniture" = 310,
                            "Manufacture of jewellery, bijouterie and related articles" = 321,
                            "Manufacture of musical instruments" = 322,
                            "Manufacture of sports goods" = 323,
                            "Manufacture of games and toys" = 324,
                            "Manufacture of medical and dental instruments and supplies" = 325,
                            "Other manufacturing n.e.c." = 329,
                            "Repair of fabricated metal products, machinery and equipment" = 331,
                            "Installation of industrial machinery and equipment" = 332,
                            "Electric power generation, transmission and distribution" = 351,
                            "Manufacture of gas; distribution of gaseous fuels through mains" = 352,
                            "Steam and air conditioning supply" = 353,
                            "Water collection, treatment and supply" = 360,
                            "Sewerage" = 370,
                            "Waste collection" = 381,
                            "Waste treatment and disposal" = 382,
                            "Materials recovery" = 383,
                            "Remediation activities and other waste management services" = 390,
                            "Construction of buildings" = 410,
                            "Construction of roads and railways" = 421,
                            "Construction of other civil engineering projects" = 429,
                            "Electrical, plumbing and other construction installation activities" = 432,
                            "Building completion and finishing" = 433,
                            "Other specialized construction activities" = 439,
                            "Maintenance and repair of motor vehicles" = 452,
                            "Sale of motor vehicle parts and accessories" = 453,
                            "Sale, maintenance and repair of motorcycles and related parts and accessories" = 454,
                            "Wholesale on a fee or contract basis" = 461,
                            "Wholesale of agricultural raw materials and live animals" = 462,
                            "Wholesale of food, beverages and tobacco" = 463,
                            "Wholesale of household goods" = 464,
                            "Other specialized wholesale" = 466,
                            "Non-specialized wholesale trade" = 469,
                            "Retail sale of food, beverages and tobacco in specialized stores" = 472,
                            "Retail sale of automotive fuel in specialized stores" = 473,
                            "Retail sale of other household equipment in specialized stores" = 475,
                            "Retail sale of cultural and recreation goods in specialized stores" = 476,
                            "Other land transport" = 492,
                            "Transport via pipeline" = 493,
                            "Warehousing and storage" = 521,
                            "Support activities for transportation" = 522,
                            "Short term accommodation activities" = 551,
                            "Restaurants and mobile food service activities" = 561,
                            "Event catering and other food service activities" = 562,
                            "Publishing of books, periodicals and other publishing activities" = 581,
                            "Motion picture, video and television programme activities" = 591,
                            "Sound recording and music publishing activities" = 592,
                            "Wired telecommunications activities" = 611,
                            "Computer programming, consultancy and related activities" = 620,
                            "Real estate activities with own or leased property" = 681,
                            "Real estate activities on a fee or contract basis" = 682,
                            "Architectural and engineering activities and related technical consultancy" = 711,
                            "Technical testing and analysis" = 712,
                            "Research and experimental development on natural sciences and engineering" = 721,
                            "Specialized design activities" = 741,
                            "Photographic activities" = 742,
                            "Other professional, scientific and technical activities n.e.c." = 749,
                            "Renting and leasing of other machinery, equipment and tangible goods n.e.c." = 773,
                            "Office administrative and support activities" = 821,
                            "Business support service activities n.e.c." = 829,
                            "Repair of computers and communication equipment" = 951,
                            "Repair of personal and household goods" = 952,
                            "Other personal service activities" = 960)
  
  nic2008_division_labels <- c("Crop and animal production, hunting and related service activities" = 1,
                               "Forestry and logging" = 2,
                               "Fishing and aquaculture" = 3,
                               "Mining of coal and lignite" = 5,
                               "Extraction of crude petroleum and natural gas" = 6,
                               "Other mining and quarrying" = 8,
                               "Mining support service activities" = 9,
                               "Manufacture of food products" = 10,
                               "Manufacture of beverages" = 11,
                               "Manufacture of tobacco products" = 12,
                               "Manufacture of textiles" = 13,
                               "Manufacture of wearing apparel" = 14,
                               "Manufacture of leather and related products" = 15,
                               "Manufacture of wood and products of wood and cork, except furniture" = 16,
                               "Manufacture of paper and paper products" = 17,
                               "Printing and reproduction of recorded media" = 18,
                               "Manufacture of coke and refined petroleum products" = 19,
                               "Manufacture of chemicals and chemical products" = 20,
                               "Manufacture of pharmaceuticals, medicinal chemical and botanical products" = 21,
                               "Manufacture of rubber and plastics products" = 22,
                               "Manufacture of other non-metallic mineral products" = 23,
                               "Manufacture of basic metals" = 24,
                               "Manufacture of fabricated metal products, except machinery and equipment" = 25,
                               "Manufacture of computer, electronic and optical products" = 26,
                               "Manufacture of electrical equipment" = 27,
                               "Manufacture of machinery and equipment n.e.c." = 28,
                               "Manufacture of motor vehicles, trailers and semi-trailers" = 29,
                               "Manufacture of other transport equipment" = 30,
                               "Manufacture of furniture" = 31,
                               "Other manufacturing" = 32,
                               "Repair and installation of machinery and equipment" = 33,
                               "Electricity, gas, steam and air conditioning supply" = 35,
                               "Water collection, treatment and supply" = 36,
                               "Sewerage" = 37,
                               "Waste collection, treatment and disposal activities; materials recovery" = 38,
                               "Remediation activities and other waste management services" = 39,
                               "Construction of buildings" = 41,
                               "Civil engineering" = 42,
                               "Specialized construction activities" = 43,
                               "Wholesale/retail trade and repair of motor vehicles and motorcycles" = 45,
                               "Wholesale trade, except of motor vehicles and motorcycles" = 46,
                               "Retail trade, except of motor vehicles and motorcycles" = 47,
                               "Land transport and transport via pipelines" = 49,
                               "Warehousing and support activities for transportation" = 52,
                               "Accommodation" = 55,
                               "Food and beverage service activities" = 56,
                               "Publishing activities" = 58,
                               "Motion picture, video, TV programme, sound recording activities" = 59,
                               "Telecommunications" = 61,
                               "Computer programming, consultancy and related activities" = 62,
                               "Real estate activities" = 68,
                               "Architecture and engineering activities; technical testing and analysis" = 71,
                               "Scientific research and development" = 72,
                               "Other professional, scientific and technical activities" = 74,
                               "Rental and leasing activities" = 77,
                               "Office administrative, office support and other business support activities" = 82,
                               "Repair of computers and personal and household goods" = 95,
                               "Other personal service activities" = 96)
  
  # Note on tabulation formula consistency across waves -----
  
  # old_waves <- c("2015_16", "2016_17", "2017_18")
  
  # From ASI 2018-19 onward, the survey redefined Rent Paid, Rent Received, Output, Input, NFCF, GFCF, GCF, Net Income, and Net Profit to align with SNA 2008 (see ASI 2018-19 Vol I, Section 1.9). 
  # For comparability across the full panel (2015-16 to 2023-24), this script applies the POST-2018-19 (revised) formula definitions uniformly to all waves, including 2015-16 through 2017-18.
  
  # As a result, the following derived variables for the 2015-16 to 2017-18 waves will not match the officially published ASI reports (which used the pre-revision definitions): 
  # rent_paid, rent_received, other_output, other_input, net_fixed_capital_formation 
  # And everything computed from them: total_output, total_input, total_gva, total_nva, net_income, net_profit, gross_fixed_capital_formation, gross_capital_formation
  
  # All other derived variables (fixed_capital, physical_working_capital, working_capital, invested_capital, gross_fixed_capital_addition, outstanding_loan, interest_paid, 
  # interest_received, gross_value_plant_machinery, value_products_byproduct, fuels_consumed, materials_consumed, total_depreciation, all stock_addition_* variables,
  # all employment variables, all wage variables, and the fuel breakdown) use an identical construction across all waves and remain directly comparable to the official PRE-2018-19 published figures.
  
  # Note: 2018-19 aggregates show a small residual gap compared to the officially published totals even though the formulas here match NSO's own revised computation.
  
  # mutating ----
  
  df <- df %>% 
    mutate(
      
      # geographic characteristics ----
      
      state = case_when(state == 26 ~ 25, T ~ state),
      state = labelled(state, labels = state_labels),
      state_abv = labelled(state, labels = state_abv_labels),
      
      sector = labelled(sector, labels = sector_labels),
      
      # enterprise details ----
      
      scheme_code = labelled(scheme_code, labels = scheme_labels),
      
      unit_status = labelled(unit_status, labels = status_labels),
      
      org_type = labelled(org_type, labels = org_labels),
      
      # industry ----
      
      nic2008_4digit = nic2008_5digit %/% 10, # the existing nic2008_4digit is 9999 for some reason in the original data
      
      nic2008_3digit = nic2008_5digit %/% 100,
      nic2008_group = labelled(nic2008_3digit, labels = nic2008_group_labels),
      
      nic2008_2digit = nic2008_5digit %/% 1000,
      nic2008_division = labelled(nic2008_2digit, labels = nic2008_division_labels),
      
      # capital & finance ----
      
      fixed_capital = rowSums(cbind(net_value_closing_rs_01, net_value_closing_rs_02, net_value_closing_rs_03, net_value_closing_rs_04, 
                                    net_value_closing_rs_05, net_value_closing_rs_06, net_value_closing_rs_07, net_value_closing_rs_09), na.rm = T),
      
      physical_working_capital = rowSums(cbind(closing_value_rs_01, closing_value_rs_02, closing_value_rs_03, closing_value_rs_05, closing_value_rs_06), na.rm = T),
      working_capital = rowSums(cbind(physical_working_capital, closing_value_rs_08, closing_value_rs_09, closing_value_rs_10), na.rm = T) - rowSums(cbind(closing_value_rs_12, closing_value_rs_13, closing_value_rs_14), na.rm = T),
      invested_capital = rowSums(cbind(fixed_capital, physical_working_capital), na.rm = T),
      
      gross_fixed_capital_addition = rowSums(cbind(gross_value_actual_addition_rs_01, gross_value_actual_addition_rs_02, gross_value_actual_addition_rs_03, gross_value_actual_addition_rs_04, 
                                                   gross_value_actual_addition_rs_05, gross_value_actual_addition_rs_06, gross_value_actual_addition_rs_07, gross_value_actual_addition_rs_09), na.rm = T),
      
      rent_paid = land_rent_royalties_rs,
      
      # rent_paid = if_else(wave %in% old_waves, rowSums(cbind(building_rent_rs, land_rent_royalties_rs), na.rm = T), land_rent_royalties_rs), # this uses old formula for PRE-2018-19 waves and new for later waves
      
      outstanding_loan = closing_value_rs_17,
      interest_paid = interest_paid_rs,
      
      rent_received = land_rent_royalties_received_rs,
      
      # rent_received = if_else(wave %in% old_waves, rowSums(cbind(building_rent_received_rs, land_rent_royalties_received_rs), na.rm = T), land_rent_royalties_received_rs), # this uses old formula for PRE-2018-19 waves and new for later waves
      
      interest_received = interest_received_rs,
      
      gross_value_plant_machinery = gross_value_closing_rs_03,
      
      # output ----
      
      value_products_byproduct = rowSums(pick(starts_with("ex_factory_value_rs_")), na.rm = T) + rowSums(cbind(own_construction_value_rs, semi_finished_goods_stock_change_rs), na.rm = T),
      
      other_output = rowSums(cbind(manufacturing_services_receipts_rs, non_manufacturing_services_receipts_rs, electricity_generated_sold_value_rs,
                                   plant_machinery_rent_received_rs, building_rent_received_rs, goods_resale_value_rs, rd_expenses_rs), na.rm = T),
      
      # other_output = if_else(wave %in% old_waves,
      #                        rowSums(cbind(manufacturing_services_receipts_rs, non_manufacturing_services_receipts_rs,
      #                                      electricity_generated_sold_value_rs, plant_machinery_rent_received_rs,
      #                                      goods_resale_value_rs), na.rm = T),                      # no building_rent_received_rs, no rd_expenses_rs
      #                        rowSums(cbind(manufacturing_services_receipts_rs, non_manufacturing_services_receipts_rs,
      #                                      electricity_generated_sold_value_rs, plant_machinery_rent_received_rs,
      #                                      building_rent_received_rs, goods_resale_value_rs, rd_expenses_rs), na.rm = T)), # this uses old formula for PRE-2018-19 waves and new for later waves
      
      total_output = rowSums(cbind(value_products_byproduct, other_output), na.rm = T),
      
      # input ----
      
      fuels_consumed = rowSums(cbind(indigenous_input_purchase_value_rs_016, indigenous_input_purchase_value_rs_017, indigenous_input_purchase_value_rs_018, 
                                     indigenous_input_purchase_value_rs_019, indigenous_input_purchase_value_rs_020), na.rm = T),
      
      materials_consumed = rowSums(across(any_of(paste0("indigenous_input_purchase_value_rs_", sprintf("%03d", c(1:11, 13, 14, 21))))), na.rm = T) + 
        rowSums(pick(matches("^indigenous_input_purchase_value_rs_(02[5-9]|0[3-9]\\d|[1-9]\\d{2})$")), na.rm = T) +
        rowSums(pick(starts_with("imported_input_purchase_value_rs_")), na.rm = T),
      
      other_input = rowSums(cbind(work_done_by_others_rs, building_repair_maintenance_rs, fixed_assets_repair_maintenance_rs, operating_expenses_rs,
                                  own_construction_materials_rs, plant_machinery_rent_rs, rd_expenses_rs, building_rent_rs, goods_purchased_for_resale_rs), na.rm = T),
      
      # other_input = if_else(wave %in% old_waves,
      #                       rowSums(cbind(work_done_by_others_rs, building_repair_maintenance_rs, fixed_assets_repair_maintenance_rs,
      #                                     operating_expenses_rs, own_construction_materials_rs, plant_machinery_rent_rs,
      #                                     rd_expenses_rs, goods_purchased_for_resale_rs), na.rm = T),   # no building_rent_rs
      #                       rowSums(cbind(work_done_by_others_rs, building_repair_maintenance_rs, fixed_assets_repair_maintenance_rs,
      #                                     operating_expenses_rs, own_construction_materials_rs, plant_machinery_rent_rs,
      #                                     rd_expenses_rs, building_rent_rs, goods_purchased_for_resale_rs), na.rm = T)), # this uses old formula for PRE-2018-19 waves and new for later waves
      
      total_input = rowSums(cbind(fuels_consumed, materials_consumed, other_input), na.rm = T),
      
      # value added, income & profit ----
      
      total_gva = total_output - total_input,
      
      total_depreciation = rowSums(cbind(depreciation_current_year_rs_01, depreciation_current_year_rs_02, depreciation_current_year_rs_03, depreciation_current_year_rs_04, 
                                         depreciation_current_year_rs_05, depreciation_current_year_rs_06, depreciation_current_year_rs_07, depreciation_current_year_rs_09), na.rm = T),
      
      total_nva = total_gva - total_depreciation,
      
      net_income = total_nva - rowSums(cbind(rent_paid, interest_paid), na.rm = T),
      
      # net_income = if_else(wave %in% old_waves,
      #                      total_nva - rowSums(cbind(rent_paid, interest_paid), na.rm = T) + rowSums(cbind(rent_received, interest_received), na.rm = T),
      #                      total_nva - rowSums(cbind(rent_paid, interest_paid), na.rm = T)), # this uses old formula for PRE-2018-19 waves and new for later waves
      
      net_profit = net_income - rowSums(cbind(wages_salaries_rs_01, wages_salaries_rs_02, wages_salaries_rs_03, wages_salaries_rs_05, 
                                              wages_salaries_rs_07, wages_salaries_rs_08, bonus_rs, provident_fund_rs, welfare_rs), na.rm = T),
      
      # fixed capital formation & stocks ----
      
      net_fixed_capital_formation = rowSums(cbind(net_value_closing_rs_01, net_value_closing_rs_02, net_value_closing_rs_03, net_value_closing_rs_04,
                                                  net_value_closing_rs_05, net_value_closing_rs_06, net_value_closing_rs_07, net_value_closing_rs_09), na.rm = T) -
        rowSums(cbind(net_value_opening_rs_01, net_value_opening_rs_02, net_value_opening_rs_03, net_value_opening_rs_04,
                      net_value_opening_rs_05, net_value_opening_rs_06, net_value_opening_rs_07, net_value_opening_rs_09), na.rm = T) -
        rowSums(cbind(gross_value_revaluation_addition_rs_01, gross_value_revaluation_addition_rs_02, gross_value_revaluation_addition_rs_03,
                      gross_value_revaluation_addition_rs_04, gross_value_revaluation_addition_rs_05, gross_value_revaluation_addition_rs_06,
                      gross_value_revaluation_addition_rs_07, gross_value_revaluation_addition_rs_09), na.rm = T) +
        rowSums(cbind(rd_expenses_rs), na.rm = T),
      
      # net_fixed_capital_formation = rowSums(cbind(net_value_closing_rs_01, net_value_closing_rs_02, net_value_closing_rs_03, net_value_closing_rs_04,
      #                                             net_value_closing_rs_05, net_value_closing_rs_06, net_value_closing_rs_07, net_value_closing_rs_09), na.rm = T) -
      #   rowSums(cbind(net_value_opening_rs_01, net_value_opening_rs_02, net_value_opening_rs_03, net_value_opening_rs_04,
      #                 net_value_opening_rs_05, net_value_opening_rs_06, net_value_opening_rs_07, net_value_opening_rs_09), na.rm = T) -
      #   rowSums(cbind(gross_value_revaluation_addition_rs_01, gross_value_revaluation_addition_rs_02, gross_value_revaluation_addition_rs_03,
      #                 gross_value_revaluation_addition_rs_04, gross_value_revaluation_addition_rs_05, gross_value_revaluation_addition_rs_06,
      #                 gross_value_revaluation_addition_rs_07, gross_value_revaluation_addition_rs_09), na.rm = T) +
      #   if_else(wave %in% old_waves, 0, coalesce(rd_expenses_rs, 0)), # this uses old formula for PRE-2018-19 waves and new for later waves
      
      gross_fixed_capital_formation = rowSums(cbind(net_fixed_capital_formation, total_depreciation), na.rm = T),
      
      stock_addition_materials_fuels = rowSums(cbind(closing_value_rs_01, closing_value_rs_02, closing_value_rs_03), na.rm = T) - rowSums(cbind(opening_value_rs_01, opening_value_rs_02, opening_value_rs_03), na.rm = T),
      
      stock_addition_semifinished = rowSums(cbind(closing_value_rs_05), na.rm = T) - rowSums(cbind(opening_value_rs_05), na.rm = T),
      stock_addition_finished_goods = rowSums(cbind(closing_value_rs_06), na.rm = T) - rowSums(cbind(opening_value_rs_06), na.rm = T),
      
      stock_addition_total = rowSums(cbind(stock_addition_materials_fuels, stock_addition_semifinished, stock_addition_finished_goods), na.rm = T),
      
      gross_capital_formation = rowSums(cbind(gross_fixed_capital_formation, stock_addition_total), na.rm = T),
      
      # employment ----
      
      total_male_workers = avg_persons_worked_01, # 1.1.1 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      total_female_workers = avg_persons_worked_02, # 1.1.2 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      total_trans_workers = avg_persons_worked_03, # 1.1.3 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      
      total_direct_workers = rowSums(cbind(total_male_workers, total_female_workers, total_trans_workers), na.rm = T), # 1.1 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      total_contract_workers = avg_persons_worked_05, # 1.2 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      total_workers = rowSums(cbind(total_direct_workers, total_contract_workers), na.rm = T), # 1 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      
      total_staff_supervisory = avg_persons_worked_07, # 2.1 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      total_employees_other = avg_persons_worked_08, # 2.2 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      total_nonworker_employees = rowSums(cbind(total_staff_supervisory, total_employees_other), na.rm = T), # 2 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      
      total_unpaid_family = avg_persons_worked_09, # 3 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under A
      
      total_persons_engaged = rowSums(cbind(total_workers, total_nonworker_employees, total_unpaid_family), na.rm = T), # A in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24
      
      total_mandays_employed = rowSums(cbind(total_mandays_01, total_mandays_02, total_mandays_03, total_mandays_05, 
                                             total_mandays_07, total_mandays_08), na.rm = T), # B in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24
      
      # wages ----
      
      wages_male_workers = wages_salaries_rs_01,
      wages_female_workers = wages_salaries_rs_02,
      wages_trans_workers = wages_salaries_rs_03,
      
      wages_direct_workers = rowSums(cbind(wages_male_workers, wages_female_workers, wages_trans_workers), na.rm = T),
      wages_contract_workers = wages_salaries_rs_05,
      wages_workers = rowSums(cbind(wages_direct_workers, wages_contract_workers), na.rm = T), # 1.1.1 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under C
      
      wages_staff_supervisory = wages_salaries_rs_07, # 1.1.2 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under C
      wages_other_employees = wages_salaries_rs_08, # 1.1.3 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under C
      wages_nonworker_employees = rowSums(cbind(wages_staff_supervisory, wages_other_employees), na.rm = T),
      
      total_wages_salaries = rowSums(cbind(wages_workers, wages_nonworker_employees), na.rm = T), # 1.1 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under C
      
      total_bonus = bonus_rs, # 1.2 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under C
      wages_salaries_bonus = rowSums(cbind(total_wages_salaries, total_bonus), na.rm = T), # 1 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under C & total_emoluments in summary report
      
      employer_contribution = rowSums(cbind(provident_fund_rs, welfare_rs), na.rm = T), # 2 in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24 under C
      wage_salaries_contribution = rowSums(cbind(wages_salaries_bonus, employer_contribution), na.rm = T), # C in tabulation programme - Volume – I (Tables – 3 & 4), 2023-24
      
      # fuels consumed ----
      
      coal_consumed = indigenous_input_purchase_value_rs_018,
      electricity_purchased = indigenous_input_purchase_value_rs_016,
      petroleum_products = indigenous_input_purchase_value_rs_017,
      gas_consumed = indigenous_input_purchase_value_rs_019,
      other_fuels = indigenous_input_purchase_value_rs_020,
      
      # categorical ranges ----
      
      employment_range = case_when(total_persons_engaged < 15 ~ 1,
                                   total_persons_engaged < 20 ~ 2,
                                   total_persons_engaged < 30 ~ 3,
                                   total_persons_engaged < 50 ~ 4,
                                   total_persons_engaged < 100 ~ 5,
                                   total_persons_engaged < 200 ~ 6,
                                   total_persons_engaged < 500 ~ 7,
                                   total_persons_engaged < 1000 ~ 8,
                                   total_persons_engaged < 2000 ~ 9,
                                   total_persons_engaged < 5000 ~ 10,
                                   total_persons_engaged >= 5000 ~ 11,
                                   T ~ NA),
      employment_range = labelled(employment_range, labels = employment_range_labels),
      
      capital_range = case_when(coalesce(gross_value_plant_machinery, 0)/1e5 <= 25 ~ 1,
                                coalesce(gross_value_plant_machinery, 0)/1e5 <= 500 ~ 2,
                                coalesce(gross_value_plant_machinery, 0)/1e5 <= 1000 ~ 3,
                                coalesce(gross_value_plant_machinery, 0)/1e5 > 1000 ~ 4,
                                T ~ NA),
      capital_range = labelled(capital_range, labels = capital_range_labels),
      
      output_range = case_when(total_output/1e5 <= 25 ~ 1,
                               total_output/1e5 <= 50 ~ 2,
                               total_output/1e5 <= 100 ~ 3,
                               total_output/1e5 <= 200 ~ 4,
                               total_output/1e5 <= 500 ~ 5,
                               total_output/1e5 <= 1000 ~ 6,
                               total_output/1e5 <= 2000 ~ 7,
                               total_output/1e5 <= 5000 ~ 8,
                               total_output/1e5 <= 10000 ~ 9,
                               total_output/1e5 <= 50000 ~ 10,
                               total_output/1e5 > 50000 ~ 11,
                               T ~ NA),
      output_range = labelled(output_range, labels = output_range_labels),
      
      nva_range = case_when(total_nva/1e5 <= 2.5 ~ 1,
                            total_nva/1e5 <= 5 ~ 2,
                            total_nva/1e5 <= 10 ~ 3,
                            total_nva/1e5 <= 20 ~ 4,
                            total_nva/1e5 <= 50 ~ 5,
                            total_nva/1e5 <= 100 ~ 6,
                            total_nva/1e5 <= 200 ~ 7,
                            total_nva/1e5 <= 500 ~ 8,
                            total_nva/1e5 <= 1000 ~ 9,
                            total_nva/1e5 <= 5000 ~ 10,
                            total_nva/1e5 > 5000 ~ 11,
                            T ~ NA),
      nva_range = labelled(nva_range, labels = nva_range_labels)
      
    )
  
  derived_vars <- c("fixed_capital", "physical_working_capital", "working_capital", "invested_capital", "gross_fixed_capital_addition", "rent_paid", "outstanding_loan", "interest_paid", "rent_received",
                    "interest_received", "gross_value_plant_machinery", "value_products_byproduct", "other_output", "total_output", "fuels_consumed", "materials_consumed", "other_input", "total_input", 
                    "total_gva", "total_depreciation", "total_nva", "net_income", "net_profit", "net_fixed_capital_formation", "gross_fixed_capital_formation", "stock_addition_materials_fuels", 
                    "stock_addition_semifinished", "stock_addition_finished_goods", "stock_addition_total", "gross_capital_formation", "total_male_workers", "total_female_workers", "total_trans_workers",
                    "total_direct_workers", "total_contract_workers", "total_workers", "total_staff_supervisory", "total_employees_other", "total_nonworker_employees", "total_unpaid_family", 
                    "total_persons_engaged", "total_mandays_employed", "wages_male_workers", "wages_female_workers", "wages_trans_workers", "wages_direct_workers", "wages_contract_workers", "wages_workers", 
                    "wages_staff_supervisory", "wages_other_employees", "wages_nonworker_employees", "total_wages_salaries", "total_bonus", "wages_salaries_bonus", "employer_contribution", 
                    "wage_salaries_contribution", "coal_consumed", "electricity_purchased", "petroleum_products", "gas_consumed", "other_fuels", "employment_range", "capital_range", "output_range", "nva_range")
  
  df <- df %>% 
    mutate(across(all_of(derived_vars), ~ if_else(unit_status %in% c(1:3), ., NA))) %>% 
    relocate(mult, .after = last_col())
  
  return(df)
  
}
