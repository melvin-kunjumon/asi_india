
standardise_asi <- function(df, name) {
  
  df <- as_tibble(df)
  
  # capturing wave and levels ----
  
  wave <- sub(".*?(\\d{4}(?:_\\d{2})?).*", "\\1", name)
  block <- sub(".*_(block[A-J])$", "\\1", name)
  
  # standardising column names ----
  
  ## defining column names ----
  
  col_dict <- list(
    
    blockA = c("year", "block_code", "dsl_code", "psl_code", "scheme_code", "nic2008_4digit", "nic2008_5digit", 
               "state", "district", "sector", "sro_code", "n_units", "unit_status", "bonus_rs", "provident_fund_rs", 
               "welfare_rs", "manufacturing_working_days", "non_manufacturing_working_days", "total_working_days", 
               "total_production_cost_rs", "export_share_pct", "mult"), 
    
    blockB = c("year", "block_code", "dsl_code", "org_type", "corporate_id", "has_iso", "initial_production_year", 
               "accounting_year_from", "accounting_year_to", "months_operated", "has_foreign_shareholding", 
               "has_rd_unit", "has_formal_training"), 
    
    blockC = c("year", "block_code", "dsl_code", "serial_no", "gross_value_opening_rs", "gross_value_revaluation_addition_rs", 
               "gross_value_actual_addition_rs", "gross_value_deduction_adjustment_rs", "gross_value_closing_rs", "depreciation_beginning_rs", 
               "depreciation_current_year_rs", "depreciation_adjustment_rs", "depreciation_end_rs", "net_value_opening_rs", 
               "net_value_closing_rs"), 
    
    blockD = c("year", "block_code", "dsl_code", "serial_no", "opening_value_rs", "closing_value_rs"),
    
    blockE = c("year", "block_code", "dsl_code", "serial_no", "manufacturing_mandays", "non_manufacturing_mandays", 
               "total_mandays", "avg_persons_worked", "mandays_paid", "wages_salaries_rs"),
    
    blockF = c("year", "block_code", "dsl_code", "work_done_by_others_rs", "building_repair_maintenance_rs", 
               "fixed_assets_repair_maintenance_rs", "operating_expenses_rs", "own_construction_materials_rs", 
               "insurance_charges_rs", "plant_machinery_rent_rs", "rd_expenses_rs", "building_rent_rs", 
               "land_rent_royalties_rs", "interest_paid_rs", "goods_purchased_for_resale_rs", 
               "inward_transport_cost_rs", "outward_transport_cost_rs"), 
    
    blockG = c("year", "block_code", "dsl_code", "manufacturing_services_receipts_rs", 
               "non_manufacturing_services_receipts_rs", "electricity_generated_sold_value_rs", 
               "own_construction_value_rs", "net_goods_resale_balance_rs", "plant_machinery_rent_received_rs", 
               "semi_finished_goods_stock_change_rs", "building_rent_received_rs", 
               "land_rent_royalties_received_rs", "interest_received_rs", "goods_resale_value_rs", 
               "other_production_subsidies_rs"),
    
    blockH = c("year", "block_code", "dsl_code", "serial_no", "item_code", "indigenous_input_quantity_unit_code", "indigenous_input_quantity_consumed", 
               "indigenous_input_purchase_value_rs", "indigenous_input_rate_per_unit_rs"),
    
    blockI = c("year", "block_code", "dsl_code", "serial_no", "item_code", "imported_input_quantity_unit_code", "imported_input_quantity_consumed", 
               "imported_input_purchase_value_rs", "imported_input_rate_per_unit_rs"), 
    
    blockJ = c("year", "block_code", "dsl_code", "serial_no", "item_code", "quantity_unit_code", "quantity_manufactured", 
               "quantity_sold", "gross_sale_value_rs", "gst_rs", "other_indirect_taxes_rs", "other_deductions_rs", 
               "subsidy_rs", "net_sale_value_per_unit_rs", "ex_factory_value_rs")
    
  )
  
  ## handling missing columns ----
  
  # Before 2020-21 Block B did not have data on whether the enterprise offers formal training
  
  if (block == "blockB" && as.integer(substr(wave, 1, 4)) < 2020) {
    
    df[[ncol(df) + 1]] <-  NA
    
  }
  
  # Before 2019-20 Block F did not have data on inward and outward transportation costs
  
  if (block == "blockF" && as.integer(substr(wave, 1, 4)) < 2019) {
    
    df[[ncol(df) + 1]] <-  NA
    df[[ncol(df) + 1]] <-  NA
    
  }
  
  ## assigning block-specific column names ----
  
  dict <- col_dict[[block]]
  
  if (!is.null(dict) & ncol(df) == length(dict)) {
    
    names(df) <- dict
    
  }
  
  # mutating -----
  
  ## cleaning ----
  
  df <- df %>%
    mutate(across(everything(.), ~ remove_val_labels(.)),
           across(everything(.), ~ remove_var_label(.)),
           across(where(is.character), ~ na_if(., "")),
           across(where(~ all(is.na(.) | grepl("^\\d+$", .))), ~ as.numeric(.)),
           wave = wave) %>% 
    arrange(dsl_code)
  
  ## handling missing values ----
  
  # Before 2023-24 Block E did not have data on transgender workers
  
  if (block == "blockE" && as.integer(substr(wave, 1, 4)) < 2023) {
    
    df <- df %>% 
      mutate(serial_no = case_when(serial_no == 1 ~ 1, # male workers
                                   serial_no == 2 ~ 2, # female workers
                                   serial_no == 3 ~ 4, # total direct workers
                                   serial_no == 4 ~ 5, # contract workers
                                   serial_no == 5 ~ 6, # total workers
                                   serial_no == 6 ~ 7, # supervisory & managerial staff
                                   serial_no == 7 ~ 8, # other employees
                                   serial_no == 8 ~ 9, # unpaid family members/proprietor/coop.members
                                   serial_no == 9 ~ 10)) # total employees
    
    # we have to create dummy rows with NA for them to exist as columns after transforming the data.frame to wide format
    
    dummy <- df %>%
      distinct(dsl_code) %>%
      mutate(serial_no = 3) # transgender workers
    
    df <- bind_rows(df, dummy)
    
  }
  
  # confining the data to required rows & columns ----
  
  # these are the rows and columns used in the estimations found in Volume - I & Summary Report
  
  ## filtering required rows ----
  
  tab_rows <- list(
    
    blockC = c(1:7, 9),
    
    blockD = c(1, 2, 3, 5, 6, 8, 9, 10, 12, 13, 14, 17),
    
    blockE = c(1, 2, 3, 5, 7, 8, 9),
    
    blockH = c(1:11, 13, 14, 16:21), # need to include all the serial_no > 24
    
    blockI = c(1:6), # need to include all the serial_no > 7
    
    blockJ = c(1:11) # need to include all the serial_no > 13
    
  )
  
  tab_rows_min_extra <- list(blockH = 24, blockI = 7, blockJ = 13)
  
  if (block %in% names(tab_rows)) {
    
    if (block %in% names(tab_rows_min_extra)) {
      
      df <- df %>% 
        filter(serial_no %in% tab_rows[[block]] | serial_no > tab_rows_min_extra[[block]])
      
    } else {
      
      df <- df %>% 
        filter(serial_no %in% tab_rows[[block]])
      
    }
    
  }
  
  ## selecting required columns ----
  
  tab_cols <- list(
    
    blockA = c("wave", "dsl_code", "psl_code", "scheme_code", "nic2008_4digit", "nic2008_5digit", 
               "state", "district", "sector", "sro_code", "n_units", "unit_status", "bonus_rs", "provident_fund_rs", 
               "welfare_rs", "manufacturing_working_days", "non_manufacturing_working_days", "total_working_days", 
               "total_production_cost_rs", "export_share_pct", "mult"),
    
    blockB = c("dsl_code", "org_type", "corporate_id", "has_iso", "initial_production_year", 
               "accounting_year_from", "accounting_year_to", "months_operated", "has_foreign_shareholding", 
               "has_rd_unit", "has_formal_training"),
    
    blockC = c("dsl_code", "serial_no", "gross_value_revaluation_addition_rs", "gross_value_actual_addition_rs",
               "gross_value_closing_rs", "depreciation_current_year_rs", "net_value_opening_rs", "net_value_closing_rs"),
    
    blockD = c("dsl_code", "serial_no", "opening_value_rs", "closing_value_rs"),
    
    blockE = c("dsl_code", "serial_no", "total_mandays", "avg_persons_worked", "wages_salaries_rs"),
    
    blockF = c("dsl_code", "plant_machinery_rent_rs", "rd_expenses_rs", "building_rent_rs",
               "land_rent_royalties_rs", "interest_paid_rs", "work_done_by_others_rs",
               "building_repair_maintenance_rs", "fixed_assets_repair_maintenance_rs",
               "operating_expenses_rs", "own_construction_materials_rs",
               "goods_purchased_for_resale_rs"),
    
    blockG = c("dsl_code", "manufacturing_services_receipts_rs", "non_manufacturing_services_receipts_rs",
               "electricity_generated_sold_value_rs", "own_construction_value_rs",
               "plant_machinery_rent_received_rs", "semi_finished_goods_stock_change_rs",
               "building_rent_received_rs", "land_rent_royalties_received_rs",
               "interest_received_rs", "goods_resale_value_rs"),
    
    blockH = c("dsl_code", "serial_no", "indigenous_input_purchase_value_rs"),
    
    blockI = c("dsl_code", "serial_no", "imported_input_purchase_value_rs"),
    
    blockJ = c("dsl_code", "serial_no", "ex_factory_value_rs")
    
  )
  
  if (block %in% names(tab_cols)) {
    
    df <- df %>% 
      select(any_of(tab_cols[[block]]))
    
  }
  
  # pivoting data.frames based on the presence of serial_no ----
  
  new_dict <- names(df)
  
  if ("serial_no" %in% new_dict) {
    
    val_cols <- setdiff(new_dict, c("dsl_code", "serial_no"))
    
    digit_width <- if (block == "blockH") 3 else 2
    
    df <- df %>%
      arrange(serial_no) %>%
      pivot_wider(names_from = serial_no,
                  values_from = all_of(val_cols),
                  values_fill = 0,
                  names_glue = paste0("{.value}_{sprintf('%0", digit_width, "d', as.integer(serial_no))}")) %>%
      arrange(dsl_code)
  }
  
  return(df)
  
}
