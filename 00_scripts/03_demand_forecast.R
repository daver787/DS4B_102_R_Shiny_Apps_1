aggregate_time_series <-
function(data, time_unit ="month"){
    output_tbl <- data %>%
        
        mutate(date = floor_date(order.date, unit = time_unit)) %>%
        
        group_by(date) %>%
        summarize(total_sales = sum(extended_price)) %>%
        ungroup() %>%
        
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))
    
   return(output_tbl)
    
    
}
plot_time_series <-
function(data){
    data %>%
        
        ggplot(aes(date, total_sales)) +
        
        geom_line(color = "#2c3e50") +
        geom_point(aes(text = label_text), color = "#2c3e50", size = 0.1) +
        geom_smooth(method = "loess", span = 0.2) +
        
        theme_tq() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "", y = "")
    
    ggplotly(g, tooltip = "text")
    
}
generate_forecast <-
function(data, n_future, seed = NULL){
 
    train_tbl <- data %>% tk_augment_timeseries_signature()
    
    
    future_data_tbl <- data %>%
        tk_index() %>%
        tk_make_future_timeseries(length_out = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
        tk_get_timeseries_signature()
    
    seed <- seed
    set.seed(seed)
    
    model_xgboost <- boost_tree(
        mode           = "regression",
        mtry           = 20, 
        trees          = 500, 
        min_n          = 3, 
        tree_depth     = 8,
        learn_rate     = 0.01, 
        loss_reduction = 0.01) %>%
        set_engine("xgboost") %>%
        fit.model_spec(total_sales ~ ., data = train_tbl %>% select(-date, -label_text, -diff))
    
    
    output_tbl <- predict(model_xgboost, new_data = future_data_tbl) %>%
        bind_cols(future_data_tbl) %>%
        select(.pred, index) %>%
        rename(total_sales = .pred,
               date        = index) %>%
        mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
        add_column(key = "Prediction") %>%
        bind_rows(
            data %>%
                add_column(key="Actual")) %>%
        arrange(date) 
    
    return(output_tbl)
    
}
plot_forecast <-
function(data){
   
     g <- data %>%
        ggplot(aes(x = date, y = total_sales, color = key)) +
        geom_line() +
        geom_point(aes(text = label_text), size = 0.01) +
        geom_smooth(method = "loess", span = 0.2) +
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "", y = "")
    

        ggplotly(g,tooltip = "text")
        
}
