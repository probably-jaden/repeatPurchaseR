#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

#

library(shiny)
library(dplyr)
library(lubridate)
library(tidyr)
library(tidyverse)


testDF <- read_csv("~/Documents/repeatPurchase Willas Kitchen/WillasKitchenRPR_18Months.csv")
month_data <- month_group(clean_data(testDF), N = 1)


#val_allPurchases_perCustomer <- read_csv("~/Documents/repeatPurchaseTesting/val_allPurchases_perCustomer.csv")
#val_uniqueOrders <- read_csv("repeatPurchaseTesting/val_uniqueOrders.csv")
#val_purchasesPerMonth <- read_csv("repeatPurchaseTesting/purchasesPerMonth.csv")
#colnames(testDF)



# testDF %>%
#     group_by(`Shipment ID`) %>%
#     summarize(diffAMI = unique(`Amazon Order Id`))

dspl_tbl_clean <- function(df){
    df <- df %>%
        dplyr::rename_with(~stringr::str_replace_all(., " ", "_")) %>%
        dplyr::rename_with(~stringr::str_replace_all(., "[:punct:]", "")) %>%
        dplyr::rename_with(~stringr::str_replace_all(., "[:space:]", ""))


    cleaned_data <- df %>%
        dplyr::select(c(PurchaseDate, BuyerEmail, ShippedQuantity, ItemPrice, AmazonOrderId, ShippingPostalCode, MerchantSKU)) %>%
        # copilot give the monthYr as follows "july 2024" instead of "2024-07"
        dplyr::mutate(monthYr = format(PurchaseDate, "%B %Y"))
    return(cleaned_data)
}


clean_data <- function(df, dropSameOrders = TRUE) {
    required_columns <- c("Amazon Order Id", "Reporting Date", "Buyer Email", "Item Price", "Shipping Postal Code", "Shipped Quantity", "Merchant SKU")

    # Check if all required columns are present
    missing_columns <- setdiff(required_columns, names(df))
    if (length(missing_columns) > 0) {
        stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
    }

    df %>%
        select(all_of(required_columns)) %>%
        mutate(
            `Buyer Email` = trimws(`Buyer Email`),
            `Reporting Date` = ymd_hms(`Reporting Date`, tz = "UTC"),
            `Reporting Month` = format(`Reporting Date`, "%Y-%m")
        ) %>%
        filter(`Buyer Email` != "") %>%
        {if(dropSameOrders) distinct(., `Amazon Order Id`, `Buyer Email`, .keep_all = TRUE) else .} %>%
        arrange(`Reporting Date`, `Buyer Email`)
}

# calculate_time_diff <- function(group) {
#     group %>%
#         arrange(`Reporting Date`) %>%
#         mutate(time_diff = as.numeric(difftime(`Reporting Date`, lag(`Reporting Date`), units = "days")))
# }
#
# cleaner_data <- function(df, timeCutOff = 0.125) {
#     clean_df <- clean_data(df, dropSameOrders = FALSE)
#
#     clean_df %>%
#         group_by(`Amazon Order Id`) %>%
#         summarise(
#             `Buyer Email` = first(`Buyer Email`),
#             `Reporting Date` = last(`Reporting Date`),
#             `Reporting Month` = last(`Reporting Month`),
#             count = n(),
#             `Item Price` = sum(`Item Price`),
#             `Shipped Quantity` = sum(`Shipped Quantity`)
#         ) %>%
#         arrange(`Buyer Email`, `Reporting Date`) %>%
#         group_by(`Buyer Email`) %>%
#         do(calculate_time_diff(.)) %>%
#         mutate(order_group = cumsum(coalesce(time_diff > timeCutOff, TRUE))) %>%
#         group_by(`Buyer Email`, order_group) %>%
#         summarise(
#             `Amazon Order Id` = last(`Amazon Order Id`),
#             `Reporting Date` = last(`Reporting Date`),
#             `Reporting Month` = last(`Reporting Month`),
#             `Shipped Quantity` = sum(`Shipped Quantity`),
#             `Item Price` = sum(`Item Price`),
#             .groups = "drop"
#         )
# }

calculate_time_diff <- function(group) {
    group %>%
        arrange(`Reporting Date`) %>%
        mutate(time_diff = as.numeric(difftime(`Reporting Date`, lag(`Reporting Date`), units = "days")))
}

calculate_avg_time_diff <- function(group, timeCutOff) {
    group %>%
        arrange(`Reporting Date`) %>%
        mutate(time_diff = as.numeric(difftime(`Reporting Date`, lag(`Reporting Date`), units = "days"))) %>%
        summarise(avg_time_diff = mean(time_diff[time_diff > timeCutOff], na.rm = TRUE))
}

cleaner_data <- function(df, timeCutOff = 0.125) {
    clean_df <- clean_data(df, dropSameOrders = FALSE)

    temp_df <- clean_df %>%
        group_by(`Amazon Order Id`) %>%
        summarise(
            `Buyer Email` = first(`Buyer Email`),
            `Reporting Date` = last(`Reporting Date`),
            `Reporting Month` = last(`Reporting Month`),
            count = n(),
            `Item Price` = sum(`Item Price`),
            `Shipped Quantity` = sum(`Shipped Quantity`)
        ) %>%
        arrange(`Buyer Email`, `Reporting Date`)

    avg_time_diffs <- temp_df %>%
        group_by(`Buyer Email`) %>%
        do(calculate_avg_time_diff(., timeCutOff))

    temp_df %>%
        group_by(`Buyer Email`) %>%
        do(calculate_time_diff(.)) %>%
        mutate(order_group = cumsum(coalesce(time_diff > timeCutOff, TRUE))) %>%
        group_by(`Buyer Email`, order_group) %>%
        summarise(
            `Amazon Order Id` = last(`Amazon Order Id`),
            `Reporting Date` = last(`Reporting Date`),
            `Reporting Month` = last(`Reporting Month`),
            `Shipped Quantity` = sum(`Shipped Quantity`),
            `Item Price` = sum(`Item Price`),
            .groups = "drop"
        ) %>%
        left_join(avg_time_diffs, by = "Buyer Email")
}


month_group <- function(df) {
    grouped_orders <- df %>%
        arrange(`Reporting Date`) %>%
        group_by(`Buyer Email`) %>%
        mutate(
            cumCount = row_number(),
            cumSpend = cumsum(`Item Price`),
            repeatPurchaser = cumCount != 1
        ) %>%
        ungroup() %>%
        mutate(Reporting.Month = as.character(`Reporting Month`))

    month_grouped <- grouped_orders %>%
        group_by(`Reporting Month`, repeatPurchaser) %>%
        summarise(
            count = n(),
            spendPerMonth = sum(`Item Price`),
            .groups = "drop"
        )

    uniqueMonths <- unique(month_grouped$`Reporting Month`)
    full_data <- expand.grid(
        `Reporting Month`= uniqueMonths,
        repeatPurchaser = c(FALSE, TRUE)
    )

    merged_data <- full_data %>%
        left_join(month_grouped, by = c("Reporting Month", "repeatPurchaser")) %>%
        replace_na(list(count = 0, spendPerMonth = 0))

    pivoted_data <- merged_data %>%
        pivot_wider(
            id_cols = `Reporting Month`,
            names_from = repeatPurchaser,
            values_from = c(count, spendPerMonth)
        ) %>%
        rename(
            "New Customer" = count_FALSE,
            "Repeat Customer" = count_TRUE,
            "New Customer Spend" = spendPerMonth_FALSE,
            "Repeat Customer Spend" = spendPerMonth_TRUE
        )

    result <- pivoted_data %>%
        mutate(
            `Total Orders` = `New Customer` + `Repeat Customer`,
            `Total Spend` = `New Customer Spend` + `Repeat Customer Spend`,
            `New Customer %` = round(`New Customer` / `Total Orders` * 100),
            `Repeat Customer %` = round(`Repeat Customer` / `Total Orders` * 100),
            `New Customer Spend %` = round(`New Customer Spend` / `Total Spend` * 100, 1),
            `Repeat Customer Spend %` = round(`Repeat Customer Spend` / `Total Spend` * 100, 1)
        )

    # Ensure all columns are present even if they contain only zeros
    all_columns <- c(
        "New Customer", "Repeat Customer", "Total Orders",
        "New Customer Spend", "Repeat Customer Spend", "Total Spend",
        "New Customer %", "Repeat Customer %",
        "New Customer Spend %", "Repeat Customer Spend %"
    )

    result[setdiff(all_columns, names(result))] <- 0

    return(result)
}

month_group <- function(df, N = 3) {
    grouped_orders <- df %>%
        arrange(`Reporting Date`) %>%
        group_by(`Buyer Email`) %>%
        mutate(
            cumCount = row_number(),
            cumSpend = cumsum(`Item Price`),
            purchase_category = case_when(
                cumCount <= N ~ as.character(cumCount),
                TRUE ~ paste(N + 1, "or more")
            )
        ) %>%
        ungroup() %>%
        mutate(`Reporting Month` = as.character(`Reporting Month`))

    month_grouped <- grouped_orders %>%
        group_by(`Reporting Month`, purchase_category) %>%
        summarise(
            count = n(),
            spendPerMonth = sum(`Item Price`),
            .groups = "drop"
        )

    uniqueMonths <- unique(month_grouped$`Reporting Month`)
    categories <- c(as.character(1:N), paste(N + 1, "or more"))
    full_data <- expand.grid(
        `Reporting Month` = uniqueMonths,
        purchase_category = categories,
        stringsAsFactors = FALSE
    )

    merged_data <- full_data %>%
        left_join(month_grouped, by = c("Reporting Month", "purchase_category")) %>%
        replace_na(list(count = 0, spendPerMonth = 0))

    pivoted_data <- merged_data %>%
        pivot_wider(
            id_cols = `Reporting Month`,
            names_from = purchase_category,
            values_from = c(count, spendPerMonth),
            names_glue = "{purchase_category}_{.value}"
        )

    # Calculate totals
    result <- pivoted_data %>%
        mutate(
            Orders_Total = rowSums(select(., ends_with("_count"))),
            Spend_Total = rowSums(select(., ends_with("_spendPerMonth")))
        )

    # Calculate percentages
    for (cat in categories) {
        result <- result %>%
            mutate(
                !!paste0(cat, "_Orders%") := round(!!sym(paste0(cat, "_count")) / Orders_Total * 100, 1),
                !!paste0(cat, "_Spend%") := round(!!sym(paste0(cat, "_spendPerMonth")) / Spend_Total * 100, 1)
            )
    }

    # Rename columns for clarity
    for (cat in categories) {
        result <- result %>%
            rename(
                !!paste0(cat, "_Orders") := !!sym(paste0(cat, "_count")),
                !!paste0(cat, "_Spend") := !!sym(paste0(cat, "_spendPerMonth"))
            )
    }

    # Ensure all expected columns are present
    expected_columns <- c(
        "Reporting.Month",
        paste0(rep(categories, each = 4), c("_Orders", "_Spend", "_Orders%", "_Spend%")),
        "Orders_Total", "Spend_Total"
    )

    missing_columns <- setdiff(expected_columns, names(result))
    if (length(missing_columns) > 0) {
        result[missing_columns] <- 0
    }

    return(result)
}

# plot_monthly_metrics <- function(data, metric = "Orders", type = "value", N = 3, chart_type = "bar") {
#     # Define category names
#     data <- month_group(data, N)
#     categories <- c(as.character(1:N), paste(N + 1, "or more"))
#
#     long_data <- if(type == "value") {
#         data %>%
#             pivot_longer(cols = ends_with(c("_Orders", "_Spend")),
#                          names_to = c("category", "measure"),
#                          names_pattern = "(.*)_(.*)",
#                          values_to = "value") %>%
#             filter(measure == metric) %>%
#             mutate(`Reporting Month` = ym(`Reporting Month`))
#     } else {
#         data %>%
#             pivot_longer(cols = ends_with(c("_Orders%", "_Spend%")),
#                          names_to = c("category", "measure"),
#                          names_pattern = "(.*)_(.*%)",
#                          values_to = "value") %>%
#             filter(measure == paste0(metric, "%")) %>%
#             mutate(`Reporting Month` = ym(`Reporting Month`))
#     }
#
#     # Base plot
#     p <- ggplot(long_data, aes(x = `Reporting Month`, y = value, fill = category, color = category)) +
#         scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
#         labs(
#             title = paste("Monthly", metric, if(type == "percent") "Percentages" else ""),
#             x = "Month",
#             y = if(type == "percent") "Percentage" else metric,
#             fill = "Purchase Number",
#             color = "Purchase Number"
#         ) +
#         theme_minimal() +
#         theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
#     # Add geom based on type and chart_type
#     if (type == "percent") {
#         p <- p + geom_bar(stat = "identity", position = "stack")
#     } else {
#         p <- p + case_when(
#             chart_type == "area" ~ list(geom_area(position = "stack")),
#             chart_type == "bar" ~ list(geom_bar(stat = "identity", position = "dodge")),
#             chart_type == "line" ~ list(geom_line(size = 1), geom_point(size = 2)),
#             TRUE ~ list(geom_area(position = "stack"))  # default to area if invalid type
#         )
#     }
#
#     # Add comma formatting for non-percentage y-axis
#     if(type != "percent") {
#         p <- p + scale_y_continuous(labels = scales::comma)
#     }
#
#     return(p)
# }
# test_data <- cleaner_data(testDF)
#
# plot_monthly_metrics(test_data, metric = "Orders", type = "percent", N = 2,  chart_type = "bar")

plot_monthly_metrics <- function(data, metric = "Orders", type = "value", N = 3, chart_type = "bar") {
    # Define category names
    data <- month_group(data, N)
    categories <- c(as.character(1:N), paste(N + 1, "or more"))

    long_data <- if(type == "value") {
        data %>%
            pivot_longer(cols = ends_with(c("_Orders", "_Spend")),
                         names_to = c("category", "measure"),
                         names_pattern = "(.*)_(.*)",
                         values_to = "value") %>%
            filter(measure == metric) %>%
            mutate(`Reporting Month` = ym(`Reporting Month`))
    } else {
        data %>%
            pivot_longer(cols = ends_with(c("_Orders%", "_Spend%")),
                         names_to = c("category", "measure"),
                         names_pattern = "(.*)_(.*%)",
                         values_to = "value") %>%
            filter(measure == paste0(metric, "%")) %>%
            mutate(`Reporting Month` = ym(`Reporting Month`))
    }

    # Create a custom factor level for category
    long_data <- long_data %>%
        mutate(category = factor(category, levels = categories, ordered = TRUE))

    # Define a color palette
    colors <- c(colorRampPalette(c("skyblue", "mediumpurple2","mediumpurple4"))(N), "indianred2")

    # Base plot
    p <- ggplot(long_data, aes(x = `Reporting Month`, y = value, fill = category, color = category)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
        scale_fill_manual(values = colors, breaks = categories) +
        scale_color_manual(values = colors, breaks = categories) +
        labs(
            title = paste("Monthly", metric, if(type == "percent") "Percentages" else ""),
            x = "Month",
            y = if(type == "percent") "Percentage" else metric,
            fill = "Purchase Number",
            color = "Purchase Number"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    # Add geom based on type and chart_type
    if (type == "percent") {
        p <- p + geom_bar(stat = "identity", position = "stack")
    } else {
        p <- p + case_when(
            chart_type == "area" ~ list(geom_area(position = "stack")),
            chart_type == "bar" ~ list(geom_bar(stat = "identity", position = "dodge")),
            chart_type == "line" ~ list(geom_line(size = 1), geom_point(size = 2)),
            TRUE ~ list(geom_area(position = "stack"))  # default to area if invalid type
        )
    }

    # Add comma formatting for non-percentage y-axis
    if(type != "percent") {
        p <- p + scale_y_continuous(labels = scales::comma)
    }

    return(p)
}




#unique(testDF$`Merchant SKU`)

#titles <- testDF %>%
#    select("Title", "Merchant SKU")

# create_ridgeline_plot <- function(data, N = 1, scale = 5, rel_min_height = 0.01, include_new_buyers = TRUE) {
#
#     data <- month_group(data, N)
#
#     order_columns <- names(data)[grep("_Orders$", names(data))]
#
#     # Exclude '1_Orders' if include_1_orders is FALSE
#     if (!include_new_buyers) {
#         order_columns <- order_columns[order_columns != "1_Orders"]
#     }
#
#     # Reshape the data from wide to long format
#     long_data <- data %>%
#         pivot_longer(
#             cols = all_of(order_columns),
#             names_to = "Order_Group",
#             values_to = "Orders"
#         ) %>%
#         mutate(
#             Order_Group = gsub("_Orders$", "", Order_Group),
#             `Reporting Month` = as.Date(paste0(`Reporting Month`, "-01"))
#         )
#
#     # Create the plot
#     ggplot(long_data, aes(x = `Reporting Month`, y = Order_Group, height = Orders, fill = Order_Group)) +
#         geom_density_ridges(
#             stat = "identity",
#             scale = scale,
#             rel_min_height = rel_min_height,
#             alpha = 0.6
#         ) +
#         scale_fill_viridis_d() +
#         scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
#         labs(title = "Ridgeline Plot of Orders by Group",
#              x = "Reporting Month",
#              y = "Order Group",
#              fill = "Order Group") +
#         theme_ridges() +
#         theme(legend.position = "none",
#               axis.text.x = element_text(angle = 45, hjust = 1))
# }

create_ridgeline_plot <- function(data, N = 1, scale = 5, rel_min_height = 0.01, include_new_buyers = TRUE) {
    data <- month_group(data, N)
    order_columns <- names(data)[grep("_Orders$", names(data))]

    # Exclude '1_Orders' if include_new_buyers is FALSE
    if (!include_new_buyers) {
        order_columns <- order_columns[order_columns != "1_Orders"]
    }

    # Reshape the data from wide to long format
    long_data <- data %>%
        pivot_longer(
            cols = all_of(order_columns),
            names_to = "Order_Group",
            values_to = "Orders"
        ) %>%
        mutate(
            Order_Group = gsub("_Orders$", "", Order_Group),
            `Reporting Month` = as.Date(paste0(`Reporting Month`, "-01"))
        )

    # Create custom factor levels for Order_Group
    categories <- c(as.character(1:N), paste(N + 1, "or more"))
    long_data <- long_data %>%
        mutate(Order_Group = factor(Order_Group, levels = categories, ordered = TRUE))

    # Define a color palette
    colors <- c(colorRampPalette(c("skyblue", "mediumpurple2","mediumpurple4"))(N), "indianred2")

    # Create the plot
    ggplot(long_data, aes(x = `Reporting Month`, y = Order_Group, height = Orders, fill = Order_Group)) +
        geom_density_ridges(
            stat = "identity",
            scale = scale,
            rel_min_height = rel_min_height,
            alpha = 0.6
        ) +
        scale_fill_manual(values = colors, breaks = categories) +
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        labs(title = "Ridgeline Plot of Orders by Group",
             x = "Reporting Month",
             y = "Order Group",
             fill = "Order Group") +
        theme_ridges() +
        theme(
            legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
}

# cleanDF <- cleaner_data(testDF)
#
# nrow(cleanDF)
#
# latestDF <- cleanDF %>%
#     arrange(desc(`Reporting Date`)) %>%
#     distinct(`Buyer Email`, .keep_all = TRUE)
#
#
# summaryStats <- latestDF %>%
#     mutate(newCustomer = ifelse(order_group == 1, TRUE, FALSE)) %>%
#     group_by(newCustomer) %>%
#     summarise(
#         count = n(),
#         total_orders = sum(`order_group`, na.rm = TRUE),
#         total_shipped_quantity = sum(`Shipped Quantity`, na.rm = TRUE),
#         total_item_price = sum(`Item Price`, na.rm = TRUE)
#
# nrow(latestDF)
#
# cleanDF
# month_group(cleanDF, N = 1)

group_orders <- function(df, N) {
    # Check if avg_time_diff column exists
    has_avg_time_diff <- "avg_time_diff" %in% names(df)

    df %>%
        group_by(`Buyer Email`) %>%
        summarize(
            order_group = n(),
            `Shipped Quantity` = sum(`Shipped Quantity`, na.rm = TRUE),
            `Item Price` = sum(`Item Price`, na.rm = TRUE),
            avg_time_diff = if(has_avg_time_diff) mean(avg_time_diff, na.rm = TRUE) else NA
        ) %>%
        mutate(grouped_order = case_when(
            order_group <= N ~ as.character(order_group),
            order_group > N ~ paste0(">", N)
        )) %>%
        group_by(grouped_order) %>%
        summarise(
            count = n(),
            total_orders = sum(order_group, na.rm = TRUE),
            total_shipped_quantity = sum(`Shipped Quantity`, na.rm = TRUE),
            total_item_price = sum(`Item Price`, na.rm = TRUE),
            Average_Number_of_Days_Between_Each_Purchase = if(has_avg_time_diff) mean(avg_time_diff, na.rm = TRUE) else NA
        ) %>%
        mutate(
            Lifetime_Value_of_the_Customer = total_item_price/count,
            avg_Units_Per_Customer = total_shipped_quantity/count
        ) %>%
        arrange(grouped_order) %>%
        # Remove NA column if avg_time_diff wasn't present
        select_if(~!all(is.na(.)))
}

# # Usage example:
# myDF <- group_orders(cleanDF, N = 1)
#
# cleanDF
#
# create_bar_chart(cleanDF, "Average_Number_of_Days_Between_Each_Purchase", repeat_Purchase_Groups = 5)
# library(ggplot2)
# library(dplyr)

create_bar_chart <- function(data, bar_chart_type, repeat_Purchase_Groups = 1) {

    data <- group_orders(data, N = repeat_Purchase_Groups)
    # Validate y_axis input
    valid_y_axes <- c("total_orders", "total_shipped_quantity", "total_item_price",
                      "Average_Number_of_Days_Between_Each_Purchase",
                      "Lifetime_Value_of_the_Customer", "avg_Units_Per_Customer")
    if (!bar_chart_type %in% valid_y_axes) {
        stop("Invalid y_axis. Choose from: ", paste(valid_y_axes, collapse = ", "))
    }

    # Create a new column for numerical order
    data <- data %>%
        mutate(numeric_order = as.numeric(ifelse(grepl("^>", grouped_order), Inf, grouped_order))) %>%
        arrange(numeric_order)

    # Determine the number of groups (excluding the last ">N" group)
    n_groups <- sum(!grepl("^>", data$grouped_order))

    # Create a color palette
    colors <- colorRampPalette(c("skyblue", "mediumpurple2","mediumpurple4"))(n_groups)
    colors <- c(colors, "indianred2")

    # Create named vector for color mapping
    color_mapping <- setNames(colors, c(as.character(1:n_groups), tail(data$grouped_order, 1)))

    # Create the plot
    p <- ggplot(data, aes(x = grouped_order, y = .data[[bar_chart_type]], fill = grouped_order)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = color_mapping) +
        scale_x_discrete(limits = data$grouped_order) +  # Preserve the original order
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none"
        ) +
        labs(
            x = "# of Purchases",
            y = gsub("_", " ", (bar_chart_type)),
            title = paste(gsub("_", " ", bar_chart_type))
        )

    return(p)
}


#
# create_bar_chart(cleanDF, "Average_Number_of_Days_Between_Each_Purchase", repeat_Purchase_Groups = 5)
#
# cleanDF <- clean_data(testDF)
# groupedDF <- group_orders(cleanDF, N = 1)
#


# cleanData <- clean_data(testDF)
# month_data <- month_group(cleanData)
#
#
# month_data <- month_group(data, N)
# glimpse(month_data)
#
# create_ridgeline_plot(cleanData, N = 5, scale = 2.5, include_1_orders = TRUE)
#
#
# glimpse(test_data)

skuRPR <- function(df) {
    df <- df %>%
        dplyr::rename_with(~stringr::str_replace_all(., " ", "_")) %>%
        dplyr::rename_with(~stringr::str_replace_all(., "[:punct:]", "")) %>%
        dplyr::rename_with(~stringr::str_replace_all(., "[:space:]", ""))


    dplyr::select(-c(AmazonOrderId, ItemTax, ShippingPrice, ShippingTax, GiftWrapTax,
                     GiftWrapPrice, ItemPromoDiscount, ShipmentPromoDiscount)) %>%

    df_SkuNames <- unique(df$`Merchant SKU`)

    resultDF <- df %>%
        tidyr::pivot_wider(names_from = MerchantSKU, values_from = ShippedQuantity, values_fill = 0) %>%
        dplyr::group_by(AmazonOrderId) %>%
        dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), sum),
                  BuyerEmail = unique(BuyerEmail),
                  PurchaseDate = unique(PurchaseDate),
                  orderCount = n()) %>%
        dplyr::group_by(BuyerEmail) %>%
        dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), sum)) %>%
        dplyr::arrange(dplyr::desc(BuyerEmail)) %>%
        dplyr::filter(!is.na(BuyerEmail)) %>%
        dplyr::group_by(orderCount) %>%
        dplyr::summarize(dplyr::across(tidyselect::where(is.numeric), sum)) %>%
        tidyr::pivot_longer(cols = unique(df$MerchantSKU), names_to = "MerchantSKU", values_to = "Quantity")

    return(resultDF)
}

# skuRPR(testDF)
#
#



############################################################
# Check if each Amazon Order Id has a unique Purchase Date #
############################################################

# check_unique <- testDF %>%
#     group_by(`Amazon Order Id`) %>%
#     summarize(
#         unique_dates = n_distinct(`Purchase Date`),
#         .groups = 'drop'
#     ) %>%
#     filter(unique_dates > 1)
#
# # If this dataframe is empty, your assumption is correct
# if (nrow(check_unique) == 0) {
#     print("All Amazon Order Ids have unique Purchase Dates")
# } else {
#     print("Some Amazon Order Ids have multiple Purchase Dates:")
#     print(check_unique)
# }

#
# result <- testDF %>%
#     # Sort the data by Buyer ID and Date
#     group_by(`Amazon Order Id`) %>%
#     summarize(`Buyer Email` = unique(`Buyer Email`),
#               `Purchase Date` = unique(`Purchase Date`),
#               `Item Price` = sum(`Item Price`),
#               `Shipped Quantity` = sum(`Shipped Quantity`)) %>%
#     arrange(`Buyer Email`, `Purchase Date`) %>%
#     group_by(`Buyer Email`) %>%
#     mutate(time_diff = as.numeric(difftime(`Purchase Date`, lag(`Purchase Date`), units = "days"))) %>%
#
#     # Calculate the average time difference, excluding NAs (first purchase)
#     summarise(
#         avg_time_between_purchases = mean(time_diff, na.rm = TRUE),
#         purchase_count = n()
#     ) %>%
#
#     # Filter out buyers with only one purchase
#     filter(purchase_count > 1) %>%
#     # Round avg_time_between_purchases to 2 decimal places
#     mutate(avg_time_between_purchases = round(avg_time_between_purchases, 2)) %>%
#     filter(avg_time_between_purchases <= 1,
#            !is.na(`Buyer Email`))
#
#
# quickRPR <- testDF %>%
#     select(`Amazon Order Id`, `Buyer Email`, `Purchase Date`, `Item Price`, `Shipped Quantity`) %>%
#     group_by(`Amazon Order Id`) %>%
#     summarize(`Buyer Email` = unique(`Buyer Email`),
#               `Purchase Date` = unique(`Purchase Date`),
#               `Item Price` = sum(`Item Price`),
#               `Shipped Quantity` = sum(`Shipped Quantity`)) %>%
#     mutate(date = format(`Purchase Date`, "%b %d, %I:%M %p"),
#            buyerName = "Robert Stevenson") %>%
#     left_join(result, by = "Buyer Email") %>%
#     filter(!is.na(avg_time_between_purchases),
#            `Buyer Email`== "wrp0thw2q6kw36s@marketplace.amazon.com") %>%
#     arrange(desc(`Buyer Email`)) %>%
#     select(`Amazon Order Id`, date, buyerName, `Buyer Email`)
#
# # Set global option to avoid scientific notation
# options(scipen = 999)
#
# # View the results
# print(result)



# categorize_skus <- function(df) {
#     df %>%
#         arrange(orderCount, desc(Quantity)) %>%
#         group_by(orderCount) %>%
#         mutate(
#             cumulative_percentage = cumsum(Quantity) / sum(Quantity),
#             Merchant_SKU_Category = if_else(cumulative_percentage <= 1, `Merchant SKU`, "Other")
#         ) %>%
#         group_by(orderCount, Merchant_SKU_Category) %>%
#         summarise(Quantity = sum(Quantity), .groups = "drop")
# }

skuRPR_FillPlot <- function(df){
    cleanDF <- skuRPR(df)
    resultPlot <- ggplot2::ggplot(data = cleanDF)+
        ggplot2::geom_bar(ggplot2::aes(x = orderCount, y = Quantity, fill = MerchantSKU), stat = "identity", position = "fill")+
        ggplot2::theme_minimal()

    return(resultPlot)
}

#
# # filter by month, week, year (for how often the repeat purchases are being made)
#
# uniqueOrders<- function(df){
#     unique_carts <- df %>%
#         dplyr::group_by(`Amazon Order Id`) %>%
#         dplyr::summarize(`Buyer Email` = unique(`Buyer Email`),
#                          `Purchase Date` = unique(`Purchase Date`),
#                          count = n(),
#                          pricePerOrder = sum(`Item Price`),
#                          unitsPerOrder = sum(`Shipped Quantity`)) %>%
#         dplyr::arrange(dplyr::desc(`Purchase Date`))
#
#     return(unique_carts)
# }
#
#  testUniqueOrders <- uniqueOrders(testDF)
# #
# # otherTest <- testUniqueOrders %>% group_by(`Buyer Email`) %>% summarize(count = n()) %>%
# #     arrange(desc(`Buyer Email`))
#
# timeCutOff <- 1
#
# numberOrders <- function(df){
#     number_orders <- uniqueOrders(df) %>%
#         dplyr::arrange(`Buyer Email`, `Purchase Date`) %>%
#         dplyr::group_by(`Buyer Email`) %>%
#         mutate(time_diff = as.numeric(difftime(`Purchase Date`, lag(`Purchase Date`), units = "days"))) %>%
#         dplyr::summarize(orderCount = n(),
#                          avgTimeBetweenPurchases = mean(time_diff, na.rm = TRUE),
#                          customerSpend = sum(pricePerOrder),
#                          customerQuantity = sum(unitsPerOrder)) %>%
#         dplyr::mutate(avgOrderUnitsCount = customerQuantity/orderCount,
#                       avgOrderSpend = customerSpend/orderCount,
#                       avgTimeBetweenPurchases = round(avgTimeBetweenPurchases, 2)) %>%
#         dplyr::filter(!is.na(`Buyer Email`),
#                       avgTimeBetweenPurchases >= timeCutOff)
#                        #%>%
#         #dplyr::arrange(dplyr::desc(`Buyer Email`)) %>%
#
#
#     return(number_orders)
# }
#
#
#  test2 <- numberOrders(testDF)
#
#
#  result <- testDF %>%
#      # Sort the data by Buyer ID and Date
#      group_by(`Amazon Order Id`) %>%
#      summarize(`Buyer Email` = unique(`Buyer Email`),
#                `Purchase Date` = unique(`Purchase Date`),
#                `Item Price` = sum(`Item Price`),
#                `Shipped Quantity` = sum(`Shipped Quantity`)) %>%
#      arrange(`Buyer Email`, `Purchase Date`) %>%
#      group_by(`Buyer Email`) %>%
#      mutate(time_diff = as.numeric(difftime(`Purchase Date`, lag(`Purchase Date`), units = "days"))) %>%
#
#      # Calculate the average time difference, excluding NAs (first purchase)
#      summarise(
#          avg_time_between_purchases = mean(time_diff, na.rm = TRUE),
#          purchase_count = n()
#      ) %>%
#
#      # Filter out buyers with only one purchase
#      filter(purchase_count > 1) %>%
#      # Round avg_time_between_purchases to 2 decimal places
#      mutate(avg_time_between_purchases = round(avg_time_between_purchases, 2)) %>%
#      filter(avg_time_between_purchases <= 1,
#             !is.na(`Buyer Email`))

#
#
# ggplot(data = test2)+
#     geom_violin(aes(x = as.factor(orderCount), y = avgOrderSpend))+
#     geom_jitter(aes(x = as.factor(orderCount), y = avgOrderSpend), width = .1, color = "red")

library(formattable)


ui <- fluidPage(
    # Application title
    titlePanel("Repeat Purchase Analysis"),
    br(),
    sidebarLayout(
        sidebarPanel(
            fileInput("files", "Choose CSV File(s)",
                      multiple = TRUE,
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
        ),
        mainPanel(
            DT::DTOutput("table")
        )
    ),
    br(),
    tabsetPanel(
        tabPanel("A) Lifetime Summary",
                 br(),
                 br(),
                 fluidRow(
                     column(3,
                            selectInput("bar_chart_type", "Bar Chart Type:",
                                        choices = c("total_orders", "total_shipped_quantity", "total_item_price",
                                                    "Average_Number_of_Days_Between_Each_Purchase",
                                                    "Lifetime_Value_of_the_Customer", "avg_Units_Per_Customer"),
                                        selected = "total_orders"),
                            numericInput("repeat_purchase_groups", "Repeat Purchase Groups:",
                                         value = 1, min = 1, max = 10)
                     ),
                     column(9,
                            plotOutput("bar_chart")
                     )
                 )
        ),
        tabPanel("B) Monthly Metrics",
                 br(),
                 fluidRow(
                     column(8,
                            h3("Monthly Metrics Plot"),
                            br(),
                            fluidRow(
                                column(3,
                                       selectInput("chart_type", "Chart Type:", choices = c("bar", "line", "area"), selected = "bar")
                                ),
                                column(2,
                                       radioButtons("metric", "Metric:",
                                                    choices = c("Orders", "Spend"),
                                                    selected = "Orders",
                                                    inline = TRUE)
                                       ),
                                column(2,
                                       radioButtons("type", "Type:",
                                                   choices = c("value", "percent"),
                                                   selected = "value",
                                                   inline = TRUE)
                                       ),
                                column(5,
                                       sliderInput("N1", "# Of Repeat Purchases:", min = 1, max = 10, value = 1, step = 1)
                                       )
                            ),
                            plotOutput("monthly_metrics_plot")
                     ),
                     column(4,
                            formattableOutput("monthly_metrics_table")
                            # formattable output

                     )
                 )
        ),
        tabPanel("D) Novelty Items",
                 br(),
                 fluidRow(
                     column(9,
                            h3("Ridgeline Plot"),
                            br(),
                            fluidRow(
                                column(4,
                                       checkboxInput("include_new_buyers", "Include New Buyers", value = TRUE)
                                ),
                                column(4,
                                       sliderInput("N2", "# Of Repeat Purchases:", min = 1, max = 10, value = 1, step = 1)
                                ),
                                column(4,
                                       sliderInput("scale", "Scale:", min = 1, max = 3, value = 1.2, step = 0.1)
                                )
                            ),
                            plotOutput("ridgeline_plot")
                     ),
                     column(3,

                     )
                 )

        ),
        tabPanel("E) Test Tab",
                 br(),
                 fluidRow(
                     column(8,
                            tags$head(
                                tags$style(HTML(".dt-buttons .dt-button {
                                        background-color: #f8f9fa;
                                        border: 1px solid #c8ced3;
                                        border-radius: 4px;
                                        color: #5c6873;
                                        font-size: 0.875rem;
                                        padding: 0.25rem 0.5rem;
                                        margin-right: 0.5rem;
                                      }
                                      .dt-buttons .dt-button:hover {
                                        background-color: #e4e7ea;
                                        border-color: #c8ced3;
                                      }
                                    "))
                            ),
                            titlePanel("Copyable Table Example"),
                            DT::DTOutput("testtable")
                            )
                     )
                 )

        # h3("Ridgeline Plot"),
        # sliderInput("N2", "# Of Repeat Purchases:",min = 1, max = 10, value = 1, step = 1),
        # #numericInput("N2", "N:", value = 1, min = 1, max = 8),
        # sliderInput("scale", "Scale:", min = 1, max = 3, value = 1.2, step = 0.1),
        # checkboxInput("include_new_buyers", "Include New Buyers", value = TRUE),
        # plotOutput("ridgeline_plot")

    ),
    br()
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    userData <- reactive({
        req(input$files)
        # Read all uploaded files
        all_data <- lapply(input$files$datapath, function(path) {
            readr::read_csv(path, col_names = input$header)
        })
        # Combine all data frames
        do.call(rbind, all_data)
    })

    cleanData <- reactive({
        req(input$files)
        clean_data(userData())
    })

    output$table <- DT::renderDT(
        dspl_tbl_clean(userData()),
        selection = "none",
        options = list(pageLength = 3),
        server = FALSE,
        editable = TRUE
    )

    output$monthly_metrics_plot <- renderPlot({
        plot_monthly_metrics(
            data = cleanData(),
            metric = input$metric,
            type = input$type,
            N = input$N1,
            chart_type = input$chart_type
        )
    })

    output$monthly_metrics_table <- renderFormattable({
        req(input$metric, input$type)

        data <- month_group(cleanData(), N = 1)

        if (input$metric == "Orders" && input$type == "value") {
            columns <- c("Reporting Month", "1_Orders", "2 or more_Orders")
            names <- c("Reporting Month", "# of Orders from New Customers", "# of Orders Repeat Customers")
        } else if (input$metric == "Orders" && input$type == "percent") {
            columns <- c("Reporting Month", "1_Orders%", "2 or more_Orders%")
            names <- c("Reporting Month", "% of All Orders from New Customers", "% of All Orders from Repeat Customers")
        } else if (input$metric == "Spend" && input$type == "value") {
            columns <- c("Reporting Month", "1_Spend", "2 or more_Spend")
            names <- c("Reporting Month", "Revenue from New Customers", "Revenue from Repeat Customers")
        } else if (input$metric == "Spend" && input$type == "percent") {
            columns <- c("Reporting Month", "1_Orders%", "2 or more_Orders%")
            names <- c("Reporting Month", "% of Revenue from New Customers", "% of Revenue from Repeat Customers")
        }
        table_data <- data[, columns]
        colnames(table_data) <- names

        formattable(
            table_data,
            align = c("l", "r", "r"),
            list(
                `Reporting Month` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                area(col = 2:3) ~ color_tile("#DeF7E9", "#71CA97")
            )
        )
    })

    output$ridgeline_plot <- renderPlot({
        create_ridgeline_plot(
            data = cleanData(),
            N = input$N2,
            scale = input$scale,
            include_new_buyers = input$include_new_buyers
        )
    })

    output$bar_chart <- renderPlot({
        create_bar_chart(
            data = cleanData(),
            bar_chart_type = input$bar_chart_type,
            repeat_Purchase_Groups = input$repeat_purchase_groups
        )
    })

    output$testtable <- DT::renderDT({
        # Example data
        data <- data.frame(
            Name = c("Alice", "Bob", "Charlie"),
            Age = c(25, 30, 35),
            City = c("New York", "London", "Paris")
        )

        DT::datatable(data,
                      extensions = 'Buttons',
                      options = list(
                          dom = 'Bfrtip',
                          buttons = list(
                              list(extend = 'copy', text = 'Copy'),
                              list(extend = 'csv', text = 'CSV'),
                              list(extend = 'excel', text = 'Excel')
                          )
                      ))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
