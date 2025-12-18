# ============================================================
# 1. LOAD PACKAGES
# ============================================================
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(ggrepel)
library(broom)

cat("\nPackages loaded.\n")


# ============================================================
# 2. LOAD RAW DATA
# ============================================================
orders     <- read_csv("data/olist_orders_dataset.csv")     %>% clean_names()
items      <- read_csv("data/olist_order_items_dataset.csv") %>% clean_names()
payments   <- read_csv("data/olist_order_payments_dataset.csv") %>% clean_names()
customers  <- read_csv("data/olist_customers_dataset.csv") %>% clean_names()
reviews    <- read_csv("data/olist_order_reviews_dataset.csv") %>% clean_names()
products   <- read_csv("data/olist_products_dataset.csv") %>% clean_names()

cat("\nData loaded.\n")


# ============================================================
# 3. MERGE ORDERS + CUSTOMERS
# ============================================================
df <- orders %>%
  left_join(customers, by = "customer_id")


# ============================================================
# 4. MERGE ITEMS + PRODUCTS
# ============================================================
items_full <- items %>%
  left_join(products %>% select(product_id, product_category_name),
            by = "product_id")


# ============================================================
# 5. ITEM SUMMARY PER ORDER
# ============================================================
items_summary <- items_full %>%
  group_by(order_id) %>%
  summarize(
    total_price   = sum(price),
    total_freight = sum(freight_value),
    product_count = n(),
    .groups = "drop"
  )


# ============================================================
# 6. PAYMENT SUMMARY PER ORDER
# ============================================================
payments_summary <- payments %>%
  group_by(order_id) %>%
  summarize(
    payment_value    = sum(payment_value),
    max_installments = max(payment_installments),
    .groups = "drop"
  )


# ============================================================
# 7. MERGE ALL DATASETS
# ============================================================
df <- df %>%
  left_join(items_summary,    by = "order_id") %>%
  left_join(payments_summary, by = "order_id") %>%
  left_join(reviews %>% select(order_id, review_score), by = "order_id")


# ============================================================
# 8. FEATURE ENGINEERING (CLEAN VERSION)
# ============================================================
df <- df %>%
  mutate(
    order_purchase_timestamp      = ymd_hms(order_purchase_timestamp),
    order_delivered_customer_date = ymd_hms(order_delivered_customer_date),
    order_estimated_delivery_date = ymd(order_estimated_delivery_date),
    
    delivery_time = as.numeric(order_delivered_customer_date - order_purchase_timestamp, "days"),
    order_value   = total_price + total_freight,
    delivery_delay = as.numeric(order_delivered_customer_date -
                                  order_estimated_delivery_date, "days")
  )

cat("\nFeature engineering done.\n")


# ============================================================
# 9. CLEAN ANALYSIS DATASET
# ============================================================
df_clean <- df %>%
  filter(
    !is.na(delivery_time),
    !is.na(order_value),
    !is.na(review_score)
  ) %>%
  mutate(is_SP = if_else(customer_state == "SP", 1, 0))

cat("\nDataset cleaned.\n")


# ============================================================
# 10. DESCRIPTIVE STATISTICS
# ============================================================

cat("\n==== MISSING VALUES ====\n")
missing_table <- df_clean %>%
  summarize(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count")
print(missing_table)


cat("\n==== NUMERIC SUMMARY ====\n")
numeric_summary <- df_clean %>% select(where(is.numeric)) %>% summary()
print(numeric_summary)


cat("\n==== CATEGORICAL SUMMARY ====\n")
categorical_summary <- df_clean %>%
  select(where(is.character)) %>%
  map_df(~ tibble(value = names(table(.x)),
                  count = as.integer(table(.x))),
         .id = "variable")
print(categorical_summary)


cat("\n==== DATASET DIMENSIONS ====\n")
print(dim(df_clean))


# ============================================================
# 11. VISUALIZATIONS
# ============================================================
# ============================================================
# 12. CUSTOM THEME (IMPROVED)
# ============================================================
theme_beme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# ============================================================
# 13. VISUALIZATIONS (IMPROVED)
# ============================================================

# ---------------------------
# 1 — Order Value Distribution
# ---------------------------
print(
  ggplot(df_clean %>% filter(order_value <= 300),
         aes(order_value)) +
    geom_histogram(binwidth = 5, fill="#4A90E2", color="white") +
    labs(
      title = "Order Value Distribution",
      subtitle = "Most orders fall between R$40 and R$180",
      x = "Order Value (R$)",
      y = "Count"
    ) +
    theme_beme
)


# ---------------------------
# 2 — Delivery Time Distribution
# ---------------------------
print(
  ggplot(df_clean %>% filter(delivery_time <= 60),
         aes(delivery_time)) +
    geom_histogram(binwidth = 1, fill="#F5A623", color="white") +
    labs(
      title = "Delivery Time Distribution",
      subtitle = "Most orders are delivered within 7–20 days",
      x = "Delivery Time (Days)",
      y = "Count"
    ) +
    theme_beme
)


# ---------------------------
# 3 — Review Score vs Delivery Time (Line Plot)
# ---------------------------
df_plot <- df_clean %>%
  filter(delivery_time >= 1, delivery_time <= 30) %>%
  mutate(delivery_day = floor(delivery_time)) %>%
  group_by(delivery_day) %>%
  summarize(avg_review = mean(review_score), .groups="drop")

print(
  ggplot(df_plot, aes(delivery_day, avg_review)) +
    geom_line(color="#1F77B4", linewidth=1.4) +
    geom_point(color="red", size=3) +
    labs(
      title = "Review Score vs Delivery Time",
      subtitle = "Customer satisfaction drops as delivery time increases",
      x = "Delivery Time (Days)",
      y = "Average Review Score"
    ) +
    scale_y_continuous(limits=c(3,5), breaks = seq(3,5,0.25)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
)


# ---------------------------
# 4 — Total Revenue by State
# ---------------------------
print(
  df_clean %>%
    group_by(customer_state) %>%
    summarize(total_revenue = sum(order_value)) %>%
    ggplot(aes(x = reorder(customer_state, total_revenue),
               y = total_revenue)) +
    geom_col(fill="#D0021B") +
    coord_flip() +
    labs(
      title = "Total Revenue by State",
      subtitle = "São Paulo leads significantly in overall purchases",
      x = "State",
      y = "Total Revenue (R$)"
    ) +
    scale_y_continuous(labels = scales::label_comma(prefix="R$ ")) +
    theme_beme
)



# ============================================================
# 12. CORRELATIONS
# ============================================================
cat("\n==== CORRELATIONS ====\n")

correlations <- tibble(
  variable_pair = c(
    "delivery_time vs review_score",
    "order_value vs review_score",
    "installments vs review_score",
    "SP vs review_score"
  ),
  correlation = c(
    cor(df_clean$delivery_time, df_clean$review_score),
    cor(df_clean$order_value, df_clean$review_score),
    cor(df_clean$max_installments, df_clean$review_score, use="complete.obs"),
    cor(df_clean$is_SP, df_clean$review_score)
  )
)

print(correlations)


# ============================================================
# 13. REGRESSIONS
# ============================================================

cat("\n==== SIMPLE REGRESSIONS ====\n")
model_1 <- lm(review_score ~ delivery_time, data=df_clean)
model_2 <- lm(review_score ~ max_installments, data=df_clean)
model_3 <- lm(review_score ~ order_value, data=df_clean)
model_4 <- lm(review_score ~ is_SP, data=df_clean)

print(summary(model_1))
print(summary(model_2))
print(summary(model_3))
print(summary(model_4))


cat("\n==== MULTIPLE REGRESSION ====\n")
model_final <- lm(
  review_score ~ delivery_time + max_installments + order_value + is_SP,
  data = df_clean
)

print(summary(model_final))


# ============================================================
# 14. TIDY OUTPUT TABLE
# ============================================================
cat("\n==== TIDY FINAL MODEL ====\n")
print(tidy(model_final))

cat("\n==== FINAL CLEAN VERSION EXECUTED ====\n")

# ----- SAVE FIGURES TO latex/ FOLDER -----
ggplot(df_clean %>% filter(order_value <= 300),
       aes(order_value)) +
  geom_histogram(binwidth = 5, fill="#4A90E2", color="white") +
  labs(
    title = "Order Value Distribution",
    subtitle = "Most orders fall between R$40 and R$180",
    x = "Order Value (R$)",
    y = "Count"
  ) +
  theme_beme
ggsave("latex/fig_order_value.png", width = 7, height = 4, dpi = 300)

ggplot(df_clean %>% filter(delivery_time <= 60),
       aes(delivery_time)) +
  geom_histogram(binwidth = 1, fill="#F5A623", color="white") +
  labs(
    title = "Delivery Time Distribution",
    subtitle = "Most orders are delivered within 7–20 days",
    x = "Delivery Time (Days)",
    y = "Count"
  ) +
  theme_beme
ggsave("latex/fig_delivery_time.png", width = 7, height = 4, dpi = 300)

df_plot <- df_clean %>%
  filter(delivery_time >= 1, delivery_time <= 30) %>%
  mutate(delivery_day = floor(delivery_time)) %>%
  group_by(delivery_day) %>%
  summarize(avg_review = mean(review_score), .groups="drop")

ggplot(df_plot, aes(delivery_day, avg_review)) +
  geom_line(color="#1F77B4", linewidth=1.4) +
  geom_point(color="red", size=3) +
  labs(
    title = "Review Score vs Delivery Time",
    subtitle = "Customer satisfaction drops as delivery time increases",
    x = "Delivery Time (Days)",
    y = "Average Review Score"
  ) +
  scale_y_continuous(limits=c(3,5), breaks = seq(3,5,0.25)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave("latex/fig_review_vs_delivery.png", width = 7, height = 4, dpi = 300)

df_clean %>%
  group_by(customer_state) %>%
  summarize(total_revenue = sum(order_value)) %>%
  ggplot(aes(x = reorder(customer_state, total_revenue),
             y = total_revenue)) +
  geom_col(fill="#D0021B") +
  coord_flip() +
  labs(
    title = "Total Revenue by State",
    subtitle = "São Paulo leads significantly in overall purchases",
    x = "State",
    y = "Total Revenue (R$)"
  ) +
  scale_y_continuous(labels = scales::label_comma(prefix="R$ ")) +
  theme_beme
ggsave("latex/fig_revenue_by_state.png", width = 7, height = 4, dpi = 300)

delivery_time_summary <- summary(df_clean$delivery_time)
print(delivery_time_summary)

