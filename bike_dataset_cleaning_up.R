library(tidyverse)

uncleaned.bike.sales.data <- read.csv("~/Books/Coding Project/Version 1/Databases/Datasets List1/KAGGLE/Bike dataset uncleaned/archive/uncleaned bike sales data.csv")

bu_data <- uncleaned.bike.sales.data

head(bu_data)   # view of the data
tail(bu_data)

bu_data |> glimpse()       # structure and view of the dataset

bu_data |> sapply(class)          # type of each column


columns_to_drop <- c("Sales_Order..", "Day", "Month", "Year", "Product_Category", "Sub_Category")  
bu_data <- bu_data |> 
  select(-all_of(columns_to_drop))                 # remove redundant columns

bu_data <- bu_data |> 
  na.omit()

bu_data <- bu_data |> 
  filter(!apply(bu_data == "", 1, any))              # remove blank cells that aren't NaN

bu_data <- bu_data |> 
  separate(Product_Description,
           into = c("Variant", "Color", "Frame_Size"),  # description column contains three variables, so separate them out
           sep = "\\s|,",
           extra = "merge",
           remove = FALSE)

bu_data <- bu_data |> 
  mutate(Date = lubridate::dmy(Date))    # converting dates to yyyy-mm-dd format for consistency

bu_data <- bu_data |> 
  mutate(across(where(is.character), str_trim))      # trim whitespace from all char columns

bu_data <- bu_data |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\s{2,}", " ")))   # remove double or more spaces between words in categorical columns

currency_cols <- c("Unit_Cost", "Unit_Price", "Profit", "Cost", "Revenue")
bu_data <- bu_data |> 
  mutate(across(all_of(currency_cols), ~parse_number(.)))          # convert columns with currency and comma symbols to numerics



# columns separated by data types for easy access


numerical_bu_data <- bu_data |>      # numerical columns subsetted, not a different copy 
  select(where(is.numeric))

char_bu_data <- bu_data |>           # character columns subsetted, not a different copy
  select(where(is.character))

date_bu_dta <- bu_data |>             # date columns subsetted, not a different copy
  select(where(lubridate::is.Date))




                                      # unique values of all categorical columns

categorical_distinct_values <- function(){
 
   for (col in names(char_bu_data)){
    u = unique(char_bu_data[col])
    print(u)
  }
}

bu_data |> is.na() |> any()       # check if any values is NaN

                                  # structure and view after cleaning the dataset
bu_data |> glimpse()


