ns## code to prepare `DATASET` dataset goes here

# This script constructs R data sets from the book
# "Basic Engineering Data Collection and Analysis" by Vardeman and Jobe
# 1. Import supplied exercise data sets and modify as needed
# 2. Reconstruct text examples

library(tidyverse)

# Import supplied exercise data -------------------------------------------

# exercise data sets are in tab-delimited text formats
# following files required editing to parse correctly
#   Chapter 3/End of Chapter/Prob 11.txt
#   Chapter 6/End of Chapter/Prob 33.txt
#   Chapter 6/Section 3/Prob 03.txt
#   Chapter 6/Section 4/Prob 02.txt
#   Chapter 7/End of Chapter/Prob 16.txt
#     reshaped for clarity
#   Chapter 7/End of Chapter/Prob 19b.txt
#     modified to use raw data only
#   Chapter 7/End of Chapter/Prob 21.txt
#   Chapter 7/Section 6/Prob 4.txt
#   Chapter 8/Section 1/Prob 1.txt
#   Chapter 8/Section 2/Prob 1.txt

# Tables 5.6 and 5.7 combined into single table as "uncounted"
# Table 5.8 presented as "uncounted"

# vector of filenames
file_names <- dir(here::here("data-raw/"), recursive = TRUE)

# vector of R syntactic names for data sets imported from text
step1 <- str_replace(file_names, "Chapter ", "ch")
step2 <- str_replace(step1, "End of Chapter/", "eoc_")
step3 <- str_remove(step2, ".txt")
step4 <- str_replace_all(step3, "/", "_")
step5 <- str_replace(step4, "Prob ", "prob")
df_names <- str_replace(step5, "Section ", "sec")
rm(step1, step2, step3, step4, step5)

# function to import text file
# CALL list2env() and # usethis::use_data() HERE?
get_text_data <- function(filename)
  read_tsv(here::here(paste0("data-raw/", filename)),
           show_col_types = FALSE)

# import text files and assign names to dataframe objects
df_list <- map(file_names, get_text_data, .progress = TRUE)
names(df_list) <- df_names
list2env(df_list, envir = .GlobalEnv)

# RUN TO HERE FOR CHANGES TO DATA -----------------------------------------

# https://dataanalytics.org.uk/save-all-objects-to-disk-as-separate-files/
# THIS SEEMS TO WORK

# for(i in 1:length(ls())) {
#   save(list = (ls()[i]),
#        file = paste(ls()[i], ".RData", sep = "")
#        )
#   }

# clean up
# rm(list = ls())

## this works, but need .Rdata
# for (name in names(df_list)) {
#   file_name <- paste0(name, ".rds")
#   saveRDS(df_list[[name]],
#           file = file.path(here::here(),
#                            "data",
#                            file_name))
#   print(paste("Saved", name, "to", file_name))
#   }

# https://cran.r-project.org/web/packages/rio/vignettes/rio.html#Importing_Data_Lists

# google search "r save multiple objects to separate files"
# # Create some example objects
# object1 <- data.frame(a = 1:5, b = letters[1:5])
# object2 <- list(name = "John", age = 30)
# object3 <- c(10, 20, 30)
#
# # Store objects in a list for easy iteration
# object_list <- list(obj1 = object1, obj2 = object2, obj3 = object3)
#
# # Loop through the list and save each object to a separate .rds file
# for (name in names(object_list)) {
#   file_name <- paste0(name, ".rds")
#   saveRDS(object_list[[name]], file = file_name)
#   print(paste("Saved", name, "to", file_name))
#   }

# Reshape data ------------------------------------------------------------

# reshape "Chapter 7/End of Chapter/Prob 16.txt"
# ch7_eoc_prob16 <- ch7_eoc_prob16 |>
#   pivot_longer(cols = -starts_with("Sample"),
#                values_to = "count") |>
#   select(sample = `Sample...1`, name, count) |>
#   separate_wider_position(name,
#                           widths = c(N = 6, day = 2)) |>
#   select(day, sample, count) |>
#   arrange(day, sample)
# usethis::use_data(ch7_eoc_prob16, overwrite = TRUE)

# Chapter 7/End of Chapter/Prob 19b.txt
# use raw data
# ch7_eoc_prob19b <-
#   data.frame(batch = factor(rep(1:15, each = 4)),
#              rod_length = c(0.0075,  0.0100,  0.0135,  0.0135,
#                            -0.0085,  0.0035, -0.0180,  0.0010,
#                             0.0085,  0.0000,  0.0100,  0.0020,
#                             0.0005, -0.0005,  0.0145,  0.0170,
#                             0.0130,  0.0035,  0.0120,  0.0070,
#                            -0.0115, -0.0110, -0.0085, -0.0105,
#                            -0.0080, -0.0070, -0.0060, -0.0045,
#                            -0.0095, -0.0100, -0.0130, -0.0165,
#                             0.0090,  0.0125,  0.0125,  0.0080,
#                            -0.0105, -0.0100, -0.0150, -0.0075,
#                             0.0115,  0.0150,  0.0175,  0.0180,
#                             0.0020,  0.0005,  0.0010,  0.0010,
#                            -0.0010, -0.0025, -0.0020, -0.0030,
#                            -0.0020,  0.0015,  0.0025,  0.0025,
#                            -0.0010, -0.0015, -0.0020, -0.0045)
#              )

# usethis::use_data(ch7_eoc_prob19b, overwrite = TRUE)

# Reshape Chapter 3, Chapter Exercise 4
# reshaping here because complex; names need to be changed to syntactic
ch3_eoc_prob04 <-
  read_tsv(
    here::here(paste0("data-raw/", "Chapter 3/End of Chapter/Prob 04.txt")),
    name_repair = make.names) |>
  pivot_longer(everything(),
               names_to = "resistor",
               values_to = "ohms") |>
  separate_wider_delim(cols = resistor,
                       delim = "W",
                       names = c("watts", "nominal")) |>
  mutate(watts = factor(watts, levels = c("X1.4", "X1.2")),
         nominal = factor(nominal,
                          levels = c("20", "75", "100", "150", "200"))) |>
  arrange(watts, nominal)
# usethis::use_data(ch3_eoc_prob04, overwrite = TRUE)

# Modify Chapter 3, Chapter Exercise 7
ch3_eoc_prob07 <- ch3_eoc_prob07 |>
  dplyr::select(A, B)
# usethis::use_data(ch3_eoc_prob07, overwrite = TRUE)

ch3_eoc_prob12 <-
  read_tsv(
    here::here(paste0("data-raw/", "Chapter 3/End of Chapter/Prob 12.txt")),
    name_repair = make.names)
# usethis::use_data(ch3_eoc_prob12, overwrite = TRUE)

ch3_eoc_prob19 <-
  read_tsv(
    here::here(paste0("data-raw/", "Chapter 3/End of Chapter/Prob 19.txt")),
    name_repair = make.names)
# usethis::use_data(ch3_eoc_prob19, overwrite = TRUE)

ch4_eoc_prob06 <- ch4_eoc_prob06 |>
  rename(Y = Year)
usethis::use_data(ch4_eoc_prob06, overwrite = TRUE)

rm(df_list, df_names, file_names, get_text_data)

# Chapter 1 example data --------------------------------------------------

# Example 7, p. 16
# Book paper thickness measurements (inches)
ch1_xmp07 <- data.frame(
  tech = rep(c("Wendel", "Gulliver"), each = 10),
  thickness = c(0.0807, 0.0826, 0.0854, 0.0817, 0.0824,
                0.0799, 0.0812, 0.0807, 0.0816, 0.0804,
                0.0972, 0.0964, 0.0978, 0.0971, 0.0960,
                0.0947, 0.1200, 0.0991, 0.0980, 0.1033))
# usethis::use_data(ch1_xmp07, overwrite = TRUE)

# Table 1.4, p. 21
table1_04 <- data.frame(
  point = seq(1, 23),
  displacement = c(0.8, 4.8, 10.8, 20.1, 31.9, 45.9, 63.3, 83.1, 105.8,
                   131.3, 159.5, 190.5, 223.8, 260.0, 299.2, 340.5, 385.0,
                   432.2, 481.8, 534.2, 589.8, 647.7, 708.8))
# usethis::use_data(table1_04, overwrite = TRUE)

# Chapter 2 example data --------------------------------------------------

# Table 2.1, p. 31
x <-  c(1, 1, 33.01, 25.0,
        1, 2, 33.08, 24.0,
        1, 3, 33.24, 23.5,
        2, 4, 32.93, 26.0,
        2, 5, 33.17, 23.0,
        2, 6, 33.07, 25.0,
        3, 7, 33.01, 25.5,
        3, 8, 32.82, 27.0,
        3, 9, 32.91, 26.0,
        4, 10, 32.80, 26.5,
        4, 11, 32.86, 28.5,
        4, 12, 32.89, 25.5,
        5, 13, 32.73, 27.0,
        5, 14, 32.57, 28.0,
        5, 15, 32.65, 26.5,
        6, 16, 32.43, 30.0,
        6, 17, 32.54, 28.0,
        6, 18, 32.61, 26.0)
table2_01 <- matrix(x, nrow = 18, byrow = TRUE) |>
  as.data.frame() |>
  set_names(c("sample", "item", "mass", "width"))
rm(x)
# usethis::use_data(table2_01, overwrite = TRUE)

# Chapter 3 example data --------------------------------------------------

# Table 3.4, p. 74
x <- c(1, 16, 16, 2, 15, 16, 3, 15, 17, 4, 15, 16, 5, 20, 20,
       6, 19, 16, 7, 19, 20, 8, 17, 19, 9, 15, 15, 10, 11, 15,
       11, 17, 19, 12, 18, 17, 13, 18, 14, 14, 15, 15, 15, 18, 17,
       16, 15, 17, 17, 18, 20, 18, 15, 14,19, 17, 17, 20, 14, 16,
       21, 17, 18, 22, 19, 16, 23, 19, 18, 24, 19, 20, 25, 15, 15,
       26, 12, 15, 27, 18, 20, 28, 13, 18, 29, 14, 18, 30, 18, 18,
       31, 18, 14, 32, 15, 13, 33, 16, 17, 34, 16, 16)

table3_04 <- matrix(x, nrow = 34, byrow = TRUE) |>
  as.data.frame() |>
  set_names(c("component", "bolt3_torque", "bolt4_torque"))
rm(x)
# usethis::use_data(table3_04, overwrite = TRUE)

# Table 3.5, p. 76
table3_05 <-
  data.frame(joint = 1:30,
             diameter = c(-0.005,  0.000, -0.010, -0.030, -0.010,
                          -0.025, -0.030, -0.035, -0.025, -0.025,
                          -0.025, -0.035, -0.040, -0.035, -0.035,
                           0.015,  0.000,  0.000, -0.015, -0.015,
                          -0.005, -0.015, -0.015, -0.010, -0.015,
                          -0.035, -0.025, -0.020, -0.025, -0.015))
# usethis::use_data(table3_05, overwrite = TRUE)

# Table 3.6, p. 79
table3_06 <- data.frame(test = 1:10,
                        strength = c(8577, 9471, 9011, 7583, 8572,
                                    10688, 9614, 9614, 8527, 9165))
# usethis::use_data(table3_06, overwrite = TRUE)

# Table 3.13, p. 94
table3_13 <- data.frame(supplier = c(rep(1, 6), rep(2, 8)),
                        pct_waste = c(0.37, 0.52, 0.65, 0.92, 2.89, 3.62,
                                      0.89, 0.99, 1.45, 1.47, 1.58, 2.27,
                                      2.63, 6.54))
# usethis::use_data(table3_13, overwrite = TRUE)

# Table 3.14, p. 100
table3_14 <- data.frame(sample = 1:25,
                        xbar = c(3509.4, 3509.2, 3512.6, 3511.6, 3512.0,
                                 3513.6, 3511.8, 3512.2, 3500.0, 3502.0,
                                 3501.4, 3504.0, 3503.6, 3504.4, 3504.6,
                                 3513.0, 3512.4, 3510.8, 3511.8, 3512.4,
                                 3511.0, 3510.6, 3510.2, 3510.4, 3510.8),
                        r = c(5, 2, 3, 4, 4, 6, 3, 2, 3, 2, 2, 2, 3, 4, 3,
                              2, 1, 5, 4, 3, 4, 1, 5, 2, 3))
# usethis::use_data(table3_14, overwrite = TRUE)

# Table 3.15, p. 101
table3_15 <- data.frame(
  wood = rep(c("pine", "fir", "oak"), each = 3),
  glue = rep(c("white", "carpenter's", "cascamite"), times = 3),
  mean_strength = c(131.7, 192.7, 201.3, 92.0, 146.3, 156.7, 257.7,
                    234.3, 177.7))
# usethis::use_data(table3_15, overwrite = TRUE)

# Table 3.16, p. 105
table3_16 <- data.frame(category = LETTERS[1:4],
                        connectors = c(3, 0, 11, 1))
# usethis::use_data(table3_16, overwrite = TRUE)

# Table 3.17, p. 106
table3_17 <- data.frame(
  problem = c("Type 1 leak", "Type 2 leak", "Type 3 leak", "Missing part 1",
              "Missing part 2", "Missing part 3", "Bad part 4", "Bad part 5",
              "Bad part 6", "Wrong part 7", "Wrong part 8"),
  n_tools = c(8, 4, 3, 2,  1, 2, 1, 2, 1, 2, 2))
# usethis::use_data(table3_17, overwrite = TRUE)

# Table 3.18, p. 109
table3_18 <- data.frame(
  job = rep(1:18, times = 2),
  type = rep(c("scrap", "rework"), each = 18),
  pct = c(2, 3, 0, 0, 0, 2, 0, 0, 2, 3, 0, 1, 0, 0, 0, 0, 0, 1,
          25, 11, 5, 0, 20, 23, 6, 5, 8, 18, 3, 5, 0, 0, 3, 2, 2, 5))
# usethis::use_data(table3_18, overwrite = TRUE)

# Table 3.19, p. 110
table3_19 <- data.frame(day = 1:26,
                        defects_truck = c(1.54, 1.42, 1.57, 1.40, 1.51, 1.08,
                                          1.27, 1.18, 1.39, 1.42, 2.08, 1.85,
                                          1.82, 2.07, 2.32, 1.23, 2.91, 1.77,
                                          1.61, 1.25, 1.15, 1.37, 1.79, 1.68,
                                          1.78, 1.84))
# usethis::use_data(table3_19, overwrite = TRUE)

# Table 3.20, p. 112
table3_20 <- data.frame(
  shot_size = rep(c("small", "large"), each = 2),
  mixture = rep(c("20_reground", "50_reground"), times = 2),
  n_conform = c(38, 66, 29, 53))
# usethis::use_data(table3_20, overwrite = TRUE)

# Chapter 4 example data --------------------------------------------------

# Table 4.1, p. 124
# showing pressure in thousands of pounds
table4_01 <- data.frame(
  pressure = rep(c(2, 4, 6, 8, 10), each = 3),
  density = c(2.486, 2.479, 2.472, 2.558, 2.570, 2.580, 2.646, 2.657,
              2.653, 2.724, 2.774, 2.808, 2.861, 2.879, 2.858))
# usethis::use_data(table4_01, overwrite = TRUE)

# Table 4.2, p. 131
# Refer to Table 4.1

# Table 4.3, p. 133
table4_03 <- data.frame(
  percent = rep(0:5, each = 3),
  strength = c(1221, 1207, 1187, 1555, 1562, 1575, 1827, 1839, 1802,
               1609, 1627, 1642, 1451, 1472, 1465, 1321, 1289, 1292))
# usethis::use_data(table4_03, overwrite = TRUE)

# Table 4.4, p. 133
# Refer to Table 4.3

# Table 4.5, p. 136
# Refer to Table 4.3

# Table 4.6, p. 139
table4_06 <- data.frame(
  x = 10000000 + seq(from = 0.1, by = 0.1, length.out = 6),
  y = c(1.1, 1.9, 3.1, 3.9, 4.9, 6.1))
# usethis::use_data(table4_06, overwrite = TRUE)
# lm1 <- lm(y ~ x, data = table4_06)
# Coefficients: (1 not defined because of singularities)

# Table 4.7, p. 146
# Refer to Table 1.4

# Table 4.8, p. 150
# data in book are different than generally presented
# these are the data as they appear in the book
air <- c(80, rep(62, 4), rep(58, 6), rep(50, 5), 56)
water <- c(27, 22, 23, 24, 24, 23, 18, 18, 17, 18, 19, 18, 18, 19, 19, 20, 20)
acid <- c(88, 87, 87, 93, 93, 87, 80, 89, 88, 82, 93, 89, 86, 72, 79, 80, 82)
stack <- c(37, 18, 18, 19, 20, 15, 14, 14, 13, 11, 12, 8, 7, 8, 8, 9, 15)
table4_08 <- data.frame(air, water, acid, stack)
rm(air, water, acid, stack)
# usethis::use_data(table4_08, overwrite = TRUE)

# Table 4.10, p. 156
# Lift/drag ratios for 9 canard/tail position combinations
table4_10 <- data.frame(
  canard = rep(c(-1.2, 0, 1.2), each = 3),
  tail = rep(c(-1.2, 0, 1.2), times = 3),
  lift_drag = c(0.858, 3.156, 3.644, 4.281, 3.481, 3.918, 4.136, 3.364, 4.018))
# usethis::use_data(table4_10, overwrite = TRUE)

# Table 4.11, p. 164
# Measured strength of 16 wood joints
table4_11 <- data.frame(
  specimen = 1:16,
  joint = c("beveled", "butt", "beveled", "butt", "beveled", "beveled",
            "lap", "beveled", "butt", "lap", "lap", "lap", "butt",
            "lap", "butt", "beveled"),
  wood = c("oak", "pine", "walnut", "oak", "oak", "pine", "walnut",
                  "walnut", "walnut", "oak", "oak", "pine", "pine", "pine",
                  "walnut", "pine"),
  strength = c(1518, 829, 2571, 1169, 1927, 1348, 1489, 2443, 1263, 1295,
               1561, 1000, 596, 859, 1029, 1207))
# usethis::use_data(table4_11, overwrite = TRUE)

# Table 4.12, p. 165
# Refer to Table 4.11

# Table 4.13, p. 169
# Refer to Table 4.11
# still can't replicate
# fit reduced model instead

# Table 4.14, p. 170
# Refer to Table 4.11

# Table 4.15, p. 173
# Flight distances of golf balls
A <- c(180,193,197,189,187,192,190,182,192,179)
B <- c(180,185,167,162,170,175,190,185,180,185)
C <- c(196,192,191,194,186,180,195,197,192,193)
D <- c(190,195,180,170,180,185,167,180,180,165)
table4_15 <-
  data.frame(distance = c(A, B, C, D),
             compression = rep(c(80,100), each = 10, length.out = 40),
             evening =rep(c(1, 2), each = 20)) |>
  mutate(across(c(compression, evening), factor))
rm(A, B, C, D)
# usethis::use_data(table4_15, overwrite = TRUE)

# Table 4.16, p. 174
# Refer to Table 4.15

# Table 4.17, p. 176
# Refer to Table 4.15

# Table 4.18, p. 177
# Refer to Table 4.15

# Table 4.19, and Table 4.20, p. 180
# raw data in article?
A <- rep(1:2, times = 4)
B <- rep(rep(1:2, each = 2), times = 2)
C <- rep(1:2, each = 4)
strength_mean = c(1520, 2450, 2340, 2900, 1670, 2540, 2230, 3230)
table4_20 <- data.frame(temp = A, time = B, span = C, strength_mean)
rm(A, B, C, strength_mean)
# usethis::use_data(table4_20, overwrite = TRUE)

# Table 4.21, p. 188
# Refer to Table 4.19

# Table 4.22, p. 189
# Refer to Table 4.19

# Table 4.23, p. 190
# Refer to Table 4.19

# Table 4.24, p. 195
table4_24 <- data.frame(
  combn = c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc",
            "d", "ad", "bd", "abd", "cd", "acd", "bcd", "abcd"),
  y = c(1.68, 1.98, 3.28, 3.44, 4.98, 5.70, 9.97, 9.07,
        2.07, 2.44, 4.09, 4.53, 7.77, 9.43, 11.75, 16.30))
# usethis::use_data(table4_24, overwrite = TRUE)

# Table 4.25, p. 199
x <- c(90, 24.4, 21.1, 150,
       90, 29.3, 23.7, 10,
       90, 34.2, 20.7, 8,
       100, 24.4, 21.1, 35,
       100, 29.3, 24.1, 8,
       100, 34.2, 22.2, 7,
       110, 24.4, 18.4, 18,
       110, 29.3, 23.4, 8,
       110, 34.2, 21.9, 10)
table4_25 <- matrix(x, nrow = 9, byrow = TRUE) |>
  as.data.frame() |>
  set_names(c("temp", "amt_B", "yield", "fil_time"))
rm(x)
# usethis::use_data(table4_25, overwrite = TRUE)

# Table 4.26, p. 200
# Refer to Table 4.25

# Chapter 5 example data --------------------------------------------------

# Table 5.1, p. 224
# Refer to Table 3.4
# Only frequencies shown
table5_01 <- table3_04 |>
  select(z = bolt3_torque) |>
  summarize(.by = z, n = n()) |>
  arrange(z)
# usethis::use_data(table5_01, overwrite = TRUE)

# Table 5.2, p. 224
# Refer to Table 5.1
# table5_02 <- table5_01 |>
#   mutate(f_z = n / sum(n)) |>
#   select(-n)
# usethis::use_data(table5_02, overwrite = TRUE)

# Table 5.3, p. 227
# Refer to Table 5.2
# table5_03 <- table5_02 |>
#   mutate(F_z = cumsum(f_z))
# usethis::use_data(table5_03, overwrite = TRUE)

# mean of random variable Z
# Z_mean <- summarize(table5_03, mean_z = sum(z * f_z))

# Table 5.4, p. 231
# Refer to Table 3.4
# table5_04 <- table5_03 |>
#   select(z, f_z) |>
#   mutate(z_mean = sum(z * f_z),
#          resid = z - z_mean,
#          resid_sqr = resid^2)
# usethis::use_data(table5_04, overwrite = TRUE)

# variance of random variable Z
# Z_var <- summarize(table5_04, var_z = sum(resid_sqr * f_z))

# Table 5.5, p. 231
# table5_05 <- data.frame(w = 0:9,
#                         f_w = 0.1)
# usethis::use_data(table5_05, overwrite = TRUE)
# "mean of square" minus "square of mean"
# with(table5_05, sum(w^2 * f_w) - sum(w * f_w)^2)

# Table 5.6, p. 265
# showing as "uncounted"
x <- c(4.81, 1, 5.00, 12,
       4.86, 1, 5.01, 10,
       4.88, 1, 5.02, 7,
       4.89, 1, 5.03, 7,
       4.91, 2, 5.04, 5,
       4.92, 2, 5.05, 4,
       4.93, 3, 5.06, 4,
       4.94, 2, 5.07, 3,
       4.95, 6, 5.08, 2,
       4.96, 4, 5.09, 3,
       4.97, 5, 5.10, 2,
       4.98, 4, 5.11, 1,
       4.99, 7, 5.13, 1)

step1 <- matrix(x, nrow = 13, byrow = TRUE)
x <- c(step1[,1], step1[,3])
y <- c(step1[,2], step1[,4])
set.seed(42)
table5_06 <- data.frame(weight = x, freq = y) |>
  uncount(freq) |>
  # randomly permute rows to replicate actual results
  slice_sample(prop = 1)
rm(step1, x, y)
# usethis::use_data(table5_06, overwrite = TRUE)

# Table 5.7, p. 265
# table5_07 <- table5_06 |>
#   mutate (p = (rank(weight, ties = "average") - 0.5) / length(weight),
#           z = qnorm(p))
# usethis::use_data(table5_07, overwrite = TRUE)

# Table 5.8, p. 267
x <- c(6, 1, 7, 0, 8, 3, 9, 0, 10, 4, 11, 10, 12, 0, 13, 6, 14, 1)

set.seed(42)
table5_08 <- matrix(x, nrow = 9, byrow = TRUE) |>
  as.data.frame()  |>
  set_names(c("thread_lgth", "freq")) |>
  uncount(freq) |>
  # randomly permute rows to replicate actual results
  slice_sample(prop = 1)
rm(x)
# usethis::use_data(table5_08, overwrite = TRUE)

# Example 15, p. 270
ch5_xmp15 <- data.frame(
  sale_time = c(8, 8, 8, 9, 9,
                10, 10, 10, 10, 10, 12, 12, 12, 12, 13, 13, 14, 14, 14, 14, 14,
                14, 15, 16, 17, 17, 17, 17, 18, 18, 19, 19,
                20, 21, 22, 22, 22, 22, 23, 24, 26, 27, 28, 29, 29, 29,
                30, 32, 32, 32, 34, 34, 36, 36, 37, 37,
                42, 43, 45, 46, 47, 48, 48, 70, 87)
  )
# usethis::use_data(ch5_xmp15, overwrite = TRUE)

# Example 15 and Figure 5.21, p. 270
# Use as Table 5.9, p. 76
# Old ID ch5_xmp16
table5_09 <- data.frame(
  voltage = c(39.4, 45.3, 49.2, 49.4, 51.3, 52.0, 53.2, 53.2, 54.9,
              55.5, 57.1, 57.2, 57.5, 59.2, 61.0, 62.4, 63.8, 64.3,
              67.3, 67.7)
  )
usethis::use_data(table5_09, overwrite = TRUE)
# usethis::use_data(ch5_xmp16, overwrite = TRUE)

# Table 5.10, p. 280
# table of joint probabilities
# table5_10 <- table3_04 |>
#   summarize(.by = c(bolt3_torque, bolt4_torque), n = n()) |>
#   arrange(bolt3_torque, bolt4_torque) |>
#   mutate(p = n / sum(n))
# usethis::use_data(table5_10, overwrite = TRUE)

# Table 5.12, p. 283
# marginal probabilities for Y (bolt 4 torque)
# table5_11 <- table5_10 |>
#   summarize(.by = bolt4_torque,
#             marginal_y = sum(p)) |>
#   arrange(bolt4_torque)
# usethis::use_data(table5_11, overwrite = TRUE)

# marginal probabilities for X (bolt 3 torque)
# summarize(table5_10, .by = bolt3_torque,
#           marginal_x = sum(p)) |>
#   arrange(bolt3_torque)

# Table 5.13, p. 284 and Table 5.14, p. 286
# table5_13 <- table5_10 |>
  # filter(bolt3_torque == 15) |>
  # # summarize(.by = bolt4_torque, n = sum(n)) |>
  # mutate(p_15 = p / sum(p))
# usethis::use_data(table5_13, overwrite = TRUE)

# CHECK 5.15 AND 5.16
# Table 5.15, p. 286 and Table 5.16, p. 287
# table5_15 <- table5_10 |>
#   filter(bolt3_torque == 18) |>
#   mutate(p_cond = p / sum(p))
# usethis::use_data(table5_15, overwrite = TRUE)

# table5_16 <- table5_10 |>
#   filter(bolt4_torque == 20) |>
#   mutate(p_cond = p / sum(p))
# usethis::use_data(table5_16, overwrite = TRUE)

# Table 5.17, p. 288
# Refer to Table 5-10

# Table 5.18, p. 289
# Make two-way table

# Table 5.19, p. 303
# Table 5.20, p. 303
# Table 5.21, p. 304
# Table 5.22, p. 304
# Table 5.23, p. 305
# Table 5.24, p. 315
# Table 5.25, p. 317

# Chapter 6 example data --------------------------------------------------

# Table 6.1, p. 337
# Table 6.2, p. 356
# Table 6.3, p. 357

# Table 6.4, p. 366
table6_04 <- data.frame(lifetime = c(225, 171, 198, 189, 189,
                                     135, 162, 135, 117, 162))
# usethis::use_data(table6_04, overwrite = TRUE)

# Table 6.5, p. 370
x <- c(1, 0.168, 0.169,
       2, 0.170, 0.168,
       3, 0.165, 0.168,
       4, 0.165, 0.168,
       5, 0.170, 0.169)

table6_05 <- matrix(x, nrow = 5, byrow = TRUE) |>
  as.data.frame() |>
  set_names(c("piece", "leading", "trailing"))
# usethis::use_data(table6_05, overwrite = TRUE)

# Table 6.6, p. 372
# refer to table 6.5

# Example 10, p. 374 and Figure 6.14, p. 375
molded <- c(117.9,
            124.5, 123.6, 121.2, 129.8, 128.9, 127.9, 127.1, 126.1, 125.7,
            125.1,
            132.3, 131.3, 130.0, 138.0, 137.0, 136.5, 136.3, 136.2,
            142.2, 140.1,
            152.1, 151.2, 150.2)
crushed <- c(161.8, 165.8, 169.6,
             171.3, 172.0, 172.4, 173.3, 173.4, 173.7, 176.6, 179.8,
             180.2, 180.9, 183.3, 183.8, 184.9, 185.5, 186.5, 187.1, 187.3,
             189.1, 189.8,
             190.0, 191.0)
ch6_xmp10 <- data.frame(molded, crushed)
rm(molded, crushed)
# usethis::use_data(ch6_xmp10, overwrite = TRUE)

# Table 6.7, p. 379
stress_950 <- c(225, 171, 198, 189, 189, 216, 162, 153, 216, 225)
stress_900 <- c(135, 162, 135, 117, 162, 216, 306, 225, 243, 189)
table6_07 <- data.frame(stress_950, stress_900)
rm(stress_950, stress_900)
# usethis::use_data(table6_07, overwrite = TRUE)

# Table 6.8, p. 389
x <- c(8, 1, 9, 1, 10, 10, 11, 4, 12, 3, 13, 1)
set.seed(42)
table6_08 <- matrix(x, nrow = 6, byrow = TRUE) |>
  as.data.frame() |>
  set_names(c("dimension", "freq")) |>
  uncount(freq) |>
  # randomly permute rows to replicate actual results
  slice_sample(prop = 1)
rm(x)
# usethis::use_data(table6_08, overwrite = TRUE)

# Table 6.9, p. 395
table6_09 <- data.frame(
  hardness = c(32.8, 44.9, 34.4, 37.0, 23.6, 29.1, 39.5, 30.1, 29.2, 19.2,
              21.0, 24.5, 19.9, 14.8, 18.8),
  treatment = c(rep("heat_treated", 10),
                rep("cold_rolled", 5)
                ))
# usethis::use_data(table6_09, overwrite = TRUE)

# Table 6.10, p. 417
x <- matrix(c(2.99, 1, 3.11, 24,
              3.01, 4, 3.13, 17,
              3.03, 4, 3.15, 13,
              3.05, 4, 3.17, 6,
              3.07, 7, 3.19, 2,
              3.09, 17, 3.21, 1),
            nrow = 6, byrow = TRUE)
table6_10 <- data.frame(wgt =  c(x[,1], x[,3]),
                        freq = c(x[,2], x[,4])) |>
  uncount(freq)
rm(x)
# usethis::use_data(table6_10, overwrite = TRUE)

# Chapter 7 example data --------------------------------------------------

# Table 7.1, p. 445
table7_01 <- matrix(c(1, 1, 5800,
                      2, 1, 4598,
                      3, 1, 6508,
                      4, 2, 5659,
                      5, 2, 6225,
                      6, 2, 5376,
                      7, 3, 5093,
                      8, 3, 4386,
                      9, 3, 4103,
                      10, 4, 3395,
                      11, 4, 3820,
                      12, 4, 3112,
                      13, 5, 3820,
                      14, 5, 2829,
                      15, 5, 2122,
                      16, 6, 2971,
                      17, 6, 3678,
                      18, 6, 3325,
                      19, 7, 2122,
                      20, 7, 1372,
                      21, 7, 1160,
                      22, 8, 2051,
                      23, 8, 2631,
                      24, 8, 2490),
                    nrow = 24, byrow = TRUE) |>
  as.data.frame() |>
  set_names(c("specimen", "concrete", "strength"))
# usethis::use_data(table7_01, overwrite = TRUE)

# Table 7.2, p. 446
table7_02 <- data.frame(type = c(rep("Type 1", 7),
                                 rep("Type 2", 6),
                                 rep("Type 3", 6)),
                        # k is standard notation for spring constant
                        # units are newtons per meter
                        k = c(1.99, 2.06, 1.99, 1.94, 2.05, 1.88, 2.30,
                              2.85, 2.74, 2.74, 2.63, 2.74, 2.80,
                              2.10, 2.01, 1.93, 2.02, 2.10, 2.05)) |>
  mutate(type = factor(type))
# usethis::use_data(table7_02, overwrite = TRUE)

# Table 7.3 and Table 7.4, p. 450
# Refer to Table 7.1

# Table 7.5 and Table 7.6, p. 452
# Refer to Table 7.2

# Function for confidence interval on residual standard error
# Example 1, p. 457

# Table 7.7, p. 463
# Refer to Table 7.1

# Table 7.8, p. 466
table7_08 <- data.frame(brand = c("Generic", "National B", "National V"),
                        n = 5,
                        y_bar = c(93.2, 81.0, 83.8),
                        s = c(0.8, 0.7, 0.8))
# usethis::use_data(table7_08, overwrite = TRUE)

# Table 7.9, p. 467
# Sample data
table7_09 <- data.frame(
  pct_water = factor(c("17%", "19%")),
  heat = factor(c("slow cool", "fast cool"))) |>
  expand(pct_water, heat) |>
  mutate(n = 3) |>
  uncount(n) |>
  mutate(MOR = c(3824, 3140, 3502,
                 4911, 5998, 5676,
                 4768, 3672, 3242,
                 4387, 5388, 5007))
# usethis::use_data(table7_09, overwrite = TRUE)

# Table 7.10, p. 468
# Refer to Table 7.9

# Table 7.11, p. 482
# Refer to Table 7.1

# Table 7.12, p. 484
# No data

# Table 7.13, p. 485
# Refer to Table 7.1

# Table 7.14, p. 488
# Random effects
table7_14 <- data.frame(
  specimen = factor(rep(1:5, times = 10)),
  Mg = c(76, 69, 73, 73, 70,
         71, 71, 69, 75, 66,
         70, 68, 68, 69, 68,
         67, 71, 69, 72, 68,
         71, 66, 70, 69, 64,
         65, 68, 70, 69, 70,
         67, 71, 65, 72, 69,
         71, 69, 67, 63, 67,
         66, 70, 67, 69, 69,
         68, 68, 64, 69, 67)) |>
  arrange(specimen)
# usethis::use_data(table7_14, overwrite = TRUE)

# Table 7.15, p. 494
# Refer to Table 7.14

# Table 7.16, p. 497
table7_16 <- data.frame(sample = factor(rep(1:22, each = 5)),
                        excess = c(9, 10, 7, 8, 10,
                                   6, 10, 8, 8, 10,
                                   11, 10, 9, 5, 11,
                                   10, 9, 9, 8, 7,
                                   7, 5, 11, 9, 5,
                                   9, 9, 10, 7, 9,
                                   10, 8, 6, 11, 8,
                                   7, 10, 8, 8, 9,
                                   10, 9, 9, 5, 12,
                                   8, 10, 6, 8, 10,
                                   8, 10, 4, 7, 8,
                                   8, 10, 10, 6, 9,
                                   10, 8, 6, 7, 10,
                                   8, 6, 10, 8, 8,
                                   13, 5, 8, 8, 13,
                                   10, 4, 9, 10, 8,
                                   7, 7, 9, 7, 8,
                                   9, 7, 7, 9, 6,
                                   5, 10, 5, 8, 10,
                                   9, 6, 8, 9, 11,
                                   6, 10, 11, 5, 6,
                                   15, 3, 7, 9, 11)) |>
  arrange(sample)
# usethis::use_data(table7_16, overwrite = TRUE)

# Table 7.17, p. 503
# remove, can be derived from Table 7.16
# x <- c(1, 8.8, 1.30, 3,
#        2, 8.4, 1.67, 4,
#        3, 9.2, 2.49, 6,
#        4, 8.6, 1.14, 3,
#        5, 7.4, 2.61, 6,
#        6, 8.8, 1.10, 3,
#        7, 8.6, 1.95, 5,
#        8, 8.4, 1.14, 3,
#        9, 9.0, 2.55, 7,
#        10, 8.4, 1.67, 4,
#        11, 7.4, 2.19, 6,
#        12, 8.6, 1.67, 4,
#        13, 8.2, 1.79, 4,
#        14, 8.0, 1.41, 4,
#        15, 9.4, 3.51, 8,
#        16, 8.2, 2.49, 6,
#        17, 7.6, 0.89, 2,
#        18, 7.6, 1.34, 3,
#        19, 7.6, 2.51, 5,
#        20, 8.6, 1.82, 5,
#        21, 7.6, 2.70, 6,
#        22, 9.0, 4.47, 12)
#
# table7_17 <- matrix(x, nrow = 22, byrow = TRUE) |>
#   as.data.frame() |>
#   set_names(c("sample", "xbar", "s", "R"))
# rm(x)
# usethis::use_data(table7_17, overwrite = TRUE)

# Table 7.18, p. 520
x <- c(1, 13, .43,
       2, 12, .40,
       3, 9, .30,
       4, 15, .50,
       5, 17, .57,
       6, 13, .43,
       7, 20, .67,
       8, 18, .60,
       9, 18, .60,
       10, 16, .53,
       11, 15, .50,
       12, 17, .57,
       13, 15, .50,
       14, 20, .67,
       15, 10, .33,
       16, 12, .40,
       17, 17, .57,
       18, 14, .47,
       19, 16, .53,
       20, 10, .33,
       21, 14, .47,
       22, 13, .43,
       23, 17, .57,
       24, 10, .33,
       25, 12, .40)

table7_18 <- matrix(x, nrow = 25, byrow = TRUE) |>
  as.data.frame() |>
  select(sample = V1, nonconform = V2) |>
  mutate(p_hat = nonconform / 30)
rm(x)
# usethis::use_data(table7_18, overwrite = TRUE)

# FILE SAVE STOPS HERE--INVESTIGATE ---------------------------------------

# Table 7.19, p. 526

x <- c(1, 11/4, 95, 114, 1.20,
       2, 11/5, 95, 142, 1.50,
       3, 11/6, 95, 146, 1.54,
       4, 11/7, 95, 257, 2.70,
       5, 11/8, 95, 185, 1.95,
       6, 11/11, 95, 228, 2.40,
       7, 11/12, 95, 327, 3.44,
       8, 11/13, 95, 269, 2.83,
       9, 11/14, 95, 167, 1.76,
       10, 11/15, 95, 190, 2.00,
       11, 11/18, 95, 199, 2.09,
       12, 11/19, 95, 180, 1.89,
       13, 11/20, 95, 171, 1.80,
       14, 11/21, 130, 163, 1.25,
       15, 11/22, 130, 205, 1.58,
       16, 11/25, 130, 292, 2.25,
       17, 11/26, 130, 325, 2.50,
       18, 11/27, 130, 267, 2.05,
       19, 11/29, 130, 190, 1.46,
       20, 12/2, 130, 200, 1.54,
       21, 12/3, 130, 185, 1.42,
       22, 12/4, 130, 204, 1.57,
       23, 12/5, 130, 182, 1.40,
       24, 12/6, 130, 196, 1.51,
       25, 12/9, 130, 140, 1.08,
       26, 12/10, 130, 165, 1.27,
       27, 12/11, 130, 153, 1.18,
       28, 12/12, 130, 181, 1.39,
       29, 12/13, 130, 185, 1.42,
       30, 12/16, 130, 270, 2.08)

table7_19 <- matrix(x, nrow = 30, byrow = TRUE) |>
  as.data.frame() |>
  set_names(c("sample", "date", "trucks", "errors", "errors_truck")) |>
  select(sample, trucks, errors) |>
  mutate(errors_truck = errors/trucks)
rm(x)
# usethis::use_data(table7_19, overwrite = TRUE)

# Chapter 8 example data --------------------------------------------------
# 42 tables

# Table 8.1, p. 547 and Table 8.2, p. 548
# Refer to Table 4.11

# Table 8.3, p. 556
# No data

# Table 8.4, p. 557
# No data

# Table 8.5, p. 559
# No data

# Table 8.6, p. 560
# Refer to Table 4.11

# Table 8.7, p. 564

# Chapter 9 example data --------------------------------------------------
# 24 tables

# Table 9.1, p. 654
# Same data as Table 4.1, p. 124

# Table 9.2, p. 655
# Refer to Table 9.1
# options(digits = 4)
table9_02 <- table4_01 |>
  summarize(.by = pressure,
            density_mean = mean(density),
            density_sd = sd(density))
# usethis::use_data(table9_02, overwrite = TRUE)

# Table 9.3, p. 655
# Refer to Table 9.1

# https://stackoverflow.com/questions/72567990/correcting-confidence-intervals-for-multiple-comparisons-using-the-multcomp-pack
# https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html

# Table 9.4, p. 655
# Refer to Table 9.1
# lm1 <- lm(density ~ pressure, data = table4_01)
# lm1_aug <- augment(lm1, interval = "confidence")

# Table 9.5, p. 666
# Refer to Table 9.1

# intervals from augmented object
# > lm1_aug |>
#      dplyr::select(pressure, .fitted, .lower, .upper) |>
#      distinct() |>
#      as.data.frame()
#   pressure .fitted  .lower  .upper
# 1        2  2.4723  2.4531  2.4916
# 2        4  2.5697  2.5561  2.5833
# 3        6  2.6670  2.6559  2.6781
# 4        8  2.7643  2.7507  2.7779
# 5       10  2.8617  2.8424  2.8809

# simultaneous intervals from Table 4.5
#   pressure      est   lower   upper
#          2   2.4723  2.4477  2.4969
#          4   2.5697  2.5523  2.5871
#          6   2.6670  2.6528  2.6812
#          8   2.7643  2.7469  2.7817
#         10   2.8617  2.8371  2.8863

# individual intervals from Table 4.5
#  pressure       est   lower   upper
#         2    2.4723  2.4587  2.4859
#         4    2.5697  2.5579  2.5815
#         6    2.6670  2.6559  2.6781
#         8    2.7643  2.7525  2.7761
#        10    2.8617  2.8481  2.8753

# using glht() from {multcomp}
# for complete list of adjustment methods, see stats::p.adjust
# for simultaneous inference about mean response, use model matrix as linfct
# K <- model.matrix(lm1) |> unique()
# lm1_glht <- glht(model = lm1,
#                  linfct = unique(model.matrix(lm1))
#                  )

# joint test, just to see how it works
# summary(lm1_glht, test = adjusted(type = "holm"))

# confint(lm1_glht, level = 0.95)
# Simultaneous Confidence Intervals
#
# Fit: lm(formula = density ~ pressure, data = table4_01)
#
# Quantile = 2.6772
# 95% family-wise confidence level
#
#
# Linear Hypotheses:
#         Estimate lwr    upr
# 1 == 0  2.4723   2.4485 2.4962
# 4 == 0  2.5697   2.5528 2.5865
# 7 == 0  2.6670   2.6532 2.6808
# 10 == 0 2.7643   2.7475 2.7812
# 13 == 0 2.8617   2.8378 2.8855

# 1, 4, 7, 10, 13 are row indices of model matrix

# Table 9.6, p. 671
# No data

# Table 9.7, p. 672
# anova(lm1)

# Table 9.8, p. 680
# table4_08 |>
#   summarize(.by = c(air, water),
#             stack_mean = mean(stack),
#             stack_var = var(stack)) |>
#   arrange(air, water)

# Table 9.9, p. 684
# lm2 <- lm(stack ~ air + water + I(air^2), data = table4_08)

# Table 9.10, p. 692
# No data

# Table 9.11, p. 693
# anova(lm2)

# Table 9.12, p. 694
# No data

# Table 9.13, p. 695
# anova(lm2)

# Any more in Ch. 9? -----------------------------------------------------

# Table 9.14, p. 700

# Table 9.15, p. 700

# Table 9.16, p. 706
# Table 9.17, p. 707
# Table 9.18, p. 707
# No data

# Table 9.19, p. 708
# Table 9.20, p. 709
# Table 9.21, p. 709
# subset of Table 4.11

# Table 9.22, p. 714
# subset of Table 8.8

# Table 9.23, p. 715
# No data

# Table 9.24, p. 716
# subset of Table 8.8

# Appendix A --------------------------------------------------------------

# Table A.2, p. 768

x <- c("4/12/81",  0, 66,
       "11/12/81", 1, 70,
       "3/22/82",  0, 69,
       "11/11/82", 0, 68,
       "4/4/83",   0, 67,
       "6/18/83",  0, 72,
       "8/30/83",  0, 73,
       "11/28/83", 0, 70,
       "2/3/84",   1, 57,
       "4/6/84",   1, 63,
       "8/30/84",  1, 70,
       "10/5/84",  0, 78,
       "11/8/84",  0, 67,
       "1/24/85",  2, 53,
       "4/12/85",  0, 67,
       "4/29/85",  0, 75,
       "6/17/85",  0, 70,
       "7/29/85",  0, 81,
       "8/27/85",  0, 76,
       "10/3/85",  0, 79,
       "10/30/85", 2, 75,
       "11/26/85", 0, 76,
       "1/12/86",  1, 58)

table_a_2 <- matrix(x, nrow = 23, byrow = TRUE) |>
  data.frame() |>
  setNames(c("flight_date", "o_ring", "tempF")) |>
  mutate(flight_date = as.Date(flight_date, "%m/%d/%y"),
         o_ring = as.numeric(o_ring),
         tempF = as.numeric(tempF))
rm(x)
# usethis::use_data(table_a_2, overwrite = TRUE)

# o_rings <- table_a_2 |>
#   mutate(any_failure = cut(o_ring, c(0, 1, Inf),
#                            include.lowest = TRUE, right = FALSE,
#                            labels = c("None", "One or more"))
#   )
#
# log_model <- glm(any_failure ~ tempF, data = o_rings, family = "binomial")
# predict(log_model, newdata = data.frame(tempF = 31), type = "response")

# Table A.4. p. 778
table_a_4 <- data.frame(
  failure = c(0.073, 0.098, 0.117, 0.135, 0.175, 0.262, 0.270, 0.350,
              0.386, 0.456))
# usethis::use_data(table_a_4)

# Table A.5, p. 780
# two largest values are censored
table_a_5 <- data.frame(
  breakdown = c(50, 134, 187, 882, 1450, 1470, 2290, 2930, 4180, 15800,
                29200, 86100))
# usethis::use_data(table_a_5, overwrite = TRUE)

# Remove sections where data appears elsewhere ----------------------------

# rename Sec 2.2, Exercise 2 to Table 1.1, where data first appears
table1_01 <- ch2_sec2_prob2
rm(ch2_sec2_prob2)

# remove Section 3.2, Exercise 2; Section 3.2, Exercise 3;
#   Section 3.4, Exercise 2;   data appear elsewhere
rm(ch3_sec2_prob2, ch3_sec2_prob3, ch3_sec4_prob2)

# remove Section 4.1, Exercise 1; data repeated in Exercise 2
rm(ch4_sec1_prob1)

# remove Section 4.2, Exercise 1; data repeated from Section 4.1, Exercise 3
rm(ch4_sec2_prob1)

# remove Section 4.3, Exercise 1; data repeated from Section 4.2, Exercise 2
rm(ch4_sec3_prob1)

# remove Chapter 5, Exercise 22; date repeated from Chapter 3, Exercise 6
rm(ch5_eoc_prob22)

# remove Section 6.1, Exercise 2; data already in Chapter 3, Exercise 2
rm(ch3_eoc_prob02)

# remove Section 6.2, Exercises 1, 4
# Exercise 1 first appears as Chapter 3, Exercise 2
# Exercise 4 first appears as Section 6.1, Exercise 4
rm(ch6_sec2_prob01, ch6_sec2_prob04)

# remove Section 6.3, Exercise 2
# first appears as Section 3.1, Exercise 3
rm(ch6_sec3_prob02)

# remove Section 6.3, Exercise 3
# first appears as Section 6.1, Exercise 4
rm(ch6_sec3_prob03)

# remove Section 6.4, Exercise 1
# first appears as Section 6.3, Exercise 4
rm(ch6_sec3_prob04)

# remove Section 6.4, Exercise 2
# first appears as Section 3.1, Exercise 3
rm(ch6_sec4_prob02)

# remove Section 6.5, Exercise 1
# first appears as Table 3.20
rm(ch6_sec5_prob1)

# remove Section 6.6, Exercise 2
# first appears as Table 6.7
rm(ch6_sec6_prob02)

# remove Chapter 6, Exercise 1; first appears as Table 3.6
rm(ch6_eoc_prob01)

# remove Chapter 6, Exercise 2; first appears as Table 1.1
rm(ch6_eoc_prob02)

# modify Chapter 6, Exercise 11; remove column of logs
ch6_eoc_prob11 <- select(ch6_eoc_prob11, y)
usethis::use_data(ch6_eoc_prob11, overwrite = TRUE)

# modify Chapter 6, Exercise 13; make names syntactic
names(ch6_eoc_prob13) <- c("octane_87", "octane_90")
usethis::use_data(ch6_eoc_prob13, overwrite = TRUE)

# modify Chapter 6, Exercise 15; make names syntactic
names(ch6_eoc_prob15) <- c("wire", "C0.0", "C21.8")
usethis::use_data(ch6_eoc_prob15, overwrite = TRUE)

# remove Chapter 6, Exercise 18; full data not needed
rm(ch6_eoc_prob18)

# modify Chapter 6, Exercise 26; make names syntactic
names(ch6_eoc_prob26) <- c("length_25cm", "length_30cm")
usethis::use_data(ch6_eoc_prob26, overwrite = TRUE)

# remove Chapter 6, Exercise 32; data reused from Chapter 3, Exercise 18
rm(ch6_eoc_prob32)

# remove Chapter 6, Exercise 33; data reused from Chapter 3, Exercise 19
rm(ch6_eoc_prob33)

# remove Chapter 6, Exercise 34; data reused from Chapter 3, Exercise 20
rm(ch6_eoc_prob34)

# remove Section 7.1, Exercise 1; data reused from Table 4.1
rm(ch7_sec1_prob1)

# remove Section 7.2, Exercise 1; data reused from Table 4.1
rm(ch7_sec2_prob1)

# remove Section 7.2, Exercise 2; data reused from Section 7.1, Exercise 2
rm(ch7_sec2_prob2)

# remove Section 7.3, Exercise 1; data reused from Table 4.1
rm(ch7_sec3_prob1)

# remove Section 7.3, Exercise 2; data reused from Section 7.1, Exercise 2
rm(ch7_sec3_prob2)

# remove Section 7.4, Exercise 1; data reused from Table 4.1
rm(ch7_sec4_prob1)

# remove Section 7.4, Exercise 2; data reused from Section 7.1, Exercise 2
rm(ch7_sec4_prob2)

# rename ch7_sec5_prob2a to ch7_sec5_prob2; remove ch7_sec5_prob2b
ch7_sec5_prob2 <- ch7_sec5_prob2a
rm(ch7_sec5_prob2a, ch7_sec5_prob2b)
usethis::use_data(ch7_sec5_prob2)

# correct names in Section 7.6, Exercise 1
names(ch7_sec6_prob1) <- c("Sample", "Defects")
usethis::use_data(ch7_sec6_prob1, overwrite = TRUE)

# correct names in Section 7.6, Exercise 2
names(ch7_sec6_prob2) <- c("Date", "Tested", "Leaks")
usethis::use_data(ch7_sec6_prob2, overwrite = TRUE)

# correct names in Chapter 7, Exercise 1
names(ch7_eoc_prob01) <- c("Lead_4H", "Lead_H", "Lead_B")
usethis::use_data(ch7_eoc_prob01, overwrite = TRUE)

# correct names in Chapter 7, Exercise 15
names(ch7_eoc_prob15) <- c("Tank", "Nonconf")
usethis::use_data(ch7_eoc_prob15, overwrite = TRUE)

# correct names in Chapter 7, Exercise 17
names(ch7_eoc_prob17) <- c("Sample", "Nonconf")
usethis::use_data(ch7_eoc_prob17, overwrite = TRUE)

# Chapter 7, Exercise 19
# combine ch7_eoc_prob19a and ch7_eoc_prob19a into ch7_eoc_prob19
ch7_eoc_prob19 <- ch7_eoc_prob19a
rm(ch7_eoc_prob19a, ch7_eoc_prob19b)
usethis::use_data(ch7_eoc_prob19, overwrite = TRUE)

# remove Chapter 8, Exercise 1; data appeared in Chapter 4, Exercise 4
rm(ch8_eoc_prob01)

# remove Chapter 8, Exercise 2; data appeared in Chapter 4, Exercise 5
rm(ch8_eoc_prob02)

# remove Table 9.2; data derived from Table 4.1
rm(table9_02)

# remove Section 8.2, Exercise 1; data appeared in Section 4.3, Exercise 2
rm(ch8_sec2_prob1)

# remove Section 8.2, Exercise 2; data appeared in Chapter 4, Exercise 9
rm(ch8_sec2_prob2)

# remove Section 8.3, Exercise 3; data appeared in Section 8.2, Exercise 3
rm(ch8_sec3_prob3)

# remove Chapter 8, Exercise 9; data appeared in Chapter 4, Exercise 9
rm(ch8_eoc_prob09)

# remove Chapter 8, Exercise 13; data appeared in Chapter 4, Exercise 20
rm(ch8_eoc_prob13)

# remove Section 9.1, Exercise 1; data appeared in Section 4.1, Exercise 3
rm(ch9_sec1_prob1)

# remove Section 9.1, Exercise 2; data appeared in Chapter 4, Exercise 1
rm(ch9_sec1_prob2)

# remove Section 9.2, Exercise 1; data appeared in Chapter 4, Exercise 2
rm(ch9_sec2_prob1)

# remove Section 9.2, Exercise 2; data appeared in Section 4.2, Exercise 2
rm(ch9_sec2_prob2)

# remove Section 9.3, Exercise 2; data appeared in Chapter 8, Exercise 10
rm(ch9_sec3_prob2)

# remove Chapter 9, Exercise 1; data appeared in Chapter 4, Exercise 3
rm(ch9_eoc_prob01)

# remove Chapter 9, Exercise 4b; data appeared in Chapter 4, Exercise 1
rm(ch9_eoc_prob04b)

# remove Chapter 9, Exercise 4c; data appeared in Section 9.3, Exercise 1
rm(ch9_eoc_prob04c)

# remove Chapter 9, Exercise 4d; data appeared in Chapter 9, Exercise 3
rm(ch9_eoc_prob04c)

# remove Chapter 9, Exercise 5; data appeared in Chapter 4, Exercise 18
rm(ch9_eoc_prob05)

# remove Chapter 9, Exercise 6; data appeared in Chapter 4, Exercise 16
rm(ch9_eoc_prob06)

# Save workspace ----------------------------------------------------------

for(i in 1:length(ls())) {
  save(list = (ls()[i]),
       file = file.path(here::here(),
                        "data",
                        paste0(ls()[i], ".rda"))
  )
  cat("Saved", ls()[i], "\n")
  }

file.remove(here::here("data", "i.rda"))
file.remove(here::here("data", "DATASET.R.rda"))

# for some reason these two weren't saved; save manually
usethis::use_data(table7_19)
usethis::use_data(table9_02)

# save("table7_19", file = file.path(here::here("data/table7_19.rda")))
# save("table9_02", file = file.path(here::here("data/table9_02.rda")))

rm(list = ls())

# ls() |> length()
# save.image(here("Vardeman/BECDA.Rdata"))

# save all data objects as separate .rda files
# save_data <- function(x)
#   save(list = x,
#        file = file.path(here::here("rdata"),
#                         paste0(x, (".rda"))))
#
# purrr::walk(ls(), save_data, .progress = TRUE)

# NEED TO FIX
# # usethis::use_data(DATASET, overwrite = TRUE)

# DIDN'T WORK
# purrr::walk(ls(x),
#             # usethis::use_data(get(x), overwrite = TRUE)
#             )

# https://forum.posit.co/t/saving-external-data-in-a-loop-with-# usethis-use-data/34021/2
# https://stackoverflow.com/questions/49673667/how-to-use-devtoolsuse-data-on-a-list-of-data-frames/49676445#49676445

# walk2(ls(), names(ls()), function(obj, name) {
#   assign(name, obj)
#   do.call("use_data", list(as.name(name), overwrite = TRUE))
#   })

# walk2(my.list, names(my.list), function(obj, name) {
#   assign(name, obj)
#   do.call("use_data", list(as.name(name), overwrite = TRUE))
#   })
