

library(dplyr)
library(tidyr)
# library(optparse)
# 
# option_list <- list(
#   make_option(c("-i", "--input"), type = "character", default = NULL,
#               help = "Input file name", metavar = "FILE"),
#   make_option(c("-o", "--output"), type = "character", default = "output.txt",
#               help = "Output file name", metavar = "FILE")
# )
# 
# opt_parser <- OptionParser(option_list = option_list)
# opts <- parse_args(opt_parser)
# 
# if (is.null(opts$input)) {
#   print_help(opt_parser)
#   stop("Input file must be specified", call. = FALSE)
# }




# Чтение таблицы из файла "n10-results.txt"
df <- read.table("input.txt", header = FALSE, stringsAsFactors = FALSE)

# Задаем имена столбцов
colnames(df) <- c("query", "subject", "identity", "length", "mm", "gap", "qstart",
                  "qend", "sstart", "send", "e", "score")

# Фильтруем данные
df <- df[df$identity > 85 & df$e == 0,]

# 1. Создаем столбец info согласно шаблону
df <- df %>%
  mutate(info = paste0("identity=", identity,
                       ";length=", length,
                       ";evalue=", e,
                       ";pos=", sstart, "-", send))

# 2. Преобразуем данные в широкий формат:
#    строки идентифицируются по subject, столбцы – уникальные значения query.
#    Аргумент values_fill заполняет отсутствующие значения пустой строкой.
new_df <- df %>%
  select(subject, query, info) %>%
  pivot_wider(
    names_from = query,
    values_from = info,
    values_fn = list(info = function(x) paste(x, collapse = ",")),
    values_fill = list(info = "")
  )

# Если для одной комбинации subject и query оказалось несколько значений,
# объединяем их через ";".
new_df[] <- lapply(new_df, function(col) {
  if (is.list(col)) {
    sapply(col, function(x) {
      if (is.null(x) || (length(x) == 1 && is.na(x))) "" else paste(x, collapse = ",")
    })
  } else {
    ifelse(is.na(col), "", col)
  }
})

# 3. Записываем результат в файл формата TSV
write.table(new_df, file = "out.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
