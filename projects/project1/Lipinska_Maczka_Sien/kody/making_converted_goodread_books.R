# Sprawdzenie nazw plików ------------------------------------------------------
# wytworzonych przez program "convert_goodreads_books_to_csv.R"

pliki <- list.files()

# Wczytanie tychże ramek -------------------------------------------------------

books_1_250000 <- read.csv("books_1_250000.csv")
books_250001_750000 <- read.csv("books_250001_750000.csv")
books_750001_1000000 <- read.csv("books_750001_1e+06.csv")
books_1000000_1500000 <- read.csv("books_1e+06_1500000.csv")
books_1500000_1750000 <- read.csv("books_1500000_1750000.csv")
books_1750001_2000000 <- read.csv("books_1750001_2e+06.csv")
books_2000000_2150000 <- read.csv("books_2e-06_2150000.csv")
books_2150000_2360655 <- read.csv("books_2150000_2360655.csv")

# Ręczna forma sprawdzenia, czy przypadkiem pewne wiersze się nie powtarzają
# z uwagi, że były brane zgodnie z kolejnością, to problem może wystąpić aby na
# końcu poprzedniego pliku i początku następnego
# Zostawiam tu tylko pierwsze sprawdzenie

books_1000000_1500000[length(books_1000000_1500000),"book_id"]
books_1500000_1750000[1,"book_id"]

# wartości były różne - wszystko dobrze, nie ma p problemu zazębiających się przedziałów

# Połączenie ramek w jedną -----------------------------------------------------
data <- rbind(books_1_250000, books_250001_750000, books_750001_1000000, 
              books_1000000_1500000, books_1500000_1750000, books_1750001_2000000,
              books_2000000_2150000, books_2150000_2360655)


# Zapis ramki do pliku csv -----------------------------------------------------
write.csv(data_1, "converted_goodreads_books.csv")
