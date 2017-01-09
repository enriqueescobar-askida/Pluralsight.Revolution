setwd("E:/Users/Admin/Google Drive/MiApp/Pluralsight.Revolution/BigDataU.R101");
# code to download the dataset
download.file("https://ibm.box.com/shared/static/n5ay5qadfe7e1nnsv5s01oe1x62mq51j.csv", destfile="Data/movies-db.csv");
movies_Data <- read.csv("Data/movies-db.csv", header=TRUE, sep=",");
movies_Data <- tibble::as_data_frame(movies_Data);
myLines <- as.character(movies_Data$name);
toupper(myLines[1]);
tolower(myLines[1]);
chartr(" ", "-", myLines[1]);
char_list <- strsplit(myLines[1], " ");
word_list <- unlist(char_list);
sort(word_list);
substr(myLines[1], start = 2, stop = 10);
toyStory <- chartr(" ", "-", myLines[1]);
toyStory <- trimws(toyStory);
library(stringr);
str_sub(toyStory, -5, -1);
as.Date(as.character(movies_Data$year), "%Y");
as.Date("27/06/94", "%d/DM/%Y");
eMail <- "enrique@travobject.com" ; # .+@.+
grep("@.*", eMail);
gsub("@.*", "@newdomain.com", eMail);
myMatch <- regexpr("@.*", "@newdomain.com", eMail);
regmatches("julio@newdomain.com", myMatch);
# Download the data file
download.file("https://ibm.box.com/shared/static/l8v8g8e6uzk7yj2j1qc8ypezbhzukphy.txt",
              destfile="data/The_Artist.txt");
file.size("data/The_Artist.txt");
my_data <- readLines("data/The_Artist.txt");
my_data1 <- scan("data/The_Artist.txt", "");
chartr(" ", "-", my_data[1]);
character_list <- strsplit(my_data[1], " ");
word_list <- unlist(character_list);
word_list;
sub_string <- substr(my_data[1], start = 4, stop = 50);
sub_string;
trimws(sub_string);
library(stringr);
str_sub(my_data[1], -8, -1);
email_df <- read.csv("https://ibm.box.com/shared/static/cbim8daa5vjf5rf4rlz11330lvqbu7rk.csv");
email_df;
grep("@.+",  c("test@testing.com" , "not an email", "test2@testing.com"))
grep("@.+",  c("test@testing.com", "not an email", "test2@testing.com"), value=TRUE)
gsub("@.+", "@newdomain.com", c("test@testing.com", "not an email", "test2@testing.com"))
matches <- regexpr("@.*", c("test@testing.com", "not an email", "test2@testing.com"))
regmatches(c("test@testing.com", "not an email", "test2@testing.com"), matches)
matches <- regexpr("@.*\\.", email_df[,'Email'])
email_df[,'Domain'] = regmatcheemail_dfs(email_df[,'Email'], matches)
email_df
table(email_df[,'Domain'])



