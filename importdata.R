my_db <- src_mysql(
  dbname = "covid",
  host = "127.0.0.1",
  user = "root",
  password = "1234"
)
my_db

##import data
df_conf <- tbl(my_db, sql("select * from covid19_confirmed"))
df_conf <- as.data.frame(df_conf)
df_conf
df_deaths <- tbl(my_db, sql("select * from covid19_deaths"))
df_deaths <- as.data.frame(df_deaths)
df_deaths
df_recover <- tbl(my_db, sql("select * from covid19_recovered"))
df_recover <- as.data.frame(df_recover)
df_recover