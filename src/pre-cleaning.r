# special case for processing the ma 20 and 40 data

# Processing raw data from Kamiar Mohaddes

# Data columns naming convention

# e.g. Delta_20_26_16
# e.g. Delta_40_85_86

# MA 20 with RCP 2.6 and h=16 (year 2030)
# MA 40 with RCP 8.5 and h=86 (year 2100)

# h=86 is year 2100


df1 <- read.csv("rawdata/GDPLosses.csv", header=TRUE)

df_20_26 <- df1 %>% select(c(iso, cntry_name, contains("Delta_20_26_")))
df_20_85 <- df1 %>% select(c(iso, cntry_name, contains("Delta_20_85_")))
df_30_26 <- df1 %>% select(c(iso, cntry_name, contains("Delta_30_26_")))
df_30_85 <- df1 %>% select(c(iso, cntry_name, contains("Delta_30_85_")))
df_40_26 <- df1 %>% select(c(iso, cntry_name, contains("Delta_40_26_")))
df_40_85 <- df1 %>% select(c(iso, cntry_name, contains("Delta_40_85_")))

dynamicColumnRename <- function(df){
    df_name <- deparse(substitute(df))
    ma <- substr(df_name, 4, 5) 
    rcp <- substr(df_name, 7, 8) 
    h_range = seq(1:100)
    for (i in h_range){
    names(df)[names(df) == paste0("Delta_",ma,"_",rcp,"_", i)] <- paste0(i)
    }
    return(df)
    }

df_20_26 <- dynamicColumnRename(df_20_26)
df_20_85 <- dynamicColumnRename(df_20_85)
df_30_26 <- dynamicColumnRename(df_30_26)
df_30_85 <- dynamicColumnRename(df_30_85)
df_40_26 <- dynamicColumnRename(df_40_26)
df_40_85 <- dynamicColumnRename(df_40_85)

rescaleColumns <- function(df){
    iso <- df$iso
    country <- df$cntry_name
    df <- df[,3:length(df)]*-1/100
    df<-df[, order(as.numeric(names(df)))]
    df <- cbind(country,df)
    df <- cbind(iso,df)
    return (df)
}

df_20_26 <- rescaleColumns(df_20_26)
df_20_85 <- rescaleColumns(df_20_85)
df_30_26 <- rescaleColumns(df_30_26)
df_30_85 <- rescaleColumns(df_30_85)
df_40_26 <- rescaleColumns(df_40_26)
df_40_85 <- rescaleColumns(df_40_85)

write.csv(df_20_26, "rawdata/ma_20_26.csv", row.names=FALSE)
write.csv(df_20_85, "rawdata/ma_20_85.csv", row.names=FALSE)
write.csv(df_30_26, "rawdata/ma_30_26.csv", row.names=FALSE)
write.csv(df_30_85, "rawdata/ma_30_85.csv", row.names=FALSE)
write.csv(df_40_26, "rawdata/ma_40_26.csv", row.names=FALSE)
write.csv(df_40_85, "rawdata/ma_40_85.csv", row.names=FALSE)


