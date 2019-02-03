# DB module

library(DBI)

writeDFToSQLite <- function(df, table, dbpath) {
    con <- dbConnect(RSQLite::SQLite(), dbname = dbpath, synchronous = NULL)
    # Add timeout so the connection doesn't die when retrying
    res <- dbSendQuery(con, "PRAGMA busy_timeout=600000;")
    dbClearResult(res)
    # https://www.sqlite.org/pragma.html#pragma_synchronous
    res <- dbSendQuery(con, "PRAGMA journal_mode = MEMORY;")#
    dbClearResult(res)

    on.exit(dbDisconnect(con))
    repeat {
        # Repeat until sucess or busy_timeout is out
        rv <- try(dbWriteTable(con, table, df, append = TRUE))
        if(!is(rv, "try-error")) break
        cat("[Database] Error on insertion. Retrying...\n")
        Sys.sleep(runif(1)) # Sleep 0 to 1 seconds
    }
    cat("[Database] Insert done\n")
    rv
}


writeToSQLite <- function(object, table, ...) {
    if (is.data.frame(object)) {
        cat("[Database] Writing object to SQLite DB\n")
        writeDFToSQLite(object, table, ...)
    } else if (is.list(object)) {
        for(elem in names(object)) {
            df <- object[[elem]]#, drop=F]]
            if (is.data.frame(df)) 
                writeDFToSQLite(df, table = elem, ...)
        }
    } else { 
        cat("[Database] Object class unknown. Not inserting it.\n")
    }
}

storeData <- function(df, storageType="", ...) {
    switch(storageType,
        SQLite = {
              writeToSQLite(df, ...)
        },
        { # If none selected, return the df as is (Identity function)
            cat("[Database] Returning the result in RAM\n")
            df 
        }
    )
}

deserializeObject <- function(obj) {
    unserialize(
        memDecompress(unlist(obj), type="gzip")
    )
}



createIndex <- function(db, table, column) {
    query <- paste("CREATE INDEX idx_", column, " ON ", table, "(", column,");"
                   , sep="")
    con <- dbConnect(RSQLite::SQLite(), dbname = db, synchronous = NULL)
    on.exit(dbDisconnect(con))
    res <- dbSendQuery(con, query)
    dbClearResult(res) 
}

## Test

#{
#    library(DBI)
#    filePath <- "/tmp/test.sqlite"
##    testTable <- "TestTable"
#    df <- iris
#
#    storeData(df, storageType="SQLite", table=testTable, dbpath=filePath)
#
#    library(dplyr)
#    db <- src_sqlite(filePath)
#    (tab <- tbl(db, testTable))
#    tab %>% count()
#}


#{
#    # Inserting BLOBs
#    library(DBI)
#    library(dplyr)
#    library(blob)
#    
#    dbPath <- "/tmp/dbBLOB.sqlite"    
#
#    db <- src_sqlite(dbPath, create=TRUE)
#    # Not needed. BLOB type is recognized.
#    #dbSendQuery(conn = db$con,
#    #   "CREATE TABLE test 
#    #   (somedata INTEGER,
#    #    raster BLOB)")
#
#    dbListTables(db$con)
#
#    ser <- as.blob(memCompress(serialize(iris, NULL, ascii=TRUE), type="gzip"))
#
#    data.frame(somedata=3, raster=ser) %>%
#        storeData(storageType="SQLite", table="test", dbpath=dbPath)
#
#    tab <- db %>% tbl("test") %>% collect
#
#    dec <- memDecompress(tab$raster[[1]], type="gzip")
#    df <- unserialize(dec)
#}
