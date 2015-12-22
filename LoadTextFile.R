# LoadTextFile uses readLines on the indicated file assuming that it is UTF-8
# and put the resulting vector in the global environment as ObjectName.
# For convenience, return it. Optionaly report the load time, return a sample,
# and/or limit the number of rows read.

LoadTextFile = function(FileName,DataDir='.',ObjectName=FileName,
                        SampleSize=-1,MaxRowsToRead=-1,verbose=F)
{
    StartTime = proc.time()
    FN = paste0(DataDir,'/',FileName)
    con = file(FN,open='rb') # So that Control-Z doesn't stop readLines since that is valid UTF-8
    ret = readLines(con,n=MaxRowsToRead,encoding='UTF-8');
    close(con)
    LoadTime = proc.time()
    LoadTime = LoadTime - StartTime
    if (verbose == T)
    {
        nrows = length(ret)
        print(paste0(format(nrows,big.mark=','),' rows loaded in:'))
        print(LoadTime)
    }
    if (SampleSize > 0)
    {
        ret = sample(ret, SampleSize)
    }
    assign(ObjectName,
           ret,
           envir=.GlobalEnv)
    ret
} # LoadTextFile
