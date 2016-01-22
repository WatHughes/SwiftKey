# library(rdrop2) # Dropbox wrapper

# Some cleanup is needed.
# For now this needs globals:
# CompressedRDataDir
# DropBoxCompressedRDataDir

LoadDataFile = function(FileName) # First downloads the file unless it is already local
{
    Note=''
    StartTime = proc.time()
    CompressedRDataPath = paste0(CompressedRDataDir,'/',FileName,'.rda')
    DropBoxCompressedRDataPath = paste0(DropBoxCompressedRDataDir,'/',FileName,'.rda')
    if (!file.exists(CompressedRDataPath))
    {
        if (drop_exists(DropBoxCompressedRDataPath))
        {
            # This is the case where we don't have the file locally as required by load()
            # but it is on DropBox. Download the file so we can use it locally.
            drop_get(DropBoxCompressedRDataPath, CompressedRDataPath)
            Note=paste0(Note,'Dowloaded RDA from DropBox. ')
        }
    }
    if (file.exists(CompressedRDataPath))
    {
        load(CompressedRDataPath,.GlobalEnv) # Load it in the global environment. The RDA file was created to contain 1 data.table with the name indicated by FileName.
        Note = paste0(Note,'Loaded compressed object ',FileName,'.')
    }
    else
    {
        Note = paste0(Note,'Failed to find ',FileName,'.rda.')
    }

    LoadTime = proc.time()
    LoadTime = LoadTime - StartTime
    print(Note)
    print('Data load took:')
    print(LoadTime)
    get(FileName)
} # LoadDataFile

# If a variable exists with the name in FileName, return it. Otherwise,
# load it using LoadDataFile.

CondLoadDataTable = function(FileName)
{
    mget(FileName,ifnotfound=list(LoadDataFile),inherits=T)[[1]]
    # <sigh> get0 was running LoadDataFile even when FileName was found!
} # CondLoadDataTable
