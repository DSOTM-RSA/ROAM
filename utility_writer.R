
utility_writer <- function(list_df,dirout){
      # wrapper function to write a list of data.frames
      # takes the list, and directory ("chr") as arguements
      
      lapply(names(list_df),
             function(x, list_df) write.table(list_df[[x]], paste(dirout,x, ".tsv", sep = ""),
                                              col.names=NA, row.names=TRUE, sep="\t", 
                                              quote=FALSE),
             list_df)
}
