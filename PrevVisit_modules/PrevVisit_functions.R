
#-------------------------------------- 
# Functions for PrevVisit reports 
#--------------------------------------

#----- Source lines within a script, rather than whole script -----
source_lines <- function(file, start, end) {
  file.lines <- scan(file, what = character(), 
                     skip = start - 1, nlines = end - start + 1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed))
}

