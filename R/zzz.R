.onAttach<-function(...){
  if (!interactive() ) return()
packageStartupMessage("Welcome to EWStools for R version 0.1!",appendLF=TRUE)
packageStartupMessage("Developed by Jared E. Knowles 2013-2014", appendLF=TRUE)
packageStartupMessage("Copyright (C) 2014 the Wisconsin Department of Public Instruction", appendLF=TRUE)
packageStartupMessage("Distributed without warranty. View license.md for more details.", appendLF=TRUE)
}
