.onLoad <- function(lib,pkg)
{
	pkg_info = utils::sessionInfo( package="PBStools" )$otherPkgs$PBStools
	if( is.character( pkg_info$Packaged ) )
		pkg_date <- strsplit( pkg_info$Packaged, " " )[[1]][1]
	else
		pkg_date  <- date()
	
	userguide_path <- system.file( "doc/PBStools-UG.pdf", package = "PBStools" )
	
	packageStartupMessage("
-----------------------------------------------------------
PBS Tools ", pkg_info$Version, " -- Copyright (C) 2007-2012 Fisheries and Oceans Canada

A complete user guide 'PBStools-UG.pdf' is located at 
", userguide_path, "

Packaged on ", pkg_date, "
Pacific Biological Station, Nanaimo

We may not be big, but we're small.
-----------------------------------------------------------

")
}
