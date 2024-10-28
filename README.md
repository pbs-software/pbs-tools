## <font color="#6A5ACD">PBStools: Tools for the efficient execution of onerous tasks</font> ##

<font color="red">&copy; Fisheries and Oceans Canada (2007-2024)</font>

**PBStools** provides an R interface for algorithms commonly used in fisheries. The scope of this package is by no means comprehensive, having grown from the need to satisfy tasks specific to British Columbia (BC) ocean fisheries. Many of the functions provide a quick way to visualize data, and in some cases perform preliminary analyses. Though oriented to users at the <a href="http://www.pac.dfo-mpo.gc.ca/science/facilities-installations/index-eng.html#pbs">Pacific Biological Station</a> (PBS), these functions may provide a broad utility to users at other locales. The R code modules and User Guide and are organised into sections that loosely classify the functions by theme -- (1) Utility, (2) Biology, (3) Fishery, (4) Survey, (5) Spatial, (6) Assessment, (7) Catch Reconstruction, (8) LaTeX tools, and (9) PJS<sup>1</sup> survey index functions. Within each section, the functions are described alphabetically.

**PBStools** depends heavily on two other R packages: **PBSmapping** and **PBSmodelling**. We use the latter to implement several Graphical User Interfaces (GUIs) that facilitate a few select functions. Most functions, however, are designed for use on the command line or in sourced code. Windows users need to be mindful that the R-statistical language is case sensitive. 

Also available in the package directory `./library/PBStools/sql` we provide a number of useful SQL queries for DFO (Department of Fisheries and Oceans, aka Fisheries and Oceans Canada) commercial fisheries databases -- `PacHarvest` (trawl, 1996-2007), `PacHarvHL` (hook and line, 1994-2006), `GFCatch` (historical landings, 1954-1995), `GFBioSQL` (biological samples, 1946-2018), and `GFFOS` (integrated fisheries, 2002-2018). To launch SQL queries, **PBStools** relies on the R package **RODBC**. If you have access to the DFO network and have privileges to query these databases, the function `getData` can send the queries to the remote servers and return a data frame called `PBSdat` in the global environment (or any user-specified environment). In the document, we highlight queries for DFO personnel using text with a background shaded '<span style="background-color:  #FFE4B5">moccasin</span>'. (Examples are shaded '<span style="background-color:  #F0F8FF">aliceblue</span>' and console output is shaded '<span style="background-color:  #F0FFF0">honeydew</span>'.) Note that many of these queries might act as useful templates for users outside DFO for similar purposes. Querying databases directly via SQL commands from R usually proves much more efficient than launching Microsoft Access queries from a front-end database shell. 

Originally, functions in **PBStools** evolved over time (2007-2012) within the R package **PBSfishery**, along with a convenient GUI tool for interacting with **PBSmapping** and useful datasets (regional boundaries, key codes, example data). In April 2012, we decided to split **PBSfishery** into three separate libraries -- **PBStools**, **PBSmapx**, and **PBSdata** -- for public distribution (see <a href="https://github.com/pbs-software">pbs-software</a>). The three packages experience different rates of change, with **PBStools** undergoing frequent revision, while **PBSdata** and **PBSmapx** can remain unchanged for long periods of time.

Although **PBStools** is not available on <a href="https://cran.r-project.org/">CRAN</a> (Comprehensive R Archive Network), the package (Windows binary and source tarball) is built after using CRAN's rigorous `R CMD check --as-cran` routine (using R-devel on a **Windows 7** 64-bit system) and posted to <a href="https://drive.google.com/drive/folders/0B2Bkic2Qu5LGOGx1WkRySVYxNFU?usp=sharing">Google Drive</a>. Most of the time, the revision on <a href="https://github.com/pbs-software/pbs-tools">GitHub</a> can be built in R using `devtools::install_github("pbs-software/pbs-tools/PBStools")`; however, not every revision has been checked for CRAN worthiness.

In July 2018, the package was updated to include a new function called `linguaFranca`, which translates a limited set of BC stock assessment english words that appear in figures into french. This feature was largely driven by <a href="https://web.archive.org/web/20190926104614/http://www.dfo-mpo.gc.ca/csas-sccs/process-processus/translation-traduction-eng.html">CSAS' policy</a> to make all scientific research documents accessible to anglophone and francophone readers.

Influences by Wizard Wickham pervade the Rverse these days; however, reliance on the convoluted machinations of the overlord are avoided in PBS packages wherever possible. While 'ggthis' and 'ggthat' provide code shortcuts, the underlying operations are not transparent. Furthermore, connecting 'ggblobs' using <a href="https://www.fromthebottomoftheheap.net/2015/06/03/my-aversion-to-pipes/">pipes</a> simply makes the mess even more difficult to clean up. Resist assimilation and keep your R code pure. 

<font color="red"><h3>Disclaimer</h3></font>

"Fisheries and Oceans Canada (DFO) GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. DFO relinquishes control of the information and assumes no responsibility to protect the integrity, confidentiality, or availability of the information. Any claims against DFO stemming from the use of its GitHub project will be governed by all applicable Canadian Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favouring by DFO. The Fisheries and Oceans Canada seal and logo, or the seal and logo of a DFO bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DFO or the Canadian Government.”

As with any freely available product, there is no warranty or promise that **PBStools** will perform adequately for all circumstances. Additionally, coding errors are possible, and users should contact the package maintainer if bugs are detected.

Maintainer: <a href="mailto:rowan.haigh@dfo-mpo.gc.ca">Rowan Haigh</a>

<sup>1</sup>PJS = Paul J. Starr

<p align="right"><img src="DFOlogo_small.jpg" alt="DFO logo" style="height:30px;"></p> 

