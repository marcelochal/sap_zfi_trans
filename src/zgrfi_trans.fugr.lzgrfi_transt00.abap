*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 08.01.2016 at 10:42:44
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTFI0001........................................*
DATA:  BEGIN OF STATUS_ZTFI0001                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI0001                      .
CONTROLS: TCTRL_ZTFI0001
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZTFI0002........................................*
DATA:  BEGIN OF STATUS_ZTFI0002                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI0002                      .
CONTROLS: TCTRL_ZTFI0002
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZTFI0003........................................*
DATA:  BEGIN OF STATUS_ZTFI0003                      .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTFI0003                      .
CONTROLS: TCTRL_ZTFI0003
            TYPE TABLEVIEW USING SCREEN '0005'.
*.........table declarations:.................................*
TABLES: *ZTFI0001                      .
TABLES: *ZTFI0002                      .
TABLES: *ZTFI0003                      .
TABLES: ZTFI0001                       .
TABLES: ZTFI0002                       .
TABLES: ZTFI0003                       .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
