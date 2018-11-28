*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGRFI_TRANS
*   generation date: 08.01.2016 at 10:42:44
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGRFI_TRANS        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
