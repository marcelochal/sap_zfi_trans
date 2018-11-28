*&---------------------------------------------------------------------*
*&  Include           MZFI0003I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_EXIT INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0001 INPUT.

  " Limpa o evento contra possiveis sujeira PAI
  CLIQUE_OK = OK_CODE_0001.
  CLEAR OK_CODE_0001.


  case clique_ok.
    when 'SALVAR'.
*       perform F_SAVE_VARIANT.
    when OTHERS.
  endcase.

ENDMODULE.

*&SPWIZARD: INPUT MODULE FOR TS 'FOLDER_001'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE FOLDER_001_ACTIVE_TAB_GET INPUT.
  OK_CODE_0001 = SY-UCOMM.
  CASE OK_CODE_0001.
    WHEN C_FOLDER_001-TAB1.
      G_FOLDER_001-PRESSED_TAB = C_FOLDER_001-TAB1.
    WHEN C_FOLDER_001-TAB2.
      G_FOLDER_001-PRESSED_TAB = C_FOLDER_001-TAB2.
    WHEN C_FOLDER_001-TAB3.
      G_FOLDER_001-PRESSED_TAB = C_FOLDER_001-TAB3.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0110 INPUT.


  " Limpa o evento contra possiveis sujeira PAI
  CLIQUE_OK = OK_CODE_0001.
  CLEAR OK_CODE_0001.



  case clique_ok.
    when 'LANC'. PERFORM : f_lancamento_enc. "•	Lançamento do encerramento
    when 'PROC'. PERFORM : f_select_itens_proc. "Visualização Execuções
    when 'FBRA'. PERFORM : f_estorna_documento. "Estorno de Lançamento
    when OTHERS.
  endcase.


ENDMODULE.
