*&---------------------------------------------------------------------*
*&  Include           MZFI0003O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS '0001'.
  SET TITLEBAR '0001'.
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TS 'FOLDER_001'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE folder_001_active_tab_set OUTPUT.
  folder_001-activetab = g_folder_001-pressed_tab.
  CASE g_folder_001-pressed_tab.
    WHEN c_folder_001-tab1.
      g_folder_001-subscreen = '0002'.
    WHEN c_folder_001-tab2.
      g_folder_001-subscreen = '0003'.
    WHEN c_folder_001-tab3.
      g_folder_001-subscreen = '0004'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INIT_LIST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_list OUTPUT.

  DATA: name    TYPE vrm_id,
        list    TYPE vrm_values,
        value   LIKE LINE OF list,
        t_dd07v TYPE TABLE OF dd07v,
        s_dd07v TYPE dd07v.

  FREE: list, it_ztfi0002, t_dd07v.

  wa_lancamento-execucao = 'X'.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDRGSUM'
    TABLES
      values_tab      = t_dd07v
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  LOOP AT t_dd07v INTO s_dd07v.
    value-key  = s_dd07v-domvalue_l.
    value-text = s_dd07v-ddtext .
    APPEND value TO list.
  ENDLOOP.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'gv_cpm_regra'
      values = list.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GERA_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gera_alv OUTPUT.


  DATA:  o_alv       TYPE REF TO cl_gui_alv_grid,
         o_container TYPE REF TO cl_gui_custom_container.


* Se o ALV ja foi instanciado...
  IF o_alv IS BOUND.
* Metodo da classe de alv atribuida ao objeto o_alv para limpar o objeto.
    o_alv->free( ).
  ENDIF.

* Se o Objeto não foi instaciando
  IF o_container IS NOT BOUND.
*  Faz a criação do objecto do container passando o nome do Custom control
* para o parametro de exportação.
    CREATE OBJECT o_container
      EXPORTING
        container_name = 'ALV_CUSTOM'.
  ENDIF.

*  Faz a criação do objeto do ALV passando o nome do Container que receberá
* o ALV
  CREATE OBJECT o_alv
    EXPORTING
      i_parent = o_container.

*  Alimenta a estrutura de Layout do alv.
  PERFORM f_layout_alv.

*  Exibe alv conforme seus parametros
  CALL METHOD o_alv->set_table_for_first_display
    EXPORTING
      i_structure_name = 'ZTFI0001'
      is_layout        = st_layout
    CHANGING
      it_outtab        = it_ztfi0001.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GERA_ALV_LOG  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gera_alv_log OUTPUT.

  DATA:  o_alv_log       TYPE REF TO cl_gui_alv_grid,
         o_container_log TYPE REF TO cl_gui_custom_container.


* Se o ALV ja foi instanciado...
  IF o_alv_log IS BOUND.
* Metodo da classe de alv atribuida ao objeto o_alv para limpar o objeto.
    o_alv_log->free( ).
  ENDIF.

* Se o Objeto não foi instaciando
  IF o_container_log IS NOT BOUND.
*  Faz a criação do objecto do container passando o nome do Custom control
* para o parametro de exportação.
    CREATE OBJECT o_container_log
      EXPORTING
        container_name = 'ALV_CUSTOM_LOG'.
  ENDIF.

*  Faz a criação do objeto do ALV passando o nome do Container que receberá
* o ALV
  CREATE OBJECT o_alv_log
    EXPORTING
      i_parent = o_container_log.

**  Alimenta a estrutura de Layout do alv.
  PERFORM f_layout_alv.
*
*  Exibe alv conforme seus parametros
  CALL METHOD o_alv_log->set_table_for_first_display
    EXPORTING
      i_structure_name = 'ZEFI0004'
      is_layout        = st_layout
    CHANGING
      it_outtab        = it_ztfi0004.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  SET PF-STATUS '0001'.
  IF pprinc IS INITIAL.
    pprinc = 'BRGA'.
  ENDIF.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GERA_ALV_SUM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gera_alv_sum OUTPUT.

  DATA:  o_alv_sum       TYPE REF TO cl_gui_alv_grid,
         o_container_sum TYPE REF TO cl_gui_custom_container.


* Se o ALV ja foi instanciado...
  IF o_alv_sum IS BOUND.
* Metodo da classe de alv atribuida ao objeto o_alv para limpar o objeto.
    o_alv_sum->free( ).
  ENDIF.

* Se o Objeto não foi instaciando
  IF o_container_sum IS NOT BOUND.
*  Faz a criação do objecto do container passando o nome do Custom control
* para o parametro de exportação.
    CREATE OBJECT o_container_sum
      EXPORTING
        container_name = 'ALV_CUSTOM_SUM'.
  ENDIF.

*  Faz a criação do objeto do ALV passando o nome do Container que receberá
* o ALV
  CREATE OBJECT o_alv_sum
    EXPORTING
      i_parent = o_container_sum.

**  Alimenta a estrutura de Layout do alv.
  PERFORM f_layout_alv.


  SORT it_zefi0001 BY bukrs
  gsber
  hkont
  gjahr
  vbund
  bewar
  kostl
  aufnr
  dmbtr.

  DELETE ADJACENT DUPLICATES FROM it_zefi0001 COMPARING bukrs
gsber
hkont
gjahr
vbund
bewar
kostl
aufnr
dmbtr.

*  Exibe alv conforme seus parametros
  CALL METHOD o_alv_sum->set_table_for_first_display
    EXPORTING
      i_structure_name = 'ZEFI0001'
      is_layout        = st_layout
    CHANGING
      it_outtab        = it_zefi0001.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GERA_ALV_LAN_LOG  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE gera_alv_lan_log OUTPUT.
  DATA:  o_alv_lan_log       TYPE REF TO cl_gui_alv_grid,
         o_container_lan_log TYPE REF TO cl_gui_custom_container.


* Se o ALV ja foi instanciado...
  IF o_alv_lan_log IS BOUND.
* Metodo da classe de alv atribuida ao objeto o_alv para limpar o objeto.
    o_alv_lan_log->free( ).
  ENDIF.

* Se o Objeto não foi instaciando
  IF o_container_lan_log IS NOT BOUND.
*  Faz a criação do objecto do container passando o nome do Custom control
* para o parametro de exportação.
    CREATE OBJECT o_container_lan_log
      EXPORTING
        container_name = 'ALV_CUSTOM_LAN_LOG'.
  ENDIF.

*  Faz a criação do objeto do ALV passando o nome do Container que receberá
* o ALV
  CREATE OBJECT o_alv_lan_log
    EXPORTING
      i_parent = o_container_lan_log.

**  Alimenta a estrutura de Layout do alv.
  PERFORM f_layout_alv.


  SORT it_zefi0005 BY linha tipo msg.

  DELETE ADJACENT DUPLICATES FROM it_zefi0005 COMPARING tipo msg .

*  Exibe alv conforme seus parametros
  CALL METHOD o_alv_lan_log->set_table_for_first_display
    EXPORTING
      i_structure_name = 'ZEFI0005'
      is_layout        = st_layout
    CHANGING
      it_outtab        = it_zefi0005.
ENDMODULE.
