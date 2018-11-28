*&---------------------------------------------------------------------*
*&  Include           MZFI0003TOP
*&---------------------------------------------------------------------*

TABLES: bsis.

TYPES: BEGIN OF ty_bsis_divisao,
         bukrs    TYPE bsis-bukrs, "- empresa
*         HKONT    type bsis-HKONT, "- conta
         gsber    TYPE bsis-gsber, "– Divisão ( concessão )
         dmbtr(16) TYPE p DECIMALS 2, "– Moeda Interna
       END OF ty_bsis_divisao.

DATA: it_bsis_divisao TYPE STANDARD TABLE OF ty_bsis_divisao,
      it_bsis_divisao_n2 TYPE STANDARD TABLE OF ty_bsis_divisao,
      wa_bsis_divisao TYPE   ty_bsis_divisao.

DATA: gv_gjahr(4)  TYPE c,
      gv_dmbtr(16) TYPE p DECIMALS 2.

DATA: it_zfi_mov_enc TYPE STANDARD TABLE OF zfi_mov_enc,
      wa_zfi_mov_enc TYPE zfi_mov_enc.

DATA : v_sign     TYPE i.

TYPES:
  BEGIN OF ty_glt0,
    bukrs TYPE glt0-bukrs,
    ryear TYPE glt0-ryear,
    racct TYPE glt0-racct,
    rbusa TYPE glt0-rbusa,
    rhlst TYPE glt0-hsl01,
  END OF ty_glt0.

DATA: tty_glt0    TYPE STANDARD TABLE OF ty_glt0,
      gt_glt0_sum LIKE tty_glt0,
      wa_glt0_sum TYPE ty_glt0.


DATA:   lt_return TYPE STANDARD TABLE OF bapiret2,
        wa_return TYPE  bapiret2.

DATA: it_ztfi0001_est TYPE STANDARD TABLE OF ztfi0001,
      wa_ztfi0001_est TYPE  ztfi0001.

DATA: it_ztfi0001_valida TYPE STANDARD TABLE OF ztfi0001,
      wa_ztfi0001_valida TYPE  ztfi0001.

DATA: item             TYPE bapiacgl09-itemno_acc,
      lv_sum_dmbtr(16) TYPE p DECIMALS 2, "bsis-dmbtr,
      credito          TYPE char1       VALUE 'H',
      debito           TYPE char1       VALUE 'S',
      l_obj_type       TYPE bapiache09-obj_type,
      l_obj_key        TYPE bapiache09-obj_key,
      l_obj_sys        TYPE bapiache09-obj_sys.

DATA: it_zefi0001 TYPE STANDARD TABLE OF zefi0001,
      wa_zefi0001 TYPE zefi0001,
      it_zefi0005 TYPE STANDARD TABLE OF zefi0005,
      wa_zefi0005 TYPE zefi0005.

DATA: gv_cpm_regra(70) TYPE c,
      gv_resultado(16) TYPE p DECIMALS 2. "like bsis-DMBTR.

DATA: it_ztfi0002 TYPE STANDARD TABLE OF ztfi0002,
      wa_ztfi0002 TYPE ztfi0002.

DATA: it_ztfi0003 TYPE STANDARD TABLE OF ztfi0003,
      wa_ztfi0003 TYPE ztfi0003.

TYPES: BEGIN OF ty_t001,
         bukrs LIKE t001-bukrs,
         waers LIKE t001-waers,
         periv LIKE t001-periv,
         xgsbe LIKE t001-xgsbe,
       END OF ty_t001.

DATA: it_t001 TYPE STANDARD TABLE OF  ty_t001,
      wa_t001 TYPE ty_t001.


TYPES: BEGIN OF ty_bsis_saida,
         lv_campo_chave(30) TYPE c,
         bukrs              TYPE bsis-bukrs, "- empresa
         hkont              TYPE bsis-hkont, "- conta
         gsber              TYPE bsis-gsber, "– Divisão ( concessão )
         dmbtr(16)          TYPE p DECIMALS 2, "– Moeda Interna
         gjahr              TYPE bsis-gjahr, "- exercício
         belnr              TYPE bsis-belnr, "– documento contábil
         buzei              TYPE bsis-buzei, "- item
         bschl              TYPE bsis-bschl, "– chave de lançamento
         shkzg              TYPE bsis-shkzg, "- Sinal
         bstat              TYPE bsis-bstat, "– status do documento
         monat              TYPE bsis-monat, "– período contábil
         blart              TYPE bsis-blart, "– tipo de documento
         budat              TYPE bsis-budat, "– data do documento
         bldat              TYPE bsis-bldat, "– Data de Lançamento

         vbund              TYPE bsis-vbund, "– Sociedade Parceira
         bewar              TYPE bsis-bewar, "– Tipo de Movimento
         kostl              TYPE bsis-kostl, "– Centro de Custo
         aufnr              TYPE bsis-aufnr, "– Ordem Interna

         buzet(1)           TYPE c,       "-Lançamento individual
         sgtxt              TYPE bsis-sgtxt,
       END OF ty_bsis_saida.

TYPES: BEGIN OF ty_bseg,
         bukrs LIKE bseg-bukrs,
         belnr LIKE bseg-belnr ,
         gjahr LIKE bseg-gjahr,
         buzei LIKE bseg-buzei,
         hkont LIKE bseg-hkont,
         kstar LIKE bseg-kstar ,
 END OF ty_bseg.

TYPES: BEGIN OF ty_cskb ,
          kokrs LIKE cskb-kokrs,
          kstar LIKE cskb-kstar,
          katyp LIKE cskb-katyp,
 END OF ty_cskb .

TYPES: BEGIN OF ty_bsis,
         bukrs    TYPE bsis-bukrs, "- empresa
         hkont    TYPE bsis-hkont, "- conta
         gjahr    TYPE bsis-gjahr, "- exercício
         belnr    TYPE bsis-belnr, "– documento contábil
         buzei    TYPE bsis-buzei, "- item
         bschl    TYPE bsis-bschl, "– chave de lançamento
         shkzg    TYPE bsis-shkzg, "- Sinal
         bstat    TYPE bsis-bstat, "– status do documento
         monat    TYPE bsis-monat, "– período contábil
         blart    TYPE bsis-blart, "– tipo de documento
         budat    TYPE bsis-budat, "– data do documento
         bldat    TYPE bsis-bldat, "– Data de Lançamento
         gsber    TYPE bsis-gsber, "– Divisão ( concessão )
         vbund    TYPE bsis-vbund, "– Sociedade Parceira
         bewar    TYPE bsis-bewar, "– Tipo de Movimento
         kostl    TYPE bsis-kostl, "– Centro de Custo
         aufnr    TYPE bsis-aufnr, "– Ordem Interna
         dmbtr    TYPE bsis-dmbtr, "– Moeda Interna
         buzet(1) TYPE c,       "-Lançamento individual
         sgtxt    TYPE bsis-sgtxt,
       END OF ty_bsis.

TYPES: BEGIN OF ty_bsis_contabil,
         bukrs     TYPE bsis-bukrs, "- empresa
         hkont     TYPE bsis-hkont, "- conta
         gjahr(4)  TYPE c, "- exercício
         dmbtr(16) TYPE p DECIMALS 2, "– Moeda Interna
       END OF ty_bsis_contabil.

DATA: it_bsis_contabil TYPE STANDARD TABLE OF ty_bsis_contabil,
      wa_bsis_contabil TYPE ty_bsis_contabil.

DATA: it_bsis           TYPE STANDARD TABLE OF ty_bsis,
      it_cskb           TYPE STANDARD TABLE OF ty_cskb,
      it_bseg           TYPE STANDARD TABLE OF ty_bseg,
      it_bsis_nivel2    TYPE STANDARD TABLE OF ty_bsis,
      it_bsis_saida     TYPE STANDARD TABLE OF ty_bsis_saida,
      wa_bsis           TYPE ty_bsis,
      wa_bsis_nivel2    TYPE ty_bsis,
      wa_cskb           TYPE ty_cskb,
      wa_bseg           TYPE ty_bseg,
      wa_bsis_saida     TYPE ty_bsis_saida,
      wa_bsis_saida_aux TYPE ty_bsis_saida.

FIELD-SYMBOLS: <it_bsis_saida> TYPE ty_bsis_saida.

TYPES: BEGIN OF ty_bsis_chv1,
         bukrs     TYPE bsis-bukrs, "- empresa
         hkont     TYPE bsis-hkont, "- conta
         kostl     TYPE bsis-kostl, "– Centro de Custo
         aufnr     TYPE bsis-aufnr, "– Ordem Interna
         dmbtr(16) TYPE p DECIMALS 2, "bsis-DMBTR, "– Moeda Interna
       END OF ty_bsis_chv1.

DATA: it_bsis_chv1 TYPE STANDARD TABLE OF ty_bsis_chv1, "BUKRS+HKONT+KOSTL+AUFNR
wa_bsis_chv1 TYPE ty_bsis_chv1.


TYPES: BEGIN OF ty_bsis_chv2,
         bukrs     TYPE bsis-bukrs, "- empresa
         hkont     TYPE bsis-hkont, "- conta
         gsber     TYPE bsis-gsber, "– Divisão ( concessão )
         kostl     TYPE bsis-kostl, "– Centro de Custo
         aufnr     TYPE bsis-aufnr, "– Ordem Interna
         dmbtr(16) TYPE p DECIMALS 2, "– Moeda Interna
       END OF ty_bsis_chv2.

DATA: it_bsis_chv2     TYPE STANDARD TABLE OF ty_bsis_chv2, "BUKRS+HKONT+GSBER+KOSTL+AUFNR
wa_bsis_chv2     TYPE ty_bsis_chv2,
it_bsis_chv2_aux TYPE STANDARD TABLE OF ty_bsis_chv2. "BUKRS+HKONT+GSBER+KOSTL+AUFNR.

TYPES: BEGIN OF ty_bsis_chv3,
         bukrs     TYPE bsis-bukrs, "- empresa
         hkont     TYPE bsis-hkont, "- conta
         kostl     TYPE bsis-kostl, "– Centro de Custo
         aufnr     TYPE bsis-aufnr, "– Ordem Interna
         vbund     TYPE bsis-vbund, "– Sociedade Parceira
         bewar     TYPE bsis-bewar, "– Tipo de Movimento
         dmbtr(16) TYPE p DECIMALS 2, "– Moeda Interna
       END OF ty_bsis_chv3.

DATA: it_bsis_chv3 TYPE STANDARD TABLE OF ty_bsis_chv3, "BUKRS+HKONT+KOSTL+AUFNR+VBUND+BEWAR
wa_bsis_chv3 TYPE ty_bsis_chv3.

TYPES: BEGIN OF ty_bsis_chv4,
         bukrs     TYPE bsis-bukrs, "- empresa
         hkont     TYPE bsis-hkont, "- conta
         gsber     TYPE bsis-gsber, "– Divisão ( concessão )
         kostl     TYPE bsis-kostl, "– Centro de Custo
         aufnr     TYPE bsis-aufnr, "– Ordem Interna
         vbund     TYPE bsis-vbund, "– Sociedade Parceira
         bewar     TYPE bsis-bewar, "– Tipo de Movimento
         dmbtr(16) TYPE p DECIMALS 2, "– Moeda Interna
       END OF ty_bsis_chv4.

DATA: it_bsis_chv4 TYPE STANDARD TABLE OF ty_bsis_chv4, "BUKRS+HKONT+GSBER+KOSTL+AUFNR+VBUND+ BEWAR
wa_bsis_chv4 TYPE ty_bsis_chv4.


DATA: it_bsis_sum TYPE STANDARD TABLE OF ty_bsis,
      wa_bsis_sum TYPE ty_bsis.

DATA: it_ztfi0001 TYPE STANDARD TABLE OF ztfi0001,
      it_ztfi0004 TYPE STANDARD TABLE OF zefi0004,
      wa_ztfi0001 TYPE ztfi0001,
      wa_ztfi0004 TYPE zefi0004,
      wa_s_msg    TYPE bal_s_msg,
      st_layout   TYPE lvc_s_layo.

FIELD-SYMBOLS: <fs_bdc> TYPE bdcdata,
               <bsis>   TYPE ty_bsis,
               <fs_msg> TYPE bdcmsgcoll.

DATA: wa_options TYPE ctu_params,
      t_bdc      TYPE TABLE OF bdcdata,
      t_msg      TYPE TABLE OF bdcmsgcoll.

TYPES: BEGIN OF ty_encerra,
         empresa   LIKE bsis-bukrs,
         exercicio LIKE bsis-gjahr,
         etapa1(1) TYPE c,
         etapa2(1) TYPE c,
         nivel(1)  TYPE n,
         divisao   LIKE bsis-gsber,
       END OF ty_encerra. "

DATA: it_encerra TYPE STANDARD TABLE OF ty_encerra,
      wa_encerra TYPE ty_encerra.


TYPES: BEGIN OF ty_estorno,
         empresa   LIKE bsis-bukrs,
         exercicio LIKE bsis-gjahr,
         etapa1(1) TYPE c,
         etapa2(1) TYPE c,
         nivel(1)  TYPE n,
         documento LIKE bkpf-belnr,
         periodo   LIKE bsis-monat,
         divisao   LIKE bsis-gsber,
       END OF ty_estorno.

DATA: it_estorno TYPE STANDARD TABLE OF ty_estorno,
      wa_estorno TYPE ty_estorno.

TYPES: BEGIN OF ty_lancamento,
         empresa     LIKE bsis-bukrs,
         exercicio   LIKE bsis-gjahr,
         etapa1(1)   TYPE c,
         etapa2(1)   TYPE c,
         tipo_doc    LIKE bsis-blart,
         periodo     LIKE bsis-monat,
         dt_lan      LIKE bsis-bldat,
         dt_doc      LIKE bsis-budat,
         txt_cab_doc LIKE bkpf-bktxt,
         txt_lan     LIKE bsis-sgtxt,
         execucao(1) TYPE c,
         layout(30)  TYPE c,
       END OF ty_lancamento.

DATA: it_lancamento TYPE STANDARD TABLE OF ty_lancamento,
      wa_lancamento TYPE ty_lancamento.

" Declara variaveis para os eventos
DATA: ok_code_0001 LIKE sy-ucomm,
      clique_ok    LIKE sy-ucomm.

CONSTANTS: BEGIN OF c_folder_001,
             tab1 LIKE sy-ucomm VALUE 'FOLDER_001_FC1',
             tab2 LIKE sy-ucomm VALUE 'FOLDER_001_FC2',
             tab3 LIKE sy-ucomm VALUE 'FOLDER_001_FC3',
           END OF c_folder_001.

CONTROLS:  folder_001 TYPE TABSTRIP.
DATA:      BEGIN OF g_folder_001,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'SAPMZFI0003',
             pressed_tab LIKE sy-ucomm VALUE c_folder_001-tab1,
           END OF g_folder_001.


SELECTION-SCREEN BEGIN OF SCREEN 0100 AS SUBSCREEN.
SELECT-OPTIONS: periodo FOR bsis-monat.
SELECTION-SCREEN END OF SCREEN 0100.

SELECTION-SCREEN BEGIN OF SCREEN 0200 AS SUBSCREEN.
SELECT-OPTIONS: contas FOR bsis-hkont.
SELECTION-SCREEN END OF SCREEN 0200.

SELECTION-SCREEN BEGIN OF SCREEN 0300 AS SUBSCREEN.
SELECT-OPTIONS: divisao FOR bsis-gsber.
SELECTION-SCREEN END OF SCREEN 0300.
