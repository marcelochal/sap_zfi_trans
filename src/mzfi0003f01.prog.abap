*&---------------------------------------------------------------------*
*&  Include           MZFI0003F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_ITENS_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_select_itens_proc .

  DATA : r_bukrs  TYPE RANGE OF ztfi0001-bukrs,
         rl_bukrs LIKE LINE OF r_bukrs.

  DATA : r_gjahr  TYPE RANGE OF ztfi0001-gjahr,
         rl_gjahr LIKE LINE OF r_gjahr.

  DATA : r_etapa1  TYPE RANGE OF ztfi0001-etapa,
         rl_etapa1 LIKE LINE OF r_etapa1.

  DATA : r_etapa2  TYPE RANGE OF ztfi0001-etapa,
         rl_etapa2 LIKE LINE OF r_etapa2.

  DATA : r_gjhlv  TYPE RANGE OF ztfi0001-gjhlv,
         rl_gjhlv LIKE LINE OF r_gjhlv.

  DATA : r_divisao  TYPE RANGE OF ztfi0001-gsber,
         rl_divisao LIKE LINE OF r_divisao.

  IF NOT wa_encerra-divisao IS INITIAL.
    rl_divisao-sign = 'I'.
    rl_divisao-option = 'EQ'.
    rl_divisao-low =  wa_encerra-divisao.
    APPEND rl_divisao TO r_divisao.
  ENDIF.

  IF NOT wa_encerra-nivel IS INITIAL.
    rl_gjhlv-sign = 'I'.
    rl_gjhlv-option = 'EQ'.
    rl_gjhlv-low =  wa_encerra-nivel.
    APPEND rl_gjhlv TO r_gjhlv.
  ENDIF.

  IF NOT wa_encerra-empresa IS INITIAL.
    rl_bukrs-sign = 'I'.
    rl_bukrs-option = 'EQ'.
    rl_bukrs-low =  wa_encerra-empresa.
    APPEND rl_bukrs TO r_bukrs.
  ENDIF.


  IF NOT wa_encerra-exercicio IS INITIAL.
    rl_gjahr-sign = 'I'.
    rl_gjahr-option = 'EQ'.
    rl_gjahr-low =  wa_encerra-exercicio.
    APPEND rl_gjahr TO r_gjahr.
  ENDIF.


  IF NOT wa_encerra-etapa1 IS INITIAL.
    rl_etapa1-sign = 'I'.
    rl_etapa1-option = 'EQ'.
    rl_etapa1-low =  '1'.
    APPEND rl_etapa1 TO r_etapa1.
  ENDIF.


  IF NOT wa_encerra-etapa2 IS INITIAL.
    rl_etapa2-sign = 'I'.
    rl_etapa2-option = 'EQ'.
    rl_etapa2-low =  '2'.
    APPEND rl_etapa2 TO r_etapa2.
  ENDIF.

  FREE: it_ztfi0001.

  IF wa_encerra-etapa1 = 'X'.


    SELECT  mandt
            bukrs
            gjahr
            hkont
            monat
            belnr
            gsber
            etapa
            gjhlv
            cpudt
            usrnp
            dtsto
            usrnd
            FROM ztfi0001
             INTO TABLE it_ztfi0001
              WHERE bukrs IN r_bukrs            AND
                    gjahr IN r_gjahr            AND
                    gsber IN r_divisao          AND
                    etapa IN r_etapa1           AND
                    gjhlv IN r_gjhlv.

  ELSE.
    SELECT  mandt
            bukrs
            gjahr
            hkont
            monat
            belnr
            gsber
            etapa
            gjhlv
            cpudt
            usrnp
            dtsto
            usrnd
          FROM ztfi0001
           INTO TABLE it_ztfi0001
            WHERE bukrs IN r_bukrs              AND
                    gjahr IN r_gjahr            AND
                    gsber IN r_divisao          AND
                    etapa IN r_etapa2           AND
                    gjhlv IN r_gjhlv.
  ENDIF.

*** Início - 30/09/2018 - Implementação de Authority-Check Projeto AGIR
  PERFORM f_authority_check TABLES it_ztfi0001[]
                            USING '    ' "Empresa
                                  '03'.  "Exibir

*** Fim    - 30/09/2018 - Implementação de Authority-Check Projeto AGIR

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout_alv .
  st_layout-cwidth_opt = 'X'.
  st_layout-zebra = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNA_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estorna_documento .



  IF wa_estorno-documento IS INITIAL.
    IF wa_estorno-empresa IS INITIAL.
      MESSAGE e001(00) WITH 'Campo Empresa é obrigatório!'.
    ELSEIF wa_estorno-exercicio IS INITIAL.
      MESSAGE e001(00) WITH 'Campo Exercício é obrigatório!'.
    ENDIF.
  ENDIF.

*** Início - 30/09/2018 - Implementação de Authority-Check Projeto AGIR
  PERFORM f_authority_check TABLES it_ztfi0001[]
                            USING wa_estorno-empresa
                                  '02'. "Modificar
*** Fim    - 30/09/2018 - Implementação de Authority-Check Projeto AGIR


  PERFORM le_itens.

  CLEAR wa_options.

  MOVE: 'S' TO wa_options-updmode,
        'N' TO wa_options-dismode,
        'X' TO wa_options-racommit.

  FREE: it_ztfi0001_est[].

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'.

  CLEAR: wa_ztfi0004, it_ztfi0004.

  LOOP AT it_ztfi0001 INTO wa_ztfi0001.

    FREE: t_msg, t_bdc.

    PERFORM f_montar_campo_tela USING:
                'X'  'SAPMF05A'             '0105',
                ' '  'BDC_OKCODE'           '=BU',
                ' '  'RF05A-BELNS'          wa_ztfi0001-belnr,
                ' '  'RF05A-GJAHS'          wa_ztfi0001-gjahr,
                ' '  'BKPF-BUKRS'           wa_ztfi0001-bukrs,
                ' '  'UF05A-STGRD'          '01'.


    CALL TRANSACTION 'FB08'
         USING t_bdc
               OPTIONS FROM wa_options
               MESSAGES INTO t_msg.

    PERFORM f_preparar_msg_saida.

  ENDLOOP.



  IF sy-subrc <> 0.
    MESSAGE s001(00) WITH 'Não há documentos a serem estornados!'.
  ENDIF.

*  if not it_ZTFI0001_est[] is initial.
*
*    update ZTFI0001 from TABLE it_ZTFI0001_est.
*
*    COMMIT work.
**     set DTSTO  = sy-datum
**         usrnd  = sy-uzeit
**          where BUKRS = wa_ZTFI0001-bukrs and
**                BELNR = wa_ZTFI0001-belnr and
**                GJAHR = wa_ZTFI0001-GJAHR.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_CAMPO_TELA
*&---------------------------------------------------------------------*
FORM f_montar_campo_tela USING p_dynbegin
                               p_name
                               p_values.

  APPEND INITIAL LINE TO t_bdc ASSIGNING <fs_bdc>.

  IF NOT p_dynbegin IS INITIAL.

    MOVE: p_name   TO <fs_bdc>-program,
          p_values TO <fs_bdc>-dynpro,
          'X'      TO <fs_bdc>-dynbegin.

  ELSE.
    MOVE: p_name   TO <fs_bdc>-fnam,
          p_values TO <fs_bdc>-fval.

  ENDIF.
ENDFORM.                    "f_montar_campo_tela
*&---------------------------------------------------------------------*
*&      Form  LE_ITENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM le_itens .
  DATA : r_bukrs  TYPE RANGE OF ztfi0001-bukrs,
         rl_bukrs LIKE LINE OF r_bukrs.

  DATA : r_gjahr  TYPE RANGE OF ztfi0001-gjahr,
         rl_gjahr LIKE LINE OF r_gjahr.

  DATA : r_etapa1  TYPE RANGE OF ztfi0001-etapa,
         rl_etapa1 LIKE LINE OF r_etapa1.

  DATA : r_etapa2  TYPE RANGE OF ztfi0001-etapa,
         rl_etapa2 LIKE LINE OF r_etapa2.

  DATA : r_gjhlv  TYPE RANGE OF ztfi0001-gjhlv,
         rl_gjhlv LIKE LINE OF r_gjhlv.

  DATA : r_belnr  TYPE RANGE OF ztfi0001-belnr,
         rl_belnr LIKE LINE OF r_belnr.

  DATA : r_periodo  TYPE RANGE OF ztfi0001-monat,
         rl_periodo LIKE LINE OF r_periodo.

  DATA : r_divisao  TYPE RANGE OF ztfi0001-gsber,
         rl_divisao LIKE LINE OF r_divisao.

  IF NOT wa_estorno-divisao IS INITIAL.
    rl_divisao-sign = 'I'.
    rl_divisao-option = 'EQ'.
    rl_divisao-low =  wa_estorno-divisao.
    APPEND rl_divisao TO r_divisao.
  ENDIF.

  IF NOT wa_estorno-periodo IS INITIAL.
    rl_periodo-sign = 'I'.
    rl_periodo-option = 'EQ'.
    rl_periodo-low =  wa_estorno-periodo.
    APPEND rl_periodo TO r_periodo.
  ENDIF.


  IF NOT wa_estorno-documento IS INITIAL.
    rl_belnr-sign = 'I'.
    rl_belnr-option = 'EQ'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_estorno-documento
      IMPORTING
        output = wa_estorno-documento.

    rl_belnr-low =  wa_estorno-documento.
    APPEND rl_belnr TO r_belnr.
  ENDIF.

  IF NOT wa_estorno-nivel IS INITIAL.
    rl_gjhlv-sign = 'I'.
    rl_gjhlv-option = 'EQ'.
    rl_gjhlv-low =  wa_estorno-nivel.
    APPEND rl_gjhlv TO r_gjhlv.
  ENDIF.

  IF NOT wa_estorno-empresa IS INITIAL.
    rl_bukrs-sign = 'I'.
    rl_bukrs-option = 'EQ'.
    rl_bukrs-low =  wa_estorno-empresa.
    APPEND rl_bukrs TO r_bukrs.
  ENDIF.


  IF NOT wa_estorno-exercicio IS INITIAL.
    rl_gjahr-sign = 'I'.
    rl_gjahr-option = 'EQ'.
    rl_gjahr-low =  wa_estorno-exercicio.
    APPEND rl_gjahr TO r_gjahr.
  ENDIF.


  IF NOT wa_estorno-etapa1 IS INITIAL.
    rl_etapa1-sign = 'I'.
    rl_etapa1-option = 'EQ'.
    rl_etapa1-low =  '1'.
    APPEND rl_etapa1 TO r_etapa1.
  ENDIF.


  IF NOT wa_estorno-etapa2 IS INITIAL.
    rl_etapa2-sign = 'I'.
    rl_etapa2-option = 'EQ'.
    rl_etapa2-low =  '2'.
    APPEND rl_etapa2 TO r_etapa2.
  ENDIF.

  FREE: it_ztfi0001.


  IF wa_estorno-etapa1 = 'X'.
    SELECT  mandt
            bukrs
            gjahr
            hkont
            monat
            belnr
            gsber
            etapa
            gjhlv
            cpudt
            usrnp
            dtsto
            usrnd
            FROM ztfi0001
             INTO TABLE it_ztfi0001
              WHERE bukrs IN r_bukrs          AND
                    gjahr IN r_gjahr          AND
                    monat IN r_periodo        AND
                    belnr IN r_belnr          AND
                    gsber IN r_divisao        AND
                    etapa IN r_etapa1         AND
                    gjhlv IN r_gjhlv          AND
                    usrnd = ''.

  ELSE.
    SELECT  mandt
            bukrs
            gjahr
            hkont
            monat
            belnr
            gsber
            etapa
            gjhlv
            cpudt
            usrnp
            dtsto
            usrnd
          FROM ztfi0001
           INTO TABLE it_ztfi0001
             WHERE  bukrs IN r_bukrs          AND
                    gjahr IN r_gjahr          AND
                    monat IN r_periodo        AND
                    belnr IN r_belnr          AND
                    gsber IN r_divisao        AND
                    etapa IN r_etapa2         AND
                    gjhlv IN r_gjhlv          AND
                    usrnd = ''.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREPARAR_MSG_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_preparar_msg_saida .


  DATA: ws_msg TYPE balm,
        vl_msg TYPE c LENGTH 200.

  DELETE t_msg WHERE msgtyp NE 'S' AND msgtyp NE 'E'.

  LOOP AT t_msg ASSIGNING <fs_msg>.

    CONDENSE: <fs_msg>-msgv1, <fs_msg>-msgv2, <fs_msg>-msgv3, <fs_msg>-msgv4.

    WRITE: <fs_msg>-msgv1 TO ws_msg-msgv1 LEFT-JUSTIFIED,
           <fs_msg>-msgv2 TO ws_msg-msgv2 LEFT-JUSTIFIED,
           <fs_msg>-msgv3 TO ws_msg-msgv3 LEFT-JUSTIFIED,
           <fs_msg>-msgv4 TO ws_msg-msgv4 LEFT-JUSTIFIED.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = sy-langu
        msg_id                 = <fs_msg>-msgid
        msg_no                 = <fs_msg>-msgnr
        msg_var1               = ws_msg-msgv1
        msg_var2               = ws_msg-msgv2
        msg_var3               = ws_msg-msgv3
        msg_var4               = ws_msg-msgv4
      IMPORTING
        msg_text               = vl_msg
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.



    IF  <fs_msg>-msgtyp = 'S'.
      wa_ztfi0004-linha = wa_ztfi0004-linha + 1.
      wa_ztfi0004-msg   = vl_msg.
      APPEND wa_ztfi0004 TO it_ztfi0004.

*      clear: wa_ztfi0001_est.
*      wa_ZTFI0001_est-bukrs = sy-mandt.
*      wa_ZTFI0001_est-bukrs = wa_ZTFI0001-bukrs.
*      wa_ZTFI0001_est-belnr = wa_ZTFI0001-belnr.
*      wa_ZTFI0001_est-GJAHR = wa_ZTFI0001-GJAHR.
*      wa_ZTFI0001_est-DTSTO = sy-datum.
*      wa_ZTFI0001_est-usrnd = sy-uzeit.
*      append wa_ZTFI0001_est to it_ZTFI0001_est.

      UPDATE ztfi0001 CLIENT SPECIFIED
       SET dtsto  = sy-datum
           usrnd  = sy-uzeit
            WHERE mandt = sy-mandt AND
                  bukrs = wa_ztfi0001-bukrs AND
                  gjahr = wa_ztfi0001-gjahr AND
*                  hkont = wa_ZTFI0001-hkont and
                  monat = wa_ztfi0001-monat AND
                  belnr = wa_ztfi0001-belnr .

      IF sy-subrc = 0.
        COMMIT WORK.
      ENDIF.
    ELSEIF <fs_msg>-msgtyp = 'E'.
      wa_ztfi0004-linha = wa_ztfi0004-linha + 1.
      wa_ztfi0004-msg   = vl_msg.
      APPEND wa_ztfi0004 TO it_ztfi0004.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_MSG>_MSGTYP  text
*      -->P_0719   text
*      -->P_004    text
*      -->P_0721   text
*      -->P_<FS_MARA>_MATNR  text
*      -->P_VL_MSG(50)  text
*      -->P_VL_MSG+50(50)  text
*----------------------------------------------------------------------*
FORM f_montar_mensagem   USING p_msgty
                             p_msgid
                             p_msgno
                             p_msgv1
*                             p_msgv2
                             p_msgv3
                             p_msgv4.
*&---------------------------------------------------------------------*
*& Variaveis
*&---------------------------------------------------------------------*
  DATA v_log_handle TYPE balloghndl.

* Incluir mensagens no Log
  CLEAR wa_s_msg.
  wa_s_msg-msgty  = p_msgty.
  wa_s_msg-msgid  = p_msgid.
  wa_s_msg-msgno  = p_msgno.
  wa_s_msg-msgv1  = p_msgv1.
*  wa_s_msg-msgv2  = p_msgv2.
  wa_s_msg-msgv3  = p_msgv3.
  wa_s_msg-msgv4  = p_msgv4.
  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      i_log_handle = v_log_handle
      i_s_msg      = wa_s_msg.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LANCAMENTO_ENC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_lancamento_enc .


  CLEAR: wa_zefi0005.

  PERFORM inicializa_alv.

  PERFORM valida_campos.

  PERFORM carrega_t001.               "Dados da empresa via tabela T001

  IF NOT it_t001[] IS INITIAL.

    PERFORM carrega_empresas.           "ZTFI0002 - Empresa Válidas - tabela de configurações

    IF NOT it_ztfi0002[] IS INITIAL.

      PERFORM carrega_agrupamento_contas. "ZTFI0003 - dados do agrupamento de contas

      PERFORM carrega_bsis.               "Leitura das Contas na BSIS
      PERFORM preparar_sumarizacao.       " sumariação das contas de acordo com as regras de sumarização
      PERFORM gravavar_itens_processados.
    ELSE.
      wa_zefi0005-linha = wa_zefi0005-linha + 1.
      wa_zefi0005-tipo   = 'W'.
      wa_zefi0005-msg   = 'Verificar se a Empresa ou a Regra de Sumarização foi Configurada na ZFI002!'.
      APPEND wa_zefi0005 TO it_zefi0005.
    ENDIF.
  ELSE.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Empresa não cadasrada na T001!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CARREGA_BSIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_bsis .

  DATA : r_gjahr  TYPE RANGE OF ztfi0001-gjahr,
         rl_gjahr LIKE LINE OF r_gjahr.


  IF NOT wa_lancamento-exercicio IS INITIAL.
    rl_gjahr-sign = 'I'.
    rl_gjahr-option = 'EQ'.
    rl_gjahr-low =  wa_lancamento-exercicio.
    APPEND rl_gjahr TO r_gjahr.
  ENDIF.

  FREE: it_bsis.

  SELECT  bukrs "- empresa
            hkont "- conta
            gjahr "- exercício
            belnr "– documento contábil
            buzei "- item
            bschl "– chave de lançamento
            shkzg "- Sinal
            bstat "– status do documento
            monat "– período contábil
            blart "– tipo de documento
            budat "– data do documento
            bldat "– Data de Lançamento
            gsber "– Divisão ( concessão )
            vbund "– Sociedade Parceira
            bewar "– Tipo de Movimento
            kostl "– Centro de Custo
            aufnr "– Ordem Interna
            dmbtr "– Moeda Interna
              FROM bsis
               INTO TABLE it_bsis
                 FOR ALL ENTRIES IN it_ztfi0002
                  WHERE bukrs = it_ztfi0002-bukrs AND
                        hkont IN contas           AND
                        gjahr IN r_gjahr          AND
                        bstat = ''                AND
                        monat IN periodo          AND
                        gsber IN divisao .


  IF sy-subrc = 0.
    SORT it_bsis BY bukrs hkont gsber.

    FREE: it_bseg.
    SELECT bukrs belnr gjahr buzei hkont kstar
      FROM bseg
       INTO TABLE it_bseg
        FOR ALL ENTRIES IN it_bsis
         WHERE bukrs = it_bsis-bukrs AND
               belnr = it_bsis-belnr AND
               gjahr = it_bsis-gjahr AND
               buzei = it_bsis-buzei.

    IF sy-subrc = 0.
      SORT it_bseg BY bukrs belnr gjahr buzei.

      FREE: it_cskb.
      SELECT  kokrs kstar katyp
        FROM cskb
          INTO TABLE it_cskb
           FOR ALL ENTRIES IN it_bseg
             WHERE kokrs = 'TB00' AND
                   kstar = it_bseg-kstar AND
                   katyp <> '1'.

      IF sy-subrc = 0.
        SORT it_cskb BY kokrs kstar.
      ENDIF.

    ENDIF.

  ELSE.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Dados não encontrados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CARREGA_EMPRESAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_empresas .

  FREE: it_ztfi0002.
  SELECT *
    FROM ztfi0002
     INTO TABLE it_ztfi0002
      FOR ALL ENTRIES IN it_t001
      WHERE bukrs = it_t001-bukrs AND
            rgsum = gv_cpm_regra.

  IF sy-subrc = 0.
    SORT it_ztfi0002 BY bukrs rgsum.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CARREGA_AGRUPAMENTO_CONTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_agrupamento_contas .

  DATA : r_bukrs  TYPE RANGE OF ztfi0001-bukrs,
         rl_bukrs LIKE LINE OF r_bukrs.


  DATA : r_etapa1  TYPE RANGE OF ztfi0001-etapa,
         rl_etapa1 LIKE LINE OF r_etapa1.

  DATA : r_etapa2  TYPE RANGE OF ztfi0001-etapa,
         rl_etapa2 LIKE LINE OF r_etapa2.

  IF NOT wa_lancamento-etapa1 IS INITIAL.
    rl_etapa1-sign = 'I'.
    rl_etapa1-option = 'EQ'.
    rl_etapa1-low =  '1'.
    APPEND rl_etapa1 TO r_etapa1.
  ENDIF.

  IF NOT wa_lancamento-etapa2 IS INITIAL.
    rl_etapa2-sign = 'I'.
    rl_etapa2-option = 'EQ'.
    rl_etapa2-low =  '2'.
    APPEND rl_etapa2 TO r_etapa2.
  ENDIF.


  FREE: it_ztfi0003.
  IF NOT wa_lancamento-etapa1 IS INITIAL.
    SELECT *
      FROM ztfi0003
        INTO TABLE it_ztfi0003
      FOR ALL ENTRIES IN it_ztfi0002
         WHERE bukrs = it_ztfi0002-bukrs  AND
               etapa IN r_etapa1. " and
*               GJHLV = 1.
    IF sy-subrc = 0.
      SORT it_ztfi0003 BY bukrs gjhlv.
    ENDIF.
  ELSE.
    SELECT *
      FROM ztfi0003
        INTO TABLE it_ztfi0003
       FOR ALL ENTRIES IN it_ztfi0002
         WHERE bukrs = it_ztfi0002-bukrs  AND
               etapa IN r_etapa2. "and
*               GJHLV = 1.
    IF sy-subrc = 0.
      SORT it_ztfi0003 BY bukrs gjhlv.
    ENDIF.
  ENDIF.


  DATA : r_gjahr  TYPE RANGE OF ztfi0001-gjahr,
         rl_gjahr LIKE LINE OF r_gjahr.

  IF NOT wa_lancamento-exercicio IS INITIAL.
    rl_gjahr-sign = 'I'.
    rl_gjahr-option = 'EQ'.
    rl_gjahr-low =  wa_lancamento-exercicio.
    APPEND rl_gjahr TO r_gjahr.
  ENDIF.




  DATA : r_conta  TYPE RANGE OF bsis-hkont,
         rl_conta LIKE LINE OF r_conta.


  FREE: r_conta.

  LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv <> '1' OR gjhlv <> '01'.
    rl_conta-sign = 'I'.
    rl_conta-option = 'BT'.
    rl_conta-low =  wa_ztfi0003-sknr1.
    rl_conta-high =  wa_ztfi0003-sknr2.
    APPEND rl_conta TO r_conta.
  ENDLOOP.

  FREE: it_bsis_nivel2.


*
  TYPES: BEGIN OF ty_bkpf,
           bukrs TYPE bkpf-bukrs,
           belnr TYPE bkpf-belnr,
           gjahr TYPE bkpf-gjahr,
           stblg TYPE bkpf-stblg,
         END OF ty_bkpf.

  DATA: it_bkpf TYPE STANDARD TABLE OF ty_bkpf,
        wa_bkpf TYPE  ty_bkpf.

*  refresh: it_bkpf.
*  select BUKRS BELNR GJAHR stblg
*    from bkpf
*      into table it_bkpf
*       FOR ALL ENTRIES IN it_ZTFI0002
*        where bukrs = it_ZTFI0002-bukrs and
*              GJAHR in r_GJAHR and
*              stblg = '' .

  .
  IF NOT it_bkpf[] IS INITIAL.
    SELECT  bukrs "- empresa
              hkont "- conta
              gjahr "- exercício
              belnr "– documento contábil
              buzei "- item
              bschl "– chave de lançamento
              shkzg "- Sinal
              bstat "– status do documento
              monat "– período contábil
              blart "– tipo de documento
              budat "– data do documento
              bldat "– Data de Lançamento
              gsber "– Divisão ( concessão )
              vbund "– Sociedade Parceira
              bewar "– Tipo de Movimento
              kostl "– Centro de Custo
              aufnr "– Ordem Interna
              dmbtr "– Moeda Interna
                FROM bsis
                 INTO TABLE it_bsis_nivel2
*                   FOR ALL ENTRIES IN  it_bkpf
                    WHERE bukrs = wa_lancamento-empresa AND
                          hkont IN r_conta              AND
                          gjahr IN r_gjahr              AND
                          bstat = ''                    AND
                          monat = wa_lancamento-periodo AND
                          gsber IN divisao.

    IF sy-subrc = 0.
      SORT it_bsis_nivel2 BY bukrs hkont gsber.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CARREGA_T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_t001 .

  DATA : r_bukrs  TYPE RANGE OF ztfi0001-bukrs,
         rl_bukrs LIKE LINE OF r_bukrs.

  IF NOT wa_lancamento-empresa IS INITIAL.
    rl_bukrs-sign = 'I'.
    rl_bukrs-option = 'EQ'.
    rl_bukrs-low =  wa_lancamento-empresa.
    APPEND rl_bukrs TO r_bukrs.
  ENDIF.

  FREE it_t001.
  SELECT bukrs waers periv xgsbe
    INTO TABLE it_t001
      FROM t001
       WHERE bukrs IN r_bukrs.

  IF sy-subrc = 0.
    SORT it_t001 BY bukrs.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARAR_SUMARIZACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM preparar_sumarizacao .

  CLEAR: wa_ztfi0002.
  READ TABLE it_ztfi0002 INTO wa_ztfi0002 INDEX 1.



  IF sy-subrc = 0.
    CASE wa_ztfi0002-rgsum.
      WHEN '01'. "Centro de Custo, Ordem Interna
        PERFORM processa_regra_1.
      WHEN '02'. "Centro de Custo, Ordem Interna, Sociedade Parceira,Tipo Movimento Consolidação
        PERFORM processa_regra_2.
    ENDCASE.
  ELSE.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Verificar se a Empresa ou a Regra de Sumarização foi Configurada na ZFI002!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_REGRA_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_regra_1 .

  FREE: it_bsis_divisao, wa_bsis, it_bsis_divisao_n2.

  DATA: lv_idx LIKE sy-tabix.

  LOOP AT it_bsis INTO wa_bsis.
    lv_idx = sy-tabix.
*    IF wa_bsis-kostl IS INITIAL AND wa_bsis-aufnr IS INITIAL.
    IF wa_bsis-blart = 'ZZ'.
      CLEAR: wa_bseg.
      READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bsis-bukrs
                                               belnr = wa_bsis-belnr
                                               gjahr = wa_bsis-gjahr
                                    buzei = wa_bsis-buzei BINARY SEARCH.

      IF sy-subrc = 0.
        IF wa_bseg-hkont NE wa_bseg-kstar.
          CLEAR: wa_cskb .
          READ TABLE it_cskb INTO wa_cskb INDEX 1.
          IF sy-subrc = 0.
            DELETE it_bsis INDEX lv_idx .
            CONTINUE.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: wa_bsis_divisao.
    MOVE-CORRESPONDING wa_bsis TO wa_bsis_divisao.
    COLLECT wa_bsis_divisao INTO it_bsis_divisao.
  ENDLOOP.

  SORT it_bsis_divisao BY bukrs gsber.

*  delete it_bsis_divisao where gsber = ''.

  CLEAR: wa_bsis.

  LOOP AT it_bsis_nivel2 INTO wa_bsis.

*    if wa_bsis-gsber = ''.
*      wa_ZeFI0005-linha = wa_ZeFI0005-linha + 1.
*      wa_ZeFI0005-tipo   = 'E'.
*      concatenate 'Conta' wa_bsis-hkont 'Documento' wa_bsis-belnr 'com Divisão Vazia'
*      into wa_ZeFI0005-msg   separated by space.
*      append wa_ZeFI0005 to it_ZeFI0005.
*    endif.


    CLEAR: wa_bsis_divisao.
    MOVE-CORRESPONDING wa_bsis TO wa_bsis_divisao.
    COLLECT wa_bsis_divisao INTO it_bsis_divisao_n2.
  ENDLOOP.

  SORT it_bsis_divisao_n2 BY bukrs gsber.

*  delete it_bsis_divisao_n2 where gsber = ''.

  SORT it_t001     BY bukrs.

  CLEAR: wa_t001,it_ztfi0001[].

  FREE: it_zefi0005, it_bsis_chv1, it_bsis_chv2,it_bsis_contabil .

  READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa BINARY SEARCH .

  IF sy-subrc = 0.
    IF wa_t001-xgsbe = ''.

      SORT it_bsis BY bukrs hkont.

      LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv = '1'.

        CLEAR: wa_bsis_divisao.

        LOOP AT it_bsis_divisao INTO wa_bsis_divisao ."where hkont >= wa_ZTFI0003-sknr1 and
          "    hkont <= wa_ZTFI0003-sknr2.

          FREE:  gv_resultado, it_bsis_contabil. FREE: it_bsis_chv1.
          LOOP AT it_bsis ASSIGNING <bsis> WHERE bukrs = wa_bsis_divisao-bukrs AND
                                                  hkont >= wa_ztfi0003-sknr1 AND
                                                  hkont <= wa_ztfi0003-sknr2 AND
*                                                 hkont = wa_bsis_divisao-hkont and
                                                 gsber = wa_bsis_divisao-gsber.

            gv_resultado = gv_resultado + <bsis>-dmbtr.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_chv1.

            IF <bsis>-shkzg = 'H'.
              wa_bsis_chv1-dmbtr  = <bsis>-dmbtr * ( -1 ).
            ELSE.
              wa_bsis_chv1-dmbtr  = <bsis>-dmbtr.
            ENDIF.

            COLLECT wa_bsis_chv1 INTO it_bsis_chv1.

            CLEAR: wa_bsis_contabil.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_contabil.
            COLLECT wa_bsis_contabil INTO it_bsis_contabil.

          ENDLOOP.



          IF sy-subrc = 0.

*           encontrar o saldo contábil no exercício de cada conta contábil
            LOOP AT it_bsis_contabil INTO wa_bsis_contabil. "Nova Tabela sumarizada

              PERFORM selecionar_glflext USING  wa_bsis_contabil-bukrs
                                                wa_bsis_contabil-gjahr
                                                wa_bsis_contabil-hkont
                                                CHANGING gt_glt0_sum  .

              LOOP AT gt_glt0_sum INTO wa_glt0_sum WHERE rhlst IS INITIAL OR rhlst = '0.00'.
                DELETE it_bsis_chv1 WHERE bukrs = wa_glt0_sum-bukrs AND
*                                        GJAHR = wa_glt0_sum-ryear and
                                          hkont = wa_glt0_sum-racct .
*                                        GSBER = wa_glt0_sum-rbusa .
              ENDLOOP.
            ENDLOOP.

            SORT it_bsis_chv1 BY bukrs hkont kostl aufnr.
            PERFORM monta_saida_01.
            IF wa_lancamento-execucao IS INITIAL.
              PERFORM processar_fb01.       "*** Processa FB01 via BAPI
            ELSE.
              PERFORM processar_fb01_check.
              LOOP AT it_bsis_saida INTO wa_bsis_saida.
                MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
                APPEND wa_zefi0001 TO it_zefi0001.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      SORT it_bsis_nivel2 BY bukrs hkont.

      LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv NE '1'.

        CLEAR: wa_bsis_divisao.

        LOOP AT it_bsis_divisao_n2 INTO wa_bsis_divisao ."where hkont >= wa_ZTFI0003-sknr1 and
          " hkont <= wa_ZTFI0003-sknr2.

          FREE:   gv_resultado, it_bsis_contabil. FREE: it_bsis_chv1.
          LOOP AT it_bsis_nivel2 ASSIGNING <bsis> WHERE bukrs = wa_bsis_divisao-bukrs AND
          hkont >= wa_ztfi0003-sknr1 AND
          hkont <= wa_ztfi0003-sknr2 AND
*                                                 hkont = wa_bsis_divisao-hkont and
          gsber = wa_bsis_divisao-gsber.

            MOVE-CORRESPONDING <bsis> TO wa_bsis_chv1.

            IF <bsis>-shkzg = 'H'.
              wa_bsis_chv1-dmbtr  = <bsis>-dmbtr * ( -1 ).
            ELSE.
              wa_bsis_chv1-dmbtr  = <bsis>-dmbtr.
            ENDIF.

            COLLECT wa_bsis_chv1 INTO it_bsis_chv1.


          ENDLOOP.


          IF sy-subrc = 0.
            SORT it_bsis_chv1 BY bukrs hkont kostl aufnr.
            PERFORM monta_saida_01_nivel2.
            IF wa_lancamento-execucao IS INITIAL.
              PERFORM processar_fb01.       "*** Processa FB01 via BAPI
            ELSE.
              PERFORM processar_fb01_check.
              LOOP AT it_bsis_saida INTO wa_bsis_saida.
                MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
                APPEND wa_zefi0001 TO it_zefi0001.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.


    ELSE.


      SORT it_bsis BY bukrs hkont  .

      LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv = '1' .
        CLEAR: wa_bsis_divisao.

        LOOP AT it_bsis_divisao INTO wa_bsis_divisao ."where hkont >= wa_ZTFI0003-sknr1 and
          "    hkont <= wa_ZTFI0003-sknr2.

          FREE:   gv_resultado, it_bsis_contabil. FREE: it_bsis_chv2.
          LOOP AT it_bsis ASSIGNING <bsis> WHERE bukrs = wa_bsis_divisao-bukrs AND
                                                    hkont >= wa_ztfi0003-sknr1 AND
                                                    hkont <= wa_ztfi0003-sknr2 AND
*                                                 hkont = wa_bsis_divisao-hkont and
                                             gsber = wa_bsis_divisao-gsber.



            gv_resultado = gv_resultado + <bsis>-dmbtr.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_chv2.

            IF <bsis>-shkzg = 'H'.
              wa_bsis_chv2-dmbtr  = <bsis>-dmbtr * ( -1 ).
            ELSE.
              wa_bsis_chv2-dmbtr  = <bsis>-dmbtr.
            ENDIF.

            COLLECT wa_bsis_chv2 INTO it_bsis_chv2.

            CLEAR: wa_bsis_contabil.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_contabil.
            COLLECT wa_bsis_contabil INTO it_bsis_contabil.


          ENDLOOP.

          IF sy-subrc = 0.

*           encontrar o saldo contábil no exercício de cada conta contábil
            LOOP AT it_bsis_contabil INTO wa_bsis_contabil. "Nova Tabela sumarizada

              PERFORM selecionar_glflext USING  wa_bsis_contabil-bukrs
                                                wa_bsis_contabil-gjahr
                                                wa_bsis_contabil-hkont
                                                CHANGING gt_glt0_sum  .

              LOOP AT gt_glt0_sum INTO wa_glt0_sum WHERE rhlst IS INITIAL OR rhlst = '0.00'.
                DELETE it_bsis_chv2 WHERE bukrs = wa_glt0_sum-bukrs AND
*                                        GJAHR = wa_glt0_sum-ryear and
                                          hkont = wa_glt0_sum-racct AND
                                          gsber = wa_glt0_sum-rbusa .
              ENDLOOP.
            ENDLOOP.

            SORT it_bsis_chv2 BY bukrs hkont gsber kostl aufnr.
            PERFORM monta_saida_02.
            IF wa_lancamento-execucao IS INITIAL.
              PERFORM processar_fb01.       "*** Processa FB01 via BAPI
            ELSE.
              PERFORM processar_fb01_check.
              LOOP AT it_bsis_saida INTO wa_bsis_saida.
                MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
                APPEND wa_zefi0001 TO it_zefi0001.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      SORT it_bsis_nivel2 BY bukrs hkont .

      LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv NE '1'.

        CLEAR: wa_bsis_divisao.

        LOOP AT it_bsis_divisao_n2 INTO wa_bsis_divisao ."where hkont >= wa_ZTFI0003-sknr1 and
          " hkont <= wa_ZTFI0003-sknr2.

          FREE:  gv_resultado, it_bsis_contabil. FREE: it_bsis_chv2.
          LOOP AT it_bsis_nivel2 ASSIGNING <bsis> WHERE bukrs = wa_bsis_divisao-bukrs AND
          hkont >= wa_ztfi0003-sknr1 AND
          hkont <= wa_ztfi0003-sknr2 AND
*                                                 hkont = wa_bsis_divisao-hkont and
          gsber = wa_bsis_divisao-gsber.

            MOVE-CORRESPONDING <bsis> TO wa_bsis_chv2.

            IF <bsis>-shkzg = 'H'.
              wa_bsis_chv2-dmbtr  = <bsis>-dmbtr * ( -1 ).
            ELSE.
              wa_bsis_chv2-dmbtr  = <bsis>-dmbtr.
            ENDIF.

            COLLECT wa_bsis_chv2 INTO it_bsis_chv2.

          ENDLOOP.

          IF sy-subrc = 0.
            SORT it_bsis_chv2 BY bukrs hkont kostl aufnr.
            PERFORM monta_saida_02_nivel2.
            IF wa_lancamento-execucao IS INITIAL.
              PERFORM processar_fb01.       "*** Processa FB01 via BAPI
            ELSE.
              PERFORM processar_fb01_check.
              LOOP AT it_bsis_saida INTO wa_bsis_saida.
                MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
                APPEND wa_zefi0001 TO it_zefi0001.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESSA_REGRA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_regra_2 .



  FREE: it_bsis_divisao, wa_bsis, it_bsis_divisao_n2.

  DATA: lv_idx LIKE sy-tabix.

  LOOP AT it_bsis INTO wa_bsis.
    lv_idx = sy-tabix.
*    IF wa_bsis-kostl IS INITIAL AND wa_bsis-aufnr IS INITIAL.
    IF wa_bsis-blart = 'ZZ'.
      CLEAR: wa_bseg.
      READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bsis-bukrs
                                               belnr = wa_bsis-belnr
                                               gjahr = wa_bsis-gjahr
                                    buzei = wa_bsis-buzei BINARY SEARCH.

      IF sy-subrc = 0.
        IF wa_bseg-hkont NE wa_bseg-kstar.
          CLEAR: wa_cskb .
          READ TABLE it_cskb INTO wa_cskb INDEX 1.
          IF sy-subrc = 0.
            DELETE it_bsis INDEX lv_idx .
            CONTINUE.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


    CLEAR: wa_bsis_divisao.
    MOVE-CORRESPONDING wa_bsis TO wa_bsis_divisao.
    COLLECT wa_bsis_divisao INTO it_bsis_divisao.
  ENDLOOP.

  SORT it_bsis_divisao BY bukrs gsber.

*  delete it_bsis_divisao where gsber = ''.

  CLEAR: wa_bsis.

  LOOP AT it_bsis_nivel2 INTO wa_bsis.

*    if wa_bsis-gsber = ''.
*      wa_ZeFI0005-linha = wa_ZeFI0005-linha + 1.
*      wa_ZeFI0005-tipo   = 'E'.
*      concatenate 'Conta' wa_bsis-hkont 'Documento' wa_bsis-belnr 'com Divisão Vazia'
*      into wa_ZeFI0005-msg   separated by space.
*      append wa_ZeFI0005 to it_ZeFI0005.
*    endif.


    CLEAR: wa_bsis_divisao.
    MOVE-CORRESPONDING wa_bsis TO wa_bsis_divisao.
    COLLECT wa_bsis_divisao INTO it_bsis_divisao_n2.
  ENDLOOP.

  SORT it_bsis_divisao_n2 BY bukrs gsber.

*  delete it_bsis_divisao_n2 where gsber = ''.

  SORT it_t001     BY bukrs.

  CLEAR: wa_t001.
  READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_ztfi0002-bukrs BINARY SEARCH.

  IF sy-subrc = 0.
    IF wa_t001-xgsbe = ''.

      SORT it_bsis BY bukrs hkont .

      LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv = '1'.
        CLEAR: wa_bsis_divisao.

        LOOP AT it_bsis_divisao INTO wa_bsis_divisao."' where hkont >= wa_ZTFI0003-sknr1 and
          "    hkont <= wa_ZTFI0003-sknr2.

          FREE: gv_resultado, it_bsis_contabil. FREE: it_bsis_chv3.
          LOOP AT it_bsis ASSIGNING <bsis> WHERE bukrs = wa_bsis_divisao-bukrs AND
                                                    hkont >= wa_ztfi0003-sknr1 AND
                                                    hkont <= wa_ztfi0003-sknr2 AND
*                                                 hkont = wa_bsis_divisao-hkont and
                                             gsber = wa_bsis_divisao-gsber.

            gv_resultado = gv_resultado + <bsis>-dmbtr.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_chv3.

            IF <bsis>-shkzg = 'H'.
              wa_bsis_chv3-dmbtr  = <bsis>-dmbtr * ( -1 ).
            ELSE.
              wa_bsis_chv3-dmbtr  = <bsis>-dmbtr.
            ENDIF.

            COLLECT wa_bsis_chv3 INTO it_bsis_chv3.

            CLEAR: wa_bsis_contabil.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_contabil.
            COLLECT wa_bsis_contabil INTO it_bsis_contabil.


          ENDLOOP.

          IF sy-subrc = 0.
* encontrar o saldo contábil no exercício de cada conta contábil
            LOOP AT it_bsis_contabil INTO wa_bsis_contabil. "Nova Tabela sumarizada

              PERFORM selecionar_glflext USING  wa_bsis_contabil-bukrs
                                                wa_bsis_contabil-gjahr
                                                wa_bsis_contabil-hkont
                                                CHANGING gt_glt0_sum  .

              LOOP AT gt_glt0_sum INTO wa_glt0_sum WHERE rhlst IS INITIAL OR rhlst = '0.00'.
                DELETE it_bsis_chv3 WHERE bukrs = wa_glt0_sum-bukrs AND
*                                        GJAHR = wa_glt0_sum-ryear and
                                          hkont = wa_glt0_sum-racct .
*                                        GSBER = wa_glt0_sum-rbusa .
              ENDLOOP.
            ENDLOOP.
            SORT it_bsis_chv3 BY bukrs hkont kostl aufnr vbund bewar.
            PERFORM monta_saida_03.
            IF wa_lancamento-execucao IS INITIAL.
              PERFORM processar_fb01.       "*** Processa FB01 via BAPI
            ELSE.
              PERFORM processar_fb01_check.
              LOOP AT it_bsis_saida INTO wa_bsis_saida.
                MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
                APPEND wa_zefi0001 TO it_zefi0001.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.


      SORT it_bsis_nivel2 BY bukrs hkont .
      FREE: it_bsis_chv3.
      LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv NE '1'.
        CLEAR: wa_bsis_divisao.

        LOOP AT it_bsis_divisao_n2 INTO wa_bsis_divisao." where hkont >= wa_ZTFI0003-sknr1 and
          "  hkont <= wa_ZTFI0003-sknr2.

          FREE:   gv_resultado. FREE: it_bsis_chv3.
          LOOP AT it_bsis_nivel2 ASSIGNING <bsis> WHERE bukrs = wa_bsis_divisao-bukrs AND
          hkont >= wa_ztfi0003-sknr1 AND
          hkont <= wa_ztfi0003-sknr2 AND
*                                                 hkont = wa_bsis_divisao-hkont and
          gsber = wa_bsis_divisao-gsber.

*          gv_resultado = gv_resultado + <bsis>-DMBTR.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_chv3.

            IF <bsis>-shkzg = 'H'.
              wa_bsis_chv3-dmbtr  = <bsis>-dmbtr * ( -1 ).
            ELSE.
              wa_bsis_chv3-dmbtr  = <bsis>-dmbtr.
            ENDIF.

            COLLECT wa_bsis_chv3 INTO it_bsis_chv3.

          ENDLOOP.
          IF sy-subrc = 0.
            SORT it_bsis_chv3 BY bukrs hkont kostl aufnr vbund bewar.

            PERFORM monta_saida_03_nivel2.
            IF wa_lancamento-execucao IS INITIAL.
              PERFORM processar_fb01.       "*** Processa FB01 via BAPI
            ELSE.
              PERFORM processar_fb01_check.
              LOOP AT it_bsis_saida INTO wa_bsis_saida.
                MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
                APPEND wa_zefi0001 TO it_zefi0001.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    ELSE.
      SORT it_bsis BY bukrs hkont.
      LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv = '1'..
        CLEAR: wa_bsis_divisao.

        LOOP AT it_bsis_divisao INTO wa_bsis_divisao." where hkont >= wa_ZTFI0003-sknr1 and
          "     hkont <= wa_ZTFI0003-sknr2.
          FREE:   it_bsis_contabil, gv_resultado. FREE: it_bsis_chv4.
          LOOP AT it_bsis ASSIGNING <bsis> WHERE bukrs = wa_bsis_divisao-bukrs AND
                                                    hkont >= wa_ztfi0003-sknr1 AND
                                                    hkont <= wa_ztfi0003-sknr2 AND
*                                                 hkont = wa_bsis_divisao-hkont and
                                             gsber = wa_bsis_divisao-gsber.
            gv_resultado = gv_resultado + <bsis>-dmbtr.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_chv4.

            IF <bsis>-shkzg = 'H'.
              wa_bsis_chv4-dmbtr  = <bsis>-dmbtr * ( -1 ).
            ELSE.
              wa_bsis_chv4-dmbtr  = <bsis>-dmbtr.
            ENDIF.

            COLLECT wa_bsis_chv4 INTO it_bsis_chv4.

            CLEAR: wa_bsis_contabil.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_contabil.
            COLLECT wa_bsis_contabil INTO it_bsis_contabil. "Nova Tabela sumarizada
          ENDLOOP.

          IF sy-subrc = 0.

*          encontrar o saldo contábil no exercício de cada conta contábil
            LOOP AT it_bsis_contabil INTO wa_bsis_contabil. "Nova Tabela sumarizada

              PERFORM selecionar_glflext USING  wa_bsis_contabil-bukrs
                                                wa_bsis_contabil-gjahr
                                                wa_bsis_contabil-hkont
                                                CHANGING gt_glt0_sum  .

              LOOP AT gt_glt0_sum INTO wa_glt0_sum WHERE rhlst IS INITIAL OR rhlst = '0.00'.
                DELETE it_bsis_chv4 WHERE bukrs = wa_glt0_sum-bukrs AND
*                                        GJAHR = wa_glt0_sum-ryear and
                                          hkont = wa_glt0_sum-racct AND
                                          gsber = wa_glt0_sum-rbusa .
              ENDLOOP.
            ENDLOOP.

            SORT it_bsis_chv4 BY bukrs hkont gsber kostl aufnr vbund bewar.
            PERFORM monta_saida_04.
            IF wa_lancamento-execucao IS INITIAL.
              PERFORM processar_fb01.       "*** Processa FB01 via BAPI
            ELSE.
              PERFORM processar_fb01_check.
              LOOP AT it_bsis_saida INTO wa_bsis_saida.
                MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
                APPEND wa_zefi0001 TO it_zefi0001.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      SORT it_bsis_nivel2 BY bukrs hkont.
      LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE gjhlv NE '1'..

        CLEAR: wa_bsis_divisao.

        LOOP AT it_bsis_divisao_n2 INTO wa_bsis_divisao." where hkont >= wa_ZTFI0003-sknr1 and
          "  hkont <= wa_ZTFI0003-sknr2.

          FREE:   gv_resultado. FREE: it_bsis_chv4.
          LOOP AT it_bsis_nivel2 ASSIGNING <bsis> WHERE bukrs = wa_bsis_divisao-bukrs AND
          hkont >= wa_ztfi0003-sknr1 AND
          hkont <= wa_ztfi0003-sknr2 AND
*                                                 hkont = wa_bsis_divisao-hkont and
          gsber = wa_bsis_divisao-gsber.


*          gv_resultado = gv_resultado + <bsis>-DMBTR.
            MOVE-CORRESPONDING <bsis> TO wa_bsis_chv4.


            IF <bsis>-shkzg = 'H'.
              wa_bsis_chv4-dmbtr  = <bsis>-dmbtr * ( -1 ).
            ELSE.
              wa_bsis_chv4-dmbtr  = <bsis>-dmbtr.
            ENDIF.

            COLLECT wa_bsis_chv4 INTO it_bsis_chv4.

          ENDLOOP.
          IF sy-subrc = 0.
            SORT it_bsis_chv4 BY bukrs hkont gsber kostl aufnr vbund bewar.
            PERFORM monta_saida_04_nivel2.
            IF wa_lancamento-execucao IS INITIAL.
              PERFORM processar_fb01.       "*** Processa FB01 via BAPI
            ELSE.
              PERFORM processar_fb01_check.
              LOOP AT it_bsis_saida INTO wa_bsis_saida.
                MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
                APPEND wa_zefi0001 TO it_zefi0001.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GRAVAVAR_ITENS_PROCESSADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gravavar_itens_processados .

  IF NOT it_ztfi0001[] IS INITIAL.
    INSERT ztfi0001 FROM TABLE it_ztfi0001.
    COMMIT WORK.
  ENDIF.
  FREE: it_ztfi0001.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDA_AGRUPAMENTO_CONTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_agrupamento_contas .

*  clear: it_bsis_saida[], it_ZEFI0001[].
*
*  case gv_cpm_regra.
*    when '1'. "Centro de Custo, Ordem Interna
*      if not it_bsis_chv1[] is INITIAL.
*
*        loop at it_bsis_chv1 into wa_bsis_chv1.
*
*          loop at it_ZTFI0003 into wa_ZTFI0003 where bukrs = wa_bsis_chv1-bukrs.
*            if wa_bsis_chv1-hkont >= wa_ZTFI0003-sknr1 and wa_bsis_chv1-hkont <= wa_ZTFI0003-sknr2.
*              clear: wa_bsis.
*              read table it_bsis into wa_bsis with key BUKRS = wa_bsis_chv1-BUKRS
*                                                       HKONT = wa_bsis_chv1-HKONT
*                                                       KOSTL = wa_bsis_chv1-KOSTL
*                                                       AUFNR = wa_bsis_chv1-aufnr BINARY SEARCH.
*              if sy-subrc = 0.
*                MOVE-CORRESPONDING wa_bsis  to wa_bsis_saida.
*                move  wa_bsis_chv1-dmbtr to wa_bsis_saida-dmbtr.
*                move  wa_ZTFI0003-BUZET to wa_bsis_saida-BUZET.
*                if wa_lancamento-txt_lan is initial.
*                  move wa_ZTFI0003-SGTXT to wa_bsis_saida-sgtxt.
*                else.
*                  move wa_lancamento-txt_lan to wa_bsis_saida-sgtxt.
*                endif.
*
*                if wa_lancamento-tipo_doc is initial.
*                  move wa_bsis-blart to wa_bsis_saida-blart.
*                else.
*                  move wa_lancamento-tipo_doc to wa_bsis_saida-blart.
*                endif.
*
*                clear: wa_t001.
*                READ TABLE it_t001 into wa_t001 with key bukrs = wa_lancamento-empresa
*                                                         XGSBE = 'X' BINARY SEARCH.
*                if sy-subrc = 0.
*                  concatenate  wa_bsis_saida-bukrs
*                               wa_bsis_saida-hkont
*                               wa_bsis_saida-gsber into wa_bsis_saida-lv_campo_chave.
*
*                ELSE.
*
*                  concatenate  wa_bsis_saida-bukrs
*                               wa_bsis_saida-hkont into wa_bsis_saida-lv_campo_chave.
*                endif.
*                append wa_bsis_saida to it_bsis_saida.
*                exit.
*              endif.
*
*            else.
*              " informar que a conta não existe no intervalo de agrupamentos de conntas
*              MOVE-CORRESPONDING wa_bsis_chv1 to wa_ZEFI0001 .
*              append wa_ZEFI0001 to it_ZEFI0001.
*            endif.
*          endloop.
*        ENDLOOP.
*
*      ELSEif not it_bsis_chv2[] is INITIAL.
*
*        loop at it_bsis_chv2 into wa_bsis_chv2.
*          loop at it_ZTFI0003 into wa_ZTFI0003 where bukrs = wa_bsis_chv2-bukrs.
*            if wa_bsis_chv2-hkont >= wa_ZTFI0003-sknr1 and wa_bsis_chv2-hkont <= wa_ZTFI0003-sknr2.
*              clear: wa_bsis.
*              read table it_bsis into wa_bsis with key BUKRS = wa_bsis_chv2-BUKRS
*                                                       HKONT = wa_bsis_chv2-HKONT
*                                                       gsber = wa_bsis_chv2-gsber
*                                                       KOSTL = wa_bsis_chv2-KOSTL
*                                                       AUFNR = wa_bsis_chv2-aufnr BINARY SEARCH.
*              if sy-subrc = 0.
*                MOVE-CORRESPONDING wa_bsis  to wa_bsis_saida.
*                move  wa_bsis_chv2-dmbtr to wa_bsis_saida-dmbtr.
*                move  wa_ZTFI0003-BUZET to wa_bsis_saida-BUZET.
*                if wa_lancamento-txt_lan is initial.
*                  move wa_ZTFI0003-SGTXT to wa_bsis_saida-sgtxt.
*                else.
*                  move wa_lancamento-txt_lan to wa_bsis_saida-sgtxt.
*                endif.
*
*                if wa_lancamento-tipo_doc is initial.
*                  move wa_bsis-blart to wa_bsis_saida-blart.
*                else.
*                  move wa_lancamento-tipo_doc to wa_bsis_saida-blart.
*                endif.
*
*                clear: wa_t001.
*                READ TABLE it_t001 into wa_t001 with key bukrs = wa_lancamento-empresa
*                                                         XGSBE = 'X' BINARY SEARCH.
*                if sy-subrc = 0.
*                  concatenate  wa_bsis_saida-bukrs
*                               wa_bsis_saida-hkont
*                               wa_bsis_saida-gsber into wa_bsis_saida-lv_campo_chave.
*
*                ELSE.
*
*                  concatenate  wa_bsis_saida-bukrs
*                               wa_bsis_saida-hkont into wa_bsis_saida-lv_campo_chave.
*                endif.
*
*                append wa_bsis_saida to it_bsis_saida.
*                exit.
*              endif.
*
*            else.
*              MOVE-CORRESPONDING wa_bsis_chv2 to wa_ZEFI0001 .
*              append wa_ZEFI0001 to it_ZEFI0001.
*            endif.
*          endloop.
*        ENDLOOP.
*
*      endif.
*    when '2'. "Centro de Custo, Ordem Interna, Sociedade Parceira,Tipo Movimento Consolidação
*      if not it_bsis_chv3[] is INITIAL.
*
*        loop at it_bsis_chv3 into wa_bsis_chv3.
*          loop at it_ZTFI0003 into wa_ZTFI0003 where bukrs = wa_bsis_chv3-bukrs.
*            if wa_bsis_chv3-hkont >= wa_ZTFI0003-sknr1 and wa_bsis_chv3-hkont <= wa_ZTFI0003-sknr2.
*              clear: wa_bsis.
*              read table it_bsis into wa_bsis with key BUKRS = wa_bsis_chv3-BUKRS
*                                                       HKONT = wa_bsis_chv3-HKONT
*                                                       KOSTL = wa_bsis_chv3-KOSTL
*                                                       AUFNR = wa_bsis_chv3-aufnr
*                                                       VBUND = wa_bsis_chv3-VBUND
*                                                       BEWAR = wa_bsis_chv3-BEWAR BINARY SEARCH.
*              if sy-subrc = 0.
*                MOVE-CORRESPONDING wa_bsis  to wa_bsis_saida.
*                move wa_bsis_chv3-dmbtr to wa_bsis_saida-dmbtr.
*                move  wa_ZTFI0003-BUZET to wa_bsis_saida-BUZET.
*                if wa_lancamento-txt_lan is initial.
*                  move wa_ZTFI0003-SGTXT to wa_bsis_saida-sgtxt.
*                else.
*                  move wa_lancamento-txt_lan to wa_bsis_saida-sgtxt.
*                endif.
*
*                if wa_lancamento-tipo_doc is initial.
*                  move wa_bsis-blart to wa_bsis_saida-blart.
*                else.
*                  move wa_lancamento-tipo_doc to wa_bsis_saida-blart.
*                endif.
*
*                clear: wa_t001.
*                READ TABLE it_t001 into wa_t001 with key bukrs = wa_lancamento-empresa
*                                                         XGSBE = 'X' BINARY SEARCH.
*                if sy-subrc = 0.
*                  concatenate  wa_bsis_saida-bukrs
*                               wa_bsis_saida-hkont
*                               wa_bsis_saida-gsber into wa_bsis_saida-lv_campo_chave.
*
*                ELSE.
*
*                  concatenate  wa_bsis_saida-bukrs
*                               wa_bsis_saida-hkont into wa_bsis_saida-lv_campo_chave.
*                endif.
*
*                append wa_bsis_saida to it_bsis_saida.
*                exit.
*              endif.
*
*            else.
*              MOVE-CORRESPONDING wa_bsis_chv3 to wa_ZEFI0001 .
*              append wa_ZEFI0001 to it_ZEFI0001.
*            endif.
*          endloop.
*        ENDLOOP.
*
*      ELSEif not it_bsis_chv4[] is INITIAL.
*
*        loop at it_bsis_chv4 into wa_bsis_chv4.
*          loop at it_ZTFI0003 into wa_ZTFI0003 where bukrs = wa_bsis_chv4-bukrs.
*            if wa_bsis_chv4-hkont >= wa_ZTFI0003-sknr1 and wa_bsis_chv4-hkont <= wa_ZTFI0003-sknr2.
*              clear: wa_bsis.
*              read table it_bsis into wa_bsis with key BUKRS = wa_bsis_chv4-BUKRS
*                                                       HKONT = wa_bsis_chv4-HKONT
*                                                       gsber = wa_bsis_chv4-gsber
*                                                       KOSTL = wa_bsis_chv4-KOSTL
*                                                       AUFNR = wa_bsis_chv4-aufnr
*                                                       VBUND = wa_bsis_chv4-VBUND
*                                                       BEWAR = wa_bsis_chv4-BEWAR BINARY SEARCH.
*              if sy-subrc = 0.
*                MOVE-CORRESPONDING wa_bsis  to wa_bsis_saida.
*                move wa_bsis_chv4-dmbtr to wa_bsis_saida-dmbtr.
*                move  wa_ZTFI0003-BUZET to wa_bsis_saida-BUZET.
*                if wa_lancamento-txt_lan is initial.
*                  move wa_ZTFI0003-SGTXT to wa_bsis_saida-sgtxt.
*                else.
*                  move wa_lancamento-txt_lan to wa_bsis_saida-sgtxt.
*                endif.
*
*                if wa_lancamento-tipo_doc is initial.
*                  move wa_bsis-blart to wa_bsis_saida-blart.
*                else.
*                  move wa_lancamento-tipo_doc to wa_bsis_saida-blart.
*                endif.
*
*                clear: wa_t001.
*                READ TABLE it_t001 into wa_t001 with key bukrs = wa_lancamento-empresa
*                                                         XGSBE = 'X' BINARY SEARCH.
*                if sy-subrc = 0.
*                  concatenate  wa_bsis_saida-bukrs
*                               wa_bsis_saida-hkont
*                               wa_bsis_saida-gsber into wa_bsis_saida-lv_campo_chave.
*
*                ELSE.
*
*                  concatenate  wa_bsis_saida-bukrs
*                               wa_bsis_saida-hkont into wa_bsis_saida-lv_campo_chave.
*                endif.
*
*                append wa_bsis_saida to it_bsis_saida.
*                exit.
*              endif.
*
*            else.
*              MOVE-CORRESPONDING wa_bsis_chv4 to wa_ZEFI0001 .
*              append wa_ZEFI0001 to it_ZEFI0001.
*            endif.
*          endloop.
*        ENDLOOP.
*
*      endif.
*  endcase.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  F_DYNPRO_FIELD_GET
*&---------------------------------------------------------------------*
FORM f_dynpro_field_get TABLES ft_dyn STRUCTURE rsdcf
                        USING f_dynnr TYPE sy-dynnr.

  DATA: lt_tline   TYPE TABLE OF tline,
        lt_dyn_aux TYPE TABLE OF rsdcf.

  DATA: lw_tstc TYPE tstc.

  lw_tstc-dypno = f_dynnr.
  lw_tstc-pgmna = sy-repid.

  CALL FUNCTION 'DYNPRO_FIELD_GET'
    EXPORTING
      dynpro           = lw_tstc-dypno
      program          = lw_tstc-pgmna
    TABLES
      dynp_fields      = lt_dyn_aux
      lines            = lt_tline
    EXCEPTIONS
      dynpro_not_found = 1
      OTHERS           = 2.

  DELETE lt_dyn_aux
   WHERE flg11         EQ '00'
      OR dynpro_fld    EQ space
      OR dynpro_fld(1) EQ '%'
      OR dynpro_fld    CS '-HIGH'.

  APPEND LINES OF lt_dyn_aux TO ft_dyn.

ENDFORM.                    " F_DYNPRO_FIELD_GET



*Abaixo o código para o botão de exibição de variante.

*&---------------------------------------------------------------------*
*&      Form  F_VARIANT_DISPLAY
*&---------------------------------------------------------------------*
FORM f_variant_display .

  DATA: l_variant TYPE rsvar-variant.

  PERFORM f_choose_variant CHANGING l_variant.

  IF l_variant NE space.

    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = sy-repid
        variant              = l_variant
      EXCEPTIONS
        variant_not_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
  ENDIF.

ENDFORM.                    " F_VARIANT_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  F_CHOOSE_VARIANT
*&---------------------------------------------------------------------*
FORM f_choose_variant  CHANGING f_variant TYPE rsvar-variant.

  DATA: l_vtext TYPE rsvar-vtext.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = sy-repid
      masked               = 'X'
    IMPORTING
      sel_variant          = f_variant
      sel_variant_text     = l_vtext
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.

ENDFORM.                    " F_CHOOSE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  PROCESSAR_FB01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processar_fb01 .

  DATA: l_documentheader  TYPE bapiache09,
        lt_accountgl      TYPE STANDARD TABLE OF bapiacgl09,
        wa_accountgl      TYPE bapiacgl09,
        lt_extension2     TYPE STANDARD TABLE OF bapiparex,
        l_extension2      TYPE  bapiparex,
        lv_cont           LIKE sy-tabix,
        l_accountgl       TYPE bapiacgl09,
        lt_currencyamount TYPE STANDARD TABLE OF bapiaccr09,
        l_currencyamount  TYPE bapiaccr09.

  CLEAR: l_documentheader,
           l_obj_type,
            l_obj_key,
            l_obj_sys,
            item, lv_cont  .

  REFRESH: lt_accountgl,
           lt_currencyamount,
           lt_return.

  DATA: lv_campo_chave(18) TYPE c,
        lv_lines           TYPE sy-tabix.



  SORT it_bsis_saida BY lv_campo_chave.


  CLEAR: lv_sum_dmbtr, lv_lines.

  DESCRIBE TABLE it_bsis_saida LINES  lv_lines.

  IF NOT it_bsis_saida[] IS INITIAL.
    SELECT *
      FROM zfi_mov_enc
         INTO TABLE  it_zfi_mov_enc.
*           for all entries in it_bsis_saida
*             where GL_ACCOUNT = it_bsis_saida-hkont.

    IF sy-subrc = 0.
      SORT it_zfi_mov_enc BY gl_account sign.
    ENDIF.
  ENDIF.



  IF wa_ztfi0003-buzet = 'X'. " com lançamento individual

    LOOP AT it_bsis_saida INTO wa_bsis_saida.

      IF sy-tabix <> 1.

        AT NEW lv_campo_chave.

          item = item + 1.
          PERFORM preencher_accountgl_part  TABLES lt_accountgl
                                          USING  wa_bsis_saida_aux.

          " Preenche com Dados Complementares da Partida/Contra Partida
          PERFORM preencher_currencyamount_part TABLES lt_currencyamount
                                           USING  wa_bsis_saida_aux.

          lv_cont = lv_cont + 1.

          IF lv_cont =  '100'.
            CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
              EXPORTING
                documentheader = l_documentheader
              IMPORTING
                obj_type       = l_obj_type
                obj_key        = l_obj_key
                obj_sys        = l_obj_sys
              TABLES
                accountgl      = lt_accountgl
                currencyamount = lt_currencyamount
                extension2     = lt_extension2
                return         = lt_return.


            IF sy-subrc = 0.

              PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              lv_lines = lv_lines - lv_cont.
              FREE: lv_cont, l_documentheader, lt_accountgl, lt_currencyamount, lt_return, item.

            ELSE.
              CLEAR: wa_zefi0005-linha.
              wa_zefi0005-linha = wa_zefi0005-linha + 1.
              wa_zefi0005-tipo   = 'W'.
              wa_zefi0005-msg   = 'Dados não encontrados'.
              APPEND wa_zefi0005 TO it_zefi0005.
            ENDIF.

          ENDIF.
        ENDAT.
      ENDIF.

      MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
      APPEND wa_zefi0001 TO it_zefi0001.
      item = item + 1.

*      lv_cont = lv_cont + 1.
      PERFORM preencher_header USING    wa_bsis_saida
                                    CHANGING l_documentheader.


      " Preenche com Dados da Partida/Contra Partida - Razão
      PERFORM preencher_accountgl  TABLES lt_accountgl
                                   USING  wa_bsis_saida.

      " Preenche com Dados Complementares da Partida/Contra Partida
      PERFORM preencher_currencyamount TABLES lt_currencyamount
                                       USING  wa_bsis_saida.


      lv_sum_dmbtr = lv_sum_dmbtr + wa_bsis_saida-dmbtr.
      wa_bsis_saida_aux = wa_bsis_saida.

      IF lv_lines <= '998'.
        AT LAST. "end of lv_campo_chave.
          item = item + 1.
          PERFORM preencher_accountgl_part  TABLES lt_accountgl
                                          USING  wa_bsis_saida_aux.

          " Preenche com Dados Complementares da Partida/Contra Partida
          PERFORM preencher_currencyamount_part TABLES lt_currencyamount
                                           USING  wa_bsis_saida_aux.

          CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
            EXPORTING
              documentheader = l_documentheader
            IMPORTING
              obj_type       = l_obj_type
              obj_key        = l_obj_key
              obj_sys        = l_obj_sys
            TABLES
              accountgl      = lt_accountgl
              currencyamount = lt_currencyamount
              extension2     = lt_extension2
              return         = lt_return.

          IF sy-subrc = 0.

            PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            FREE: lv_cont, l_documentheader, lt_accountgl, lt_currencyamount, lt_return.

          ELSE.
            CLEAR: wa_zefi0005-linha.
            wa_zefi0005-linha = wa_zefi0005-linha + 1.
            wa_zefi0005-tipo   = 'W'.
            wa_zefi0005-msg   = 'Dados não encontrados'.
            APPEND wa_zefi0005 TO it_zefi0005.
          ENDIF.
        ENDAT.
      ENDIF.
*      at last.
*        item = item + 1.
*        PERFORM preencher_accountgl_part  TABLES lt_accountgl
*                                        USING  wa_bsis_saida_aux.
*
*        " Preenche com Dados Complementares da Partida/Contra Partida
*        PERFORM preencher_currencyamount_part TABLES lt_currencyamount
*                                         USING  wa_bsis_saida_aux.
*
*
*
*      ENDAT.
    ENDLOOP.

  ELSE. " sem lançamento individual

    CLEAR: lv_cont.

    LOOP AT it_bsis_saida INTO wa_bsis_saida.

      lv_cont = lv_cont + 1.

      MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
      APPEND wa_zefi0001 TO it_zefi0001.
      item = item + 1.


      PERFORM preencher_header USING    wa_bsis_saida
                                    CHANGING l_documentheader.


      " Preenche com Dados da Partida/Contra Partida - Razão
      PERFORM preencher_accountgl  TABLES lt_accountgl
                                   USING  wa_bsis_saida.

      " Preenche com Dados Complementares da Partida/Contra Partida
      PERFORM preencher_currencyamount TABLES lt_currencyamount
                                       USING  wa_bsis_saida.


      lv_sum_dmbtr = lv_sum_dmbtr + wa_bsis_saida-dmbtr.
      wa_bsis_saida_aux = wa_bsis_saida.

      IF lv_cont = '998'.
*      at last. "end of lv_campo_chave.
        item = item + 1.
        PERFORM preencher_accountgl_part  TABLES lt_accountgl
                                        USING  wa_bsis_saida_aux.

        " Preenche com Dados Complementares da Partida/Contra Partida
        PERFORM preencher_currencyamount_part TABLES lt_currencyamount
                                         USING  wa_bsis_saida_aux.
*      ENDAT.


*        if sy-subrc = 0.

        CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
          EXPORTING
            documentheader = l_documentheader
          IMPORTING
            obj_type       = l_obj_type
            obj_key        = l_obj_key
            obj_sys        = l_obj_sys
          TABLES
            accountgl      = lt_accountgl
            currencyamount = lt_currencyamount
            extension2     = lt_extension2
            return         = lt_return.


        IF sy-subrc = 0.

          PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          lv_lines = lv_lines - lv_cont.
          FREE: lv_cont, l_documentheader, lt_accountgl, lt_currencyamount, lt_return, item.

        ELSE.
          CLEAR: wa_zefi0005-linha.
          wa_zefi0005-linha = wa_zefi0005-linha + 1.
          wa_zefi0005-tipo   = 'W'.
          wa_zefi0005-msg   = 'Dados não encontrados'.
          APPEND wa_zefi0005 TO it_zefi0005.
        ENDIF.
      ELSEIF lv_lines <= '998'.
        AT LAST. "end of lv_campo_chave.
          item = item + 1.
          PERFORM preencher_accountgl_part  TABLES lt_accountgl
                                          USING  wa_bsis_saida_aux.

          " Preenche com Dados Complementares da Partida/Contra Partida
          PERFORM preencher_currencyamount_part TABLES lt_currencyamount
                                           USING  wa_bsis_saida_aux.

          CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
            EXPORTING
              documentheader = l_documentheader
            IMPORTING
              obj_type       = l_obj_type
              obj_key        = l_obj_key
              obj_sys        = l_obj_sys
            TABLES
              accountgl      = lt_accountgl
              currencyamount = lt_currencyamount
              extension2     = lt_extension2
              return         = lt_return.

          IF sy-subrc = 0.

            PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            FREE: lv_cont, l_documentheader, lt_accountgl, lt_currencyamount, lt_return.

          ELSE.
            CLEAR: wa_zefi0005-linha.
            wa_zefi0005-linha = wa_zefi0005-linha + 1.
            wa_zefi0005-tipo   = 'W'.
            wa_zefi0005-msg   = 'Dados não encontrados'.
            APPEND wa_zefi0005 TO it_zefi0005.
          ENDIF.
        ENDAT.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  if sy-subrc = 0.
*
*    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
*      EXPORTING
*        documentheader = l_documentheader
*      IMPORTING
*        obj_type       = l_obj_type
*        obj_key        = l_obj_key
*        obj_sys        = l_obj_sys
*      TABLES
*        accountgl      = lt_accountgl
*        currencyamount = lt_currencyamount
*        EXTENSION2     = lt_EXTENSION2
*        return         = lt_return.
*
*
*    PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*  else.
*    wa_ZeFI0005-linha = wa_ZeFI0005-linha + 1.
*    wa_ZeFI0005-tipo   = 'W'.
*    wa_ZeFI0005-msg   = 'Dados não encontrados'.
*    append wa_ZeFI0005 to it_ZeFI0005.
*  endif.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_BSIS_SAIDA  text
*      <--P_L_DOCUMENTHEADER  text
*----------------------------------------------------------------------*
FORM preencher_header  USING   p_record TYPE ty_bsis_saida
                      CHANGING p_header TYPE bapiache09.


  DATA: l_data TYPE sy-datum.

  p_header-username   = sy-uname.
  p_header-header_txt = wa_lancamento-txt_cab_doc.
  p_header-comp_code  = p_record-bukrs.
  p_header-fisc_year  = wa_lancamento-exercicio.

*  IF NOT p_record-monat IS INITIAL.
  p_header-fis_period = wa_lancamento-periodo. "p_record-monat.
*  ENDIF.

  CLEAR: l_data.


  p_header-doc_date   = wa_lancamento-dt_doc.

  CLEAR: l_data.

  p_header-pstng_date = wa_lancamento-dt_lan.

  p_header-doc_type   = p_record-blart.
  p_header-ref_doc_no = 'ENCERRAMENTO'.
  p_header-bus_act    = 'RFBU'.
  p_header-acc_principle = 'L1'." p_record-acc_principle.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_ACCOUNTGL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ACCOUNTGL  text
*      -->P_WA_BSIS_SAIDA  text
*----------------------------------------------------------------------*
FORM preencher_accountgl  TABLES pt_accountgl STRUCTURE bapiacgl09
                          USING  p_record     TYPE ty_bsis_saida.


  DATA: l_accountgl  LIKE bapiacgl09.

  CLEAR l_accountgl.



  l_accountgl-itemno_acc     = item.



  IF wa_lancamento-txt_lan IS INITIAL.
    MOVE wa_ztfi0003-sgtxt TO l_accountgl-item_text.
  ELSE.
    MOVE wa_lancamento-txt_lan TO l_accountgl-item_text.
  ENDIF.

  l_accountgl-bus_area       = p_record-gsber.
  l_accountgl-costcenter     = p_record-kostl.
  l_accountgl-comp_code      = p_record-bukrs.
  l_accountgl-orderid        = p_record-aufnr.
  l_accountgl-cs_trans_t     = p_record-bewar.

  l_accountgl-gl_account     = p_record-hkont.

  IF NOT p_record-bewar IS INITIAL.
    l_accountgl-cs_trans_t     = p_record-bewar.
  ELSE.
    CLEAR: wa_zfi_mov_enc.
    READ TABLE it_zfi_mov_enc INTO wa_zfi_mov_enc WITH KEY gl_account = p_record-hkont
    sign = ' ' BINARY SEARCH.
    IF sy-subrc = 0.
      l_accountgl-cs_trans_t     = wa_zfi_mov_enc-trtyp.
    ELSE.
      CLEAR: wa_zfi_mov_enc.
      READ TABLE it_zfi_mov_enc INTO wa_zfi_mov_enc WITH KEY gl_account = p_record-hkont
      BINARY SEARCH.
      IF sy-subrc = 0.

        gv_dmbtr = p_record-dmbtr.
        gv_dmbtr = gv_dmbtr * -1.
        v_sign = sign( gv_dmbtr ) .

        IF v_sign < 0. "- se lançamento for negativo ( credito ) 50
          CLEAR: wa_zfi_mov_enc.
          READ TABLE it_zfi_mov_enc INTO wa_zfi_mov_enc WITH KEY gl_account = p_record-hkont
          sign = 'C ' BINARY SEARCH.

          IF sy-subrc = 0.      "se o movimento for credito (-) 50
            l_accountgl-cs_trans_t     = wa_zfi_mov_enc-trtyp.
          ENDIF.
        ELSE. "se lançamento for positivo ( debito ) 40
          CLEAR: wa_zfi_mov_enc.
          READ TABLE it_zfi_mov_enc INTO wa_zfi_mov_enc WITH KEY gl_account = p_record-hkont
          sign = 'D' BINARY SEARCH.

          IF sy-subrc = 0..  "se o movimento for debito (+) 40
            l_accountgl-cs_trans_t     = wa_zfi_mov_enc-trtyp.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

  APPEND l_accountgl TO pt_accountgl.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_CURRENCYAMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CURRENCYAMOUNT  text
*      -->P_WA_BSIS_SAIDA  text
*----------------------------------------------------------------------*
FORM preencher_currencyamount  TABLES pt_currencyamount STRUCTURE bapiaccr09
USING  p_record          TYPE ty_bsis_saida.

  DATA: l_amount    LIKE bapiaccr08, "Header for Line Item Currency Fields
        l_dmbtr(16) TYPE c.
  l_amount-itemno_acc   = item.
  l_amount-curr_type    = '00'.

  READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = p_record-bukrs BINARY SEARCH.

  l_amount-currency     = wa_t001-waers.


  l_amount-amt_doccur =  p_record-dmbtr.

  l_amount-amt_doccur = l_amount-amt_doccur * -1.

*  IF p_record-shkzg =  credito. "(H)
*    l_amount-amt_doccur = l_amount-amt_doccur * -1.
*  ENDIF.
  APPEND l_amount TO pt_currencyamount.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREPARAR_MSG_SAIDA_SUM_LAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl STRUCTURE bapiacgl09.

  DATA: ws_msg TYPE balm,
        vl_msg TYPE c LENGTH 200.

  DELETE t_msg WHERE msgtyp NE 'S' AND msgtyp NE 'E'.

  DATA: wa_accountgl TYPE bapiacgl09.

  CLEAR: wa_accountgl.

  SORT lt_accountgl BY gl_account comp_code.
  DELETE ADJACENT DUPLICATES FROM lt_accountgl COMPARING gl_account comp_code.



  LOOP AT lt_return INTO wa_return.

    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = wa_return-type.


    IF wa_return-type = 'S' AND wa_return-number = '605'.

      CONCATENATE 'Documento' wa_return-message_v2(10) 'criado' INTO wa_zefi0005-msg SEPARATED BY space.

      LOOP AT lt_accountgl INTO wa_accountgl.
        CLEAR: wa_bsis_saida, wa_ztfi0001.
        READ TABLE it_bsis_saida INTO wa_bsis_saida WITH KEY hkont = wa_accountgl-gl_account BINARY SEARCH.
        IF sy-subrc = 0.
          wa_ztfi0001-mandt = sy-mandt.
          wa_ztfi0001-bukrs = wa_lancamento-empresa.
          wa_ztfi0001-gjahr = wa_lancamento-exercicio.
          wa_ztfi0001-hkont = wa_accountgl-gl_account.
          wa_ztfi0001-monat = wa_lancamento-periodo.
          wa_ztfi0001-belnr = wa_return-message_v2(10).

          wa_ztfi0001-gsber = wa_bsis_saida-gsber.
          IF NOT wa_lancamento-etapa1 IS INITIAL.
            wa_ztfi0001-etapa = '1'.
          ELSE.
            wa_ztfi0001-etapa = '2'.
          ENDIF.
          wa_ztfi0001-gjhlv =  wa_ztfi0003-gjhlv.
          wa_ztfi0001-cpudt = sy-datum.
          wa_ztfi0001-usrnp = sy-uname.
          APPEND wa_ztfi0001 TO it_ztfi0001.
        ENDIF.
      ENDLOOP.

    ELSE.
      wa_zefi0005-msg   = wa_return-message.
    ENDIF.
    APPEND wa_zefi0005 TO it_zefi0005.

  ENDLOOP.

*  sort IT_ZTFI0003 by MANDT BUKRS ETAPA GJHLV GRPSK.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_ACCOUNTGL_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ACCOUNTGL  text
*      -->P_WA_BSIS_SAIDA  text
*----------------------------------------------------------------------*
FORM preencher_accountgl_part  TABLES pt_accountgl STRUCTURE bapiacgl09
                          USING  p_record     TYPE ty_bsis_saida.


  DATA: l_accountgl  LIKE bapiacgl09.

  CLEAR l_accountgl.

  DATA: lv_sknrd LIKE ztfi0003-sknrd.

  l_accountgl-itemno_acc     = item.

  CLEAR: wa_ztfi0003.
  LOOP AT it_ztfi0003 INTO wa_ztfi0003 WHERE  bukrs = p_record-bukrs AND
                                             sknr1 <= p_record-hkont AND
                                             sknr2 >= p_record-hkont.
    lv_sknrd = wa_ztfi0003-sknrd.
    EXIT.
  ENDLOOP.

  IF wa_lancamento-txt_lan IS INITIAL.
    MOVE wa_ztfi0003-sgtxt TO l_accountgl-item_text.
  ELSE.
    MOVE wa_lancamento-txt_lan TO l_accountgl-item_text.
  ENDIF.
  l_accountgl-trade_id       = p_record-vbund.
  l_accountgl-bus_area       = p_record-gsber.
  l_accountgl-comp_code      = p_record-bukrs.
  l_accountgl-cs_trans_t     =   p_record-bewar.
  l_accountgl-gl_account     = lv_sknrd.


  CLEAR: wa_zfi_mov_enc.
  READ TABLE it_zfi_mov_enc INTO wa_zfi_mov_enc WITH KEY gl_account = lv_sknrd
  sign = ' ' BINARY SEARCH.
  IF sy-subrc = 0.
    l_accountgl-cs_trans_t     = wa_zfi_mov_enc-trtyp.
  ELSE.
    CLEAR: wa_zfi_mov_enc.
    READ TABLE it_zfi_mov_enc INTO wa_zfi_mov_enc WITH KEY gl_account = lv_sknrd
       BINARY SEARCH.
    IF sy-subrc = 0.

      gv_dmbtr = p_record-dmbtr.
*      gv_dmbtr = gv_dmbtr * -1.
      v_sign = sign( gv_dmbtr ) .

      IF v_sign < 0. "- se lançamento for negativo ( credito ) 50
        CLEAR: wa_zfi_mov_enc.
        READ TABLE it_zfi_mov_enc INTO wa_zfi_mov_enc WITH KEY gl_account = lv_sknrd
        sign = 'C ' BINARY SEARCH.

        IF sy-subrc = 0.      "se o movimento for credito (-) 50
          l_accountgl-cs_trans_t     = wa_zfi_mov_enc-trtyp.
        ENDIF.
      ELSE. "se lançamento for positivo ( debito ) 40
        CLEAR: wa_zfi_mov_enc.
        READ TABLE it_zfi_mov_enc INTO wa_zfi_mov_enc WITH KEY gl_account = lv_sknrd
        sign = 'D' BINARY SEARCH.

        IF sy-subrc = 0..  "se o movimento for debito (+) 40
          l_accountgl-cs_trans_t     = wa_zfi_mov_enc-trtyp.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  APPEND l_accountgl TO pt_accountgl.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_CURRENCYAMOUNT_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_CURRENCYAMOUNT  text
*      -->P_WA_BSIS_SAIDA_AUX  text
*----------------------------------------------------------------------*
FORM preencher_currencyamount_part  TABLES pt_currencyamount STRUCTURE bapiaccr09
USING  p_record          TYPE ty_bsis_saida.

  DATA: l_amount    LIKE bapiaccr08, "Header for Line Item Currency Fields
        l_dmbtr(16) TYPE c.
  l_amount-itemno_acc   = item.
  l_amount-curr_type    = '00'.

  READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = p_record-bukrs BINARY SEARCH.

  l_amount-currency     = wa_t001-waers.
  l_amount-amt_doccur =  lv_sum_dmbtr. "p_record-dmbtr.
  CLEAR: lv_sum_dmbtr.

  APPEND l_amount TO pt_currencyamount.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_campos .

  IF wa_lancamento-empresa IS INITIAL.
    MESSAGE e001(00) WITH 'Campo Empresa é obrigatório!'.
  ELSEIF wa_lancamento-exercicio IS INITIAL.
    MESSAGE e001(00) WITH 'Campo Exercício é obrigatório!'.
  ELSEIF gv_cpm_regra IS INITIAL.
    MESSAGE e001(00) WITH 'Campo Regra de Sumariação é obrigatório!'.
  ELSEIF wa_lancamento-dt_lan IS INITIAL.
    MESSAGE e001(00) WITH 'Campo Data de Lançamento é obrigatório!'.
  ELSEIF wa_lancamento-dt_doc IS INITIAL.
    MESSAGE e001(00) WITH 'Campo Data de Documento é obrigatório!'.
  ELSEIF wa_lancamento-tipo_doc IS INITIAL.
    MESSAGE e001(00) WITH 'Campo Tipo de Documento é obrigatório!'.
  ELSEIF wa_lancamento-periodo IS INITIAL.
    MESSAGE e001(00) WITH 'Campo Periodo é obrigatório!'.
  ENDIF.

  DATA : rl_conta LIKE LINE OF contas.

  REFRESH it_ztfi0001.


*** Início - 30/09/2018 - Implementação de Authority-Check Projeto AGIR
  PERFORM f_authority_check TABLES it_ztfi0001[]
                            USING wa_lancamento-empresa
                                  '01'. "Criar
*** Fim    - 30/09/2018 - Implementação de Authority-Check Projeto AGIR


*  select *
*     from ZTFI0001
*      into table it_ZTFI0001_valida
*       where HKONT in contas and
*             MONAT = WA_LANCAMENTO-PERIODO and
*             USRND = ''.
*
*  if sy-subrc = 0.
*    loop at it_ZTFI0001_valida into wa_ZTFI0001_valida.
*      wa_ZeFI0005-linha = wa_ZeFI0005-linha + 1.
*      wa_ZeFI0005-tipo   = 'E'.
*      concatenate 'Conta' wa_ZTFI0001_valida-hkont 'já processada no doc.:' wa_ZTFI0001_valida-belnr into wa_ZeFI0005-msg  SEPARATED BY space.
*      append wa_ZeFI0005 to it_ZeFI0005.
*
*
*    ENDLOOP.
*
*    refresh it_ZTFI0001.
*
*    message e001(00) with 'Conta pertence a documento já processado!'.
*  endif.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INICIALIZA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inicializa_alv .
  FREE: it_zefi0005, it_zefi0001.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_saida_01 .

  REFRESH: it_bsis_saida.

  DELETE it_bsis_chv1 WHERE dmbtr = '0.00'.

  IF sy-subrc = 0 AND it_bsis_chv1[] IS INITIAL.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Registros com valores zerados, foram deletados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.

  SORT it_bsis BY bukrs hkont kostl aufnr.

  PERFORM saldo_contabil_01 . "encontrar o saldo contábil no exercício de cada conta contábil e suas divisões

  LOOP AT  it_bsis_chv1 INTO wa_bsis_chv1.
    CLEAR: wa_bsis.
    READ TABLE it_bsis INTO wa_bsis WITH KEY bukrs = wa_bsis_chv1-bukrs
                                             hkont = wa_bsis_chv1-hkont
                                             kostl = wa_bsis_chv1-kostl
                               aufnr = wa_bsis_chv1-aufnr BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_bsis  TO wa_bsis_saida.
    ENDIF.
    MOVE  wa_bsis_chv1-dmbtr TO wa_bsis_saida-dmbtr.
    MOVE  wa_ztfi0003-buzet TO wa_bsis_saida-buzet.
    IF wa_lancamento-txt_lan IS INITIAL.
      MOVE wa_ztfi0003-sgtxt TO wa_bsis_saida-sgtxt.
    ELSE.
      MOVE wa_lancamento-txt_lan TO wa_bsis_saida-sgtxt.
    ENDIF.
    MOVE wa_lancamento-tipo_doc TO wa_bsis_saida-blart.
    CLEAR: wa_t001.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa
                                             xgsbe = 'X' BINARY SEARCH.
    IF sy-subrc = 0.

      IF wa_ztfi0003-buzet = 'X'.
        CONCATENATE  wa_bsis_saida-bukrs
                     wa_bsis_saida-hkont
wa_bsis_saida-gsber wa_bsis_saida-kostl wa_bsis_saida-aufnr INTO wa_bsis_saida-lv_campo_chave.

      ELSE.
        CONCATENATE  wa_bsis_saida-bukrs
                                 wa_bsis_saida-hkont
                 wa_bsis_saida-gsber  INTO wa_bsis_saida-lv_campo_chave.
      ENDIF.

    ELSE.

      CONCATENATE  wa_bsis_saida-bukrs
                  wa_bsis_saida-hkont INTO wa_bsis_saida-lv_campo_chave.
    ENDIF.
    APPEND wa_bsis_saida TO it_bsis_saida.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_saida_02 .

  REFRESH: it_bsis_saida.

  DELETE it_bsis_chv2 WHERE dmbtr = '0.00'.

  IF sy-subrc = 0 AND it_bsis_chv1[] IS INITIAL.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Registros com valores zerados, foram deletados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.

  SORT it_bsis BY bukrs hkont gsber kostl aufnr.

  LOOP AT  it_bsis_chv2 INTO wa_bsis_chv2.
    CLEAR: wa_bsis.
    READ TABLE it_bsis INTO wa_bsis WITH KEY bukrs = wa_bsis_chv2-bukrs
                                             hkont = wa_bsis_chv2-hkont
                                             gsber = wa_bsis_chv2-gsber
                                             kostl = wa_bsis_chv2-kostl
                               aufnr = wa_bsis_chv2-aufnr BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_bsis  TO wa_bsis_saida.
    ENDIF.
    MOVE  wa_bsis_chv2-dmbtr TO wa_bsis_saida-dmbtr.
    MOVE  wa_ztfi0003-buzet TO wa_bsis_saida-buzet.
    IF wa_lancamento-txt_lan IS INITIAL.
      MOVE wa_ztfi0003-sgtxt TO wa_bsis_saida-sgtxt.
    ELSE.
      MOVE wa_lancamento-txt_lan TO wa_bsis_saida-sgtxt.
    ENDIF.
    MOVE wa_lancamento-tipo_doc TO wa_bsis_saida-blart.
    CLEAR: wa_t001.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa
                                             xgsbe = 'X' BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_ztfi0003-buzet = 'X'.
        CONCATENATE  wa_bsis_saida-bukrs
                     wa_bsis_saida-hkont
wa_bsis_saida-gsber wa_bsis_saida-kostl wa_bsis_saida-aufnr INTO wa_bsis_saida-lv_campo_chave.

      ELSE.
        CONCATENATE  wa_bsis_saida-bukrs
                                 wa_bsis_saida-hkont
                 wa_bsis_saida-gsber  INTO wa_bsis_saida-lv_campo_chave.
      ENDIF.
    ELSE.

      CONCATENATE  wa_bsis_saida-bukrs
                  wa_bsis_saida-hkont INTO wa_bsis_saida-lv_campo_chave.
    ENDIF.
    APPEND wa_bsis_saida TO it_bsis_saida.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA_03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_saida_03 .

  REFRESH: it_bsis_saida.

  DELETE it_bsis_chv3 WHERE dmbtr = '0.00'.

  IF sy-subrc = 0 AND it_bsis_chv1[] IS INITIAL.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Registros com valores zerados, foram deletados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.

  SORT it_bsis BY bukrs hkont kostl aufnr vbund bewar.

  LOOP AT  it_bsis_chv3 INTO wa_bsis_chv3.
    CLEAR: wa_bsis.
    READ TABLE it_bsis INTO wa_bsis WITH KEY bukrs = wa_bsis_chv3-bukrs
                                             hkont = wa_bsis_chv3-hkont
                                             kostl = wa_bsis_chv3-kostl
                                             aufnr = wa_bsis_chv3-aufnr
                                             vbund = wa_bsis_chv3-vbund
                               bewar = wa_bsis_chv3-bewar BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_bsis  TO wa_bsis_saida.
    ENDIF.
    MOVE  wa_bsis_chv3-dmbtr TO wa_bsis_saida-dmbtr.
    MOVE  wa_ztfi0003-buzet TO wa_bsis_saida-buzet.
    IF wa_lancamento-txt_lan IS INITIAL.
      MOVE wa_ztfi0003-sgtxt TO wa_bsis_saida-sgtxt.
    ELSE.
      MOVE wa_lancamento-txt_lan TO wa_bsis_saida-sgtxt.
    ENDIF.
    MOVE wa_lancamento-tipo_doc TO wa_bsis_saida-blart.
    CLEAR: wa_t001.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa
                                             xgsbe = 'X' BINARY SEARCH.
    IF sy-subrc = 0.

      IF wa_ztfi0003-buzet = 'X'.
        CONCATENATE  wa_bsis_saida-bukrs
                     wa_bsis_saida-hkont
wa_bsis_saida-gsber wa_bsis_saida-kostl wa_bsis_saida-aufnr INTO wa_bsis_saida-lv_campo_chave.

      ELSE.
        CONCATENATE  wa_bsis_saida-bukrs
                                 wa_bsis_saida-hkont
                 wa_bsis_saida-gsber  INTO wa_bsis_saida-lv_campo_chave.
      ENDIF.
    ELSE.

      CONCATENATE  wa_bsis_saida-bukrs
                  wa_bsis_saida-hkont INTO wa_bsis_saida-lv_campo_chave.
    ENDIF.
    APPEND wa_bsis_saida TO it_bsis_saida.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA_04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_saida_04 .
  REFRESH: it_bsis_saida.

  DELETE it_bsis_chv4 WHERE dmbtr = '0.00'.

  IF sy-subrc = 0 AND it_bsis_chv1[] IS INITIAL.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Registros com valores zerados, foram deletados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.

  SORT it_bsis BY bukrs hkont gsber kostl aufnr vbund bewar.

  LOOP AT  it_bsis_chv4 INTO wa_bsis_chv4.
    CLEAR: wa_bsis.
    READ TABLE it_bsis INTO wa_bsis WITH KEY bukrs = wa_bsis_chv4-bukrs
                                             hkont = wa_bsis_chv4-hkont
                                             gsber = wa_bsis_chv4-gsber
                                             kostl = wa_bsis_chv4-kostl
                                             aufnr = wa_bsis_chv4-aufnr
                                             vbund = wa_bsis_chv4-vbund
                               bewar = wa_bsis_chv4-bewar BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_bsis  TO wa_bsis_saida.
    ENDIF.
    MOVE  wa_bsis_chv4-dmbtr TO wa_bsis_saida-dmbtr.
    MOVE  wa_ztfi0003-buzet TO wa_bsis_saida-buzet.
    IF wa_lancamento-txt_lan IS INITIAL.
      MOVE wa_ztfi0003-sgtxt TO wa_bsis_saida-sgtxt.
    ELSE.
      MOVE wa_lancamento-txt_lan TO wa_bsis_saida-sgtxt.
    ENDIF.
    MOVE wa_lancamento-tipo_doc TO wa_bsis_saida-blart.
    CLEAR: wa_t001.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa
                                             xgsbe = 'X' BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_ztfi0003-buzet = 'X'.
        CONCATENATE  wa_bsis_saida-bukrs
                     wa_bsis_saida-hkont
wa_bsis_saida-gsber wa_bsis_saida-kostl wa_bsis_saida-aufnr INTO wa_bsis_saida-lv_campo_chave.

      ELSE.
        CONCATENATE  wa_bsis_saida-bukrs
                                 wa_bsis_saida-hkont
                 wa_bsis_saida-gsber  INTO wa_bsis_saida-lv_campo_chave.
      ENDIF.
    ELSE.

      CONCATENATE  wa_bsis_saida-bukrs
                  wa_bsis_saida-hkont INTO wa_bsis_saida-lv_campo_chave.
    ENDIF.
    APPEND wa_bsis_saida TO it_bsis_saida.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA_01_NIVEL2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_saida_01_nivel2 .
  REFRESH: it_bsis_saida.

  DELETE it_bsis_chv1 WHERE dmbtr = '0.00'.

  IF sy-subrc = 0 AND it_bsis_chv1[] IS INITIAL.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Registros com valores zerados, foram deletados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.

  SORT it_bsis_nivel2 BY bukrs hkont kostl aufnr.

  LOOP AT  it_bsis_chv1 INTO wa_bsis_chv1.
    CLEAR: wa_bsis.
    READ TABLE it_bsis_nivel2 INTO wa_bsis WITH KEY bukrs = wa_bsis_chv1-bukrs
                   hkont = wa_bsis_chv1-hkont
                   kostl = wa_bsis_chv1-kostl
           aufnr = wa_bsis_chv1-aufnr BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_bsis  TO wa_bsis_saida.
    ENDIF.
    MOVE  wa_bsis_chv1-dmbtr TO wa_bsis_saida-dmbtr.
    MOVE  wa_ztfi0003-buzet TO wa_bsis_saida-buzet.
    IF wa_lancamento-txt_lan IS INITIAL.
      MOVE wa_ztfi0003-sgtxt TO wa_bsis_saida-sgtxt.
    ELSE.
      MOVE wa_lancamento-txt_lan TO wa_bsis_saida-sgtxt.
    ENDIF.
    MOVE wa_lancamento-tipo_doc TO wa_bsis_saida-blart.
    CLEAR: wa_t001.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa
                                             xgsbe = 'X' BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_ztfi0003-buzet = 'X'.
        CONCATENATE  wa_bsis_saida-bukrs
                     wa_bsis_saida-hkont
wa_bsis_saida-gsber wa_bsis_saida-kostl wa_bsis_saida-aufnr INTO wa_bsis_saida-lv_campo_chave.

      ELSE.
        CONCATENATE  wa_bsis_saida-bukrs
                                 wa_bsis_saida-hkont
                 wa_bsis_saida-gsber  INTO wa_bsis_saida-lv_campo_chave.
      ENDIF.

    ELSE.

      CONCATENATE  wa_bsis_saida-bukrs
                  wa_bsis_saida-hkont INTO wa_bsis_saida-lv_campo_chave.
    ENDIF.
    APPEND wa_bsis_saida TO it_bsis_saida.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA_02_NIVEL2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_saida_02_nivel2 .
  REFRESH: it_bsis_saida.

  DELETE it_bsis_chv2 WHERE dmbtr = '0.00'.

  IF sy-subrc = 0 AND it_bsis_chv1[] IS INITIAL.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Registros com valores zerados, foram deletados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.

  SORT it_bsis_nivel2 BY bukrs hkont gsber kostl aufnr.

  LOOP AT  it_bsis_chv2 INTO wa_bsis_chv2.
    CLEAR: wa_bsis.
    READ TABLE it_bsis_nivel2 INTO wa_bsis WITH KEY bukrs = wa_bsis_chv2-bukrs
                   hkont = wa_bsis_chv2-hkont
                   gsber = wa_bsis_chv2-gsber
                   kostl = wa_bsis_chv2-kostl
           aufnr = wa_bsis_chv2-aufnr BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_bsis  TO wa_bsis_saida.
    ENDIF.
    MOVE  wa_bsis_chv2-dmbtr TO wa_bsis_saida-dmbtr.
    MOVE  wa_ztfi0003-buzet TO wa_bsis_saida-buzet.
    IF wa_lancamento-txt_lan IS INITIAL.
      MOVE wa_ztfi0003-sgtxt TO wa_bsis_saida-sgtxt.
    ELSE.
      MOVE wa_lancamento-txt_lan TO wa_bsis_saida-sgtxt.
    ENDIF.
    MOVE wa_lancamento-tipo_doc TO wa_bsis_saida-blart.
    CLEAR: wa_t001.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa
                                             xgsbe = 'X' BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_ztfi0003-buzet = 'X'.
        CONCATENATE  wa_bsis_saida-bukrs
                     wa_bsis_saida-hkont
wa_bsis_saida-gsber wa_bsis_saida-kostl wa_bsis_saida-aufnr INTO wa_bsis_saida-lv_campo_chave.

      ELSE.
        CONCATENATE  wa_bsis_saida-bukrs
                                 wa_bsis_saida-hkont
                 wa_bsis_saida-gsber  INTO wa_bsis_saida-lv_campo_chave.
      ENDIF.

    ELSE.

      CONCATENATE  wa_bsis_saida-bukrs
                  wa_bsis_saida-hkont INTO wa_bsis_saida-lv_campo_chave.
    ENDIF.
    APPEND wa_bsis_saida TO it_bsis_saida.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA_03_NIVEL3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_saida_03_nivel2 .
  REFRESH: it_bsis_saida.

  DELETE it_bsis_chv3 WHERE dmbtr = '0.00'.

  IF sy-subrc = 0 AND it_bsis_chv1[] IS INITIAL.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Registros com valores zerados, foram deletados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.

  SORT it_bsis_nivel2 BY bukrs hkont kostl aufnr vbund bewar.

  LOOP AT  it_bsis_chv3 INTO wa_bsis_chv3.
    CLEAR: wa_bsis.
    READ TABLE it_bsis_nivel2 INTO wa_bsis WITH KEY bukrs = wa_bsis_chv3-bukrs
                   hkont = wa_bsis_chv3-hkont
                   kostl = wa_bsis_chv3-kostl
                   aufnr = wa_bsis_chv3-aufnr
                   vbund = wa_bsis_chv3-vbund
           bewar = wa_bsis_chv3-bewar BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_bsis  TO wa_bsis_saida.
    ENDIF.
    MOVE  wa_bsis_chv3-dmbtr TO wa_bsis_saida-dmbtr.
    MOVE  wa_ztfi0003-buzet TO wa_bsis_saida-buzet.
    IF wa_lancamento-txt_lan IS INITIAL.
      MOVE wa_ztfi0003-sgtxt TO wa_bsis_saida-sgtxt.
    ELSE.
      MOVE wa_lancamento-txt_lan TO wa_bsis_saida-sgtxt.
    ENDIF.
    MOVE wa_lancamento-tipo_doc TO wa_bsis_saida-blart.
    CLEAR: wa_t001.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa
                                             xgsbe = 'X' BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_ztfi0003-buzet = 'X'.
        CONCATENATE  wa_bsis_saida-bukrs
                     wa_bsis_saida-hkont
wa_bsis_saida-gsber wa_bsis_saida-kostl wa_bsis_saida-aufnr INTO wa_bsis_saida-lv_campo_chave.

      ELSE.
        CONCATENATE  wa_bsis_saida-bukrs
                                 wa_bsis_saida-hkont
                 wa_bsis_saida-gsber  INTO wa_bsis_saida-lv_campo_chave.
      ENDIF.
    ENDIF.
    APPEND wa_bsis_saida TO it_bsis_saida.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA_04_NIVEL2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_saida_04_nivel2 .
  REFRESH: it_bsis_saida.

  DELETE it_bsis_chv4 WHERE dmbtr = '0.00'.

  IF sy-subrc = 0 AND it_bsis_chv1[] IS INITIAL.
    wa_zefi0005-linha = wa_zefi0005-linha + 1.
    wa_zefi0005-tipo   = 'W'.
    wa_zefi0005-msg   = 'Registros com valores zerados, foram deletados!'.
    APPEND wa_zefi0005 TO it_zefi0005.
  ENDIF.

  SORT it_bsis_nivel2 BY bukrs hkont gsber kostl aufnr vbund bewar.

  LOOP AT  it_bsis_chv4 INTO wa_bsis_chv4.
    CLEAR: wa_bsis.
    READ TABLE it_bsis_nivel2 INTO wa_bsis WITH KEY bukrs = wa_bsis_chv4-bukrs
                   hkont = wa_bsis_chv4-hkont
                   gsber = wa_bsis_chv4-gsber
                   kostl = wa_bsis_chv4-kostl
                   aufnr = wa_bsis_chv4-aufnr
                   vbund = wa_bsis_chv4-vbund
           bewar = wa_bsis_chv4-bewar BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_bsis  TO wa_bsis_saida.
    ENDIF.
    MOVE  wa_bsis_chv4-dmbtr TO wa_bsis_saida-dmbtr.
    MOVE  wa_ztfi0003-buzet TO wa_bsis_saida-buzet.
    IF wa_lancamento-txt_lan IS INITIAL.
      MOVE wa_ztfi0003-sgtxt TO wa_bsis_saida-sgtxt.
    ELSE.
      MOVE wa_lancamento-txt_lan TO wa_bsis_saida-sgtxt.
    ENDIF.
    MOVE wa_lancamento-tipo_doc TO wa_bsis_saida-blart.
    CLEAR: wa_t001.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_lancamento-empresa
                                             xgsbe = 'X' BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_ztfi0003-buzet = 'X'.
        CONCATENATE  wa_bsis_saida-bukrs
                     wa_bsis_saida-hkont
wa_bsis_saida-gsber wa_bsis_saida-kostl wa_bsis_saida-aufnr INTO wa_bsis_saida-lv_campo_chave.

      ELSE.
        CONCATENATE  wa_bsis_saida-bukrs
                                 wa_bsis_saida-hkont
                 wa_bsis_saida-gsber  INTO wa_bsis_saida-lv_campo_chave.
      ENDIF.
    ELSE.

      CONCATENATE  wa_bsis_saida-bukrs
                  wa_bsis_saida-hkont INTO wa_bsis_saida-lv_campo_chave.
    ENDIF.
    APPEND wa_bsis_saida TO it_bsis_saida.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALDO_CONTABIL_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saldo_contabil_01 .

*data: it_bsis_contabil type standard table of ty_bsis_contabil,
*      wa_bsis_contabil type ty_bsis_contabil.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_GLFLEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_glflext USING pt_bukrs    TYPE bsis-bukrs
                             pt_gjahr    LIKE gv_gjahr
                             pt_hkont    TYPE bsis-hkont
                    CHANGING pt_glt0     LIKE tty_glt0.


  FREE: pt_glt0.

  DATA: summe LIKE bapi1028_3-balance.


  DATA: it_glt0 TYPE fagl_t_glt0,
        gc_glt0 TYPE ty_glt0,
        gt_glt0 TYPE glt0.


  DATA: lv_gjahr LIKE bsis-gjahr.

  lv_gjahr  = pt_gjahr.

  CLEAR: summe.

  FREE: it_glt0.

  CALL FUNCTION 'FAGL_GET_GLT0'
    EXPORTING
      i_glt0_rldnr = '00'
      i_rrcty      = '0'
      i_rvers      = '001'
      i_bukrs      = pt_bukrs
      i_ryear      = lv_gjahr
      i_racct      = pt_hkont
      i_rpmax      = '016'
    IMPORTING
      et_glt0      = it_glt0.

  SORT it_glt0 BY bukrs ryear racct rbusa.

  DATA: lv_saldo_acum LIKE zari_cod_imposto-irrf,
        lv_plv_dt_fim LIKE bsis-bldat.


  CLEAR: gt_glt0.
  LOOP AT it_glt0 INTO gt_glt0.

    AT NEW rbusa.
      CLEAR : summe.
    ENDAT.

    PERFORM glt0_summ_hw USING gt_glt0
                               lv_saldo_acum
                               lv_plv_dt_fim
     CHANGING summe.

    AT END OF rbusa.
      CLEAR: gc_glt0.
      gc_glt0-bukrs = gt_glt0-bukrs.
      gc_glt0-ryear = gt_glt0-ryear.
      gc_glt0-racct = gt_glt0-racct.
      gc_glt0-rbusa = gt_glt0-rbusa.
      gc_glt0-rhlst = summe.

      APPEND gc_glt0 TO pt_glt0.

    ENDAT.


  ENDLOOP.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM GLT0_SUMM_HW                                             *
*---------------------------------------------------------------------*
*       Die Verkehrszahlen der GLT0-Einträge werden                   *
*       in Hauswährung = Buchungskreiswährung kummuliert              *
*---------------------------------------------------------------------*
*  -->  GLT0   : aktueller GLT0 Satz                                  *
*  -->  SUMME                                                         *
*---------------------------------------------------------------------*
FORM glt0_summ_hw USING VALUE(glt0)        LIKE glt0
                        saldo_acum  LIKE zari_cod_imposto-irrf
                        plv_dt_fim  LIKE bsis-bldat
               CHANGING summe       LIKE bapi1028_3-balance.

  DATA: glt0vary LIKE glt0-hsl01.

  DO 16 TIMES VARYING glt0vary FROM glt0-hsl01 NEXT glt0-hsl02.
    summe = summe + glt0vary.
  ENDDO.

ENDFORM.                    "GLT0_SUMM_HW
*&---------------------------------------------------------------------*
*&      Form  PROCESSAR_FB01_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processar_fb01_check .

  DATA: l_documentheader  TYPE bapiache09,
        lt_accountgl      TYPE STANDARD TABLE OF bapiacgl09,
        wa_accountgl      TYPE bapiacgl09,
        lt_extension2     TYPE STANDARD TABLE OF bapiparex,
        l_extension2      TYPE  bapiparex,
        lv_cont           LIKE sy-tabix,
        l_accountgl       TYPE bapiacgl09,
        lt_currencyamount TYPE STANDARD TABLE OF bapiaccr09,
        l_currencyamount  TYPE bapiaccr09.

  CLEAR: l_documentheader,
           l_obj_type,
            l_obj_key,
            l_obj_sys,
            item, lv_cont  .

  REFRESH: lt_accountgl,
           lt_currencyamount,
           lt_return.

  DATA: lv_campo_chave(18) TYPE c,
        lv_lines           TYPE sy-tabix.



  SORT it_bsis_saida BY lv_campo_chave.


  CLEAR: lv_sum_dmbtr, lv_lines.

  DESCRIBE TABLE it_bsis_saida LINES  lv_lines.

  IF NOT it_bsis_saida[] IS INITIAL.
    SELECT *
      FROM zfi_mov_enc
         INTO TABLE  it_zfi_mov_enc.


    IF sy-subrc = 0.
      SORT it_zfi_mov_enc BY gl_account sign.
    ENDIF.
  ENDIF.




  IF wa_ztfi0003-buzet = 'X'. " com lançamento individual

    LOOP AT it_bsis_saida INTO wa_bsis_saida.

      IF sy-tabix <> 1.

        AT NEW lv_campo_chave.

          item = item + 1.
          PERFORM preencher_accountgl_part  TABLES lt_accountgl
                                          USING  wa_bsis_saida_aux.

          " Preenche com Dados Complementares da Partida/Contra Partida
          PERFORM preencher_currencyamount_part TABLES lt_currencyamount
                                           USING  wa_bsis_saida_aux.

          lv_cont = lv_cont + 1.

          IF lv_cont =  '100'.

            CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
              EXPORTING
                documentheader = l_documentheader
              TABLES
                accountgl      = lt_accountgl
                currencyamount = lt_currencyamount
                extension2     = lt_extension2
                return         = lt_return.


            IF sy-subrc = 0.

              PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              lv_lines = lv_lines - lv_cont.
              FREE: lv_cont, l_documentheader, lt_accountgl, lt_currencyamount, lt_return, item.

            ELSE.
              CLEAR: wa_zefi0005-linha.
              wa_zefi0005-linha = wa_zefi0005-linha + 1.
              wa_zefi0005-tipo   = 'W'.
              wa_zefi0005-msg   = 'Dados não encontrados'.
              APPEND wa_zefi0005 TO it_zefi0005.
            ENDIF.

          ENDIF.
        ENDAT.
      ENDIF.

      MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
      APPEND wa_zefi0001 TO it_zefi0001.
      item = item + 1.


      PERFORM preencher_header USING    wa_bsis_saida
                                    CHANGING l_documentheader.


      " Preenche com Dados da Partida/Contra Partida - Razão
      PERFORM preencher_accountgl  TABLES lt_accountgl
                                   USING  wa_bsis_saida.

      " Preenche com Dados Complementares da Partida/Contra Partida
      PERFORM preencher_currencyamount TABLES lt_currencyamount
                                       USING  wa_bsis_saida.


      lv_sum_dmbtr = lv_sum_dmbtr + wa_bsis_saida-dmbtr.
      wa_bsis_saida_aux = wa_bsis_saida.

      IF lv_lines <= '998'.
        AT LAST. "end of lv_campo_chave.
          item = item + 1.
          PERFORM preencher_accountgl_part  TABLES lt_accountgl
                                          USING  wa_bsis_saida_aux.

          " Preenche com Dados Complementares da Partida/Contra Partida
          PERFORM preencher_currencyamount_part TABLES lt_currencyamount
                                           USING  wa_bsis_saida_aux.

          CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
            EXPORTING
              documentheader = l_documentheader
            TABLES
              accountgl      = lt_accountgl
              currencyamount = lt_currencyamount
              extension2     = lt_extension2
              return         = lt_return.

          IF sy-subrc = 0.

            PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            FREE: lv_cont, l_documentheader, lt_accountgl, lt_currencyamount, lt_return.

          ELSE.
            CLEAR: wa_zefi0005-linha.
            wa_zefi0005-linha = wa_zefi0005-linha + 1.
            wa_zefi0005-tipo   = 'W'.
            wa_zefi0005-msg   = 'Dados não encontrados'.
            APPEND wa_zefi0005 TO it_zefi0005.
          ENDIF.
        ENDAT.
      ENDIF.

    ENDLOOP.

  ELSE. " sem lançamento individual

    CLEAR: lv_cont.

    LOOP AT it_bsis_saida INTO wa_bsis_saida.

      lv_cont = lv_cont + 1.

      MOVE-CORRESPONDING wa_bsis_saida TO wa_zefi0001.
      APPEND wa_zefi0001 TO it_zefi0001.
      item = item + 1.


      PERFORM preencher_header USING    wa_bsis_saida
                                    CHANGING l_documentheader.


      " Preenche com Dados da Partida/Contra Partida - Razão
      PERFORM preencher_accountgl  TABLES lt_accountgl
                                   USING  wa_bsis_saida.

      " Preenche com Dados Complementares da Partida/Contra Partida
      PERFORM preencher_currencyamount TABLES lt_currencyamount
                                       USING  wa_bsis_saida.


      lv_sum_dmbtr = lv_sum_dmbtr + wa_bsis_saida-dmbtr.
      wa_bsis_saida_aux = wa_bsis_saida.

      IF lv_cont = '998'.

        item = item + 1.
        PERFORM preencher_accountgl_part  TABLES lt_accountgl
                                        USING  wa_bsis_saida_aux.

        " Preenche com Dados Complementares da Partida/Contra Partida
        PERFORM preencher_currencyamount_part TABLES lt_currencyamount
                                         USING  wa_bsis_saida_aux.


        CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
          EXPORTING
            documentheader = l_documentheader
          TABLES
            accountgl      = lt_accountgl
            currencyamount = lt_currencyamount
            extension2     = lt_extension2
            return         = lt_return.


        IF sy-subrc = 0.

          PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          lv_lines = lv_lines - lv_cont.
          FREE: lv_cont, l_documentheader, lt_accountgl, lt_currencyamount, lt_return, item.

        ELSE.
          CLEAR: wa_zefi0005-linha.
          wa_zefi0005-linha = wa_zefi0005-linha + 1.
          wa_zefi0005-tipo   = 'W'.
          wa_zefi0005-msg   = 'Dados não encontrados'.
          APPEND wa_zefi0005 TO it_zefi0005.
        ENDIF.
      ELSEIF lv_lines <= '998'.
        AT LAST. "end of lv_campo_chave.
          item = item + 1.
          PERFORM preencher_accountgl_part  TABLES lt_accountgl
                                          USING  wa_bsis_saida_aux.

          " Preenche com Dados Complementares da Partida/Contra Partida
          PERFORM preencher_currencyamount_part TABLES lt_currencyamount
                                           USING  wa_bsis_saida_aux.

          CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
            EXPORTING
              documentheader = l_documentheader
            TABLES
              accountgl      = lt_accountgl
              currencyamount = lt_currencyamount
              extension2     = lt_extension2
              return         = lt_return.

          IF sy-subrc = 0.

            PERFORM f_preparar_msg_saida_sum_lan TABLES lt_accountgl.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            FREE: lv_cont, l_documentheader, lt_accountgl, lt_currencyamount, lt_return.

          ELSE.
            CLEAR: wa_zefi0005-linha.
            wa_zefi0005-linha = wa_zefi0005-linha + 1.
            wa_zefi0005-tipo   = 'W'.
            wa_zefi0005-msg   = 'Dados não encontrados'.
            APPEND wa_zefi0005 TO it_zefi0005.
          ENDIF.
        ENDAT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*** Início - 30/09/2018 - Implementação de Authority-Check Projeto AGIR
*---------------------------------------------------------------------*
*      Form  f_authority_check
*---------------------------------------------------------------------*
FORM f_authority_check TABLES p_it_ztfi0001 STRUCTURE ztfi0001
                       USING  p_empresa     TYPE bukrs      "Empresa
                              p_atividade   TYPE zag_campo.

*** Constantes
  CONSTANTS:
    lc_f_bkpf_buk TYPE xuobject  VALUE 'F_BKPF_BUK'.

*** Variáveis
  DATA:
    lv_objeto   TYPE xuobject,
    lv_campo1   TYPE zag_campo,
    lv_campo2   TYPE zag_campo,
    lv_mensagem TYPE string,
    lw_ztfi0001 TYPE ztfi0001,
    ti_ztfi0001 TYPE STANDARD TABLE OF ztfi0001.

*** Preenche variáveis
  lv_objeto = lc_f_bkpf_buk.

  lv_campo2 = p_atividade.

  IF NOT p_empresa IS INITIAL.

    lv_campo1 = p_empresa.

*** Executa função padrão para verificação de autorizações
    CALL FUNCTION 'ZAG_F_AUTHORITY'
      EXPORTING
        i_tcode    = sy-tcode
        i_xuobject = lv_objeto
        i_campo1   = lv_campo1
        i_campo2   = lv_campo2
      IMPORTING
        e_mensagem = lv_mensagem.

*** Caso o usuário não tenha autorização, exibe mensagem e interrompe o processamento
    IF lv_mensagem IS NOT INITIAL.
      MESSAGE e000(zfi0) WITH lv_mensagem.
    ENDIF.

  ELSE.

    ti_ztfi0001[] = p_it_ztfi0001[].

    SORT ti_ztfi0001 BY bukrs.
    DELETE ADJACENT DUPLICATES FROM ti_ztfi0001 COMPARING bukrs.

    LOOP AT ti_ztfi0001 INTO lw_ztfi0001.

      lv_campo1 = lw_ztfi0001-bukrs.

***   Executa função padrão para verificação de autorizações
      CALL FUNCTION 'ZAG_F_AUTHORITY'
        EXPORTING
          i_tcode    = sy-tcode
          i_xuobject = lv_objeto
          i_campo1   = lv_campo1
          i_campo2   = lv_campo2
        IMPORTING
          e_mensagem = lv_mensagem.

***   Caso o usuário não tenha autorização, exibe mensagem e interrompe o processamento
      IF lv_mensagem IS NOT INITIAL.
        REFRESH p_it_ztfi0001.
        MESSAGE e000(zfi0) WITH lv_mensagem.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*** Fim    - 30/09/2018 - Implementação de Authority-Check Projeto AGIR
