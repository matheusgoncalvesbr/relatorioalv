*&---------------------------------------------------------------------*
*& Report ZRELATORIO1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelatorio1.
" Tabelas transparentes
TABLES: bseg,
        bkpf,
        bsid,
        kna1.

" Tipos de tabelas
TYPES: BEGIN OF ty_bsid,
         flag(1) TYPE c,
         bukrs   TYPE bsid-bukrs,
         kunnr   TYPE bsid-kunnr,
         belnr   TYPE bsid-belnr,
         budat   TYPE bsid-budat,
         blart   TYPE bsid-blart,
         xblnr   TYPE bsid-xblnr,
         zfbdt   TYPE bsid-zfbdt,
         zbd1t   TYPE bsid-zbd1t,
         dmbtr   TYPE bsid-dmbtr,
       END OF ty_bsid.

TYPES: BEGIN OF ty_kna1,
         name1 TYPE kna1-name1,
       END OF ty_kna1.

" Tabelas internas
DATA: t_bsid     TYPE TABLE OF ty_bsid,
      t_bkpf     TYPE TABLE OF bkpf,
      t_saida    TYPE TABLE OF zsrlt1,
      t_fieldcat TYPE lvc_t_fcat,
      t_header   TYPE slis_t_listheader,
      t_ordena   TYPE lvc_t_sort.

" Variaveis
DATA:
  v_name         TYPE rs38l_fnam,
  v_total_linhas TYPE i,
  v_linha_selec  TYPE i,
  v_princ        TYPE bsid-dmbtr,
  v_tprinc       TYPE bsid-dmbtr,
  v_juros        TYPE bsid-dmbtr,
  v_tjuros       TYPE bsid-dmbtr,
  v_varia        TYPE bsid-dmbtr,
  v_tvaria       TYPE bsid-dmbtr,
  v_multa        TYPE bsid-dmbtr,
  v_tmulta       TYPE bsid-dmbtr,
  v_desco        TYPE bsid-dmbtr,
  v_tdesco       TYPE bsid-dmbtr,
  v_venci        TYPE bsid-dmbtr,
  v_tvenci       TYPE bsid-dmbtr,
  v_avenc        TYPE bsid-dmbtr,
  v_tavenc       TYPE bsid-dmbtr,
  v_saldo        TYPE bsid-dmbtr,
  v_tsaldo       TYPE bsid-dmbtr.

" Work areas
DATA: w_saida    TYPE zsrlt1,
      w_bsid     TYPE ty_bsid,
      w_bkpf     TYPE bkpf,
      w_kna1     TYPE ty_kna1,
      w_fieldcat TYPE lvc_s_fcat,
      w_layout   TYPE lvc_s_layo,
      w_header   TYPE slis_listheader,
      w_ordena   TYPE lvc_s_sort.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_bukrs FOR bsid-bukrs,
                s_belnr FOR bsid-belnr,
                s_kunnr FOR bsid-kunnr,
                s_budat FOR bsid-budat.
SELECTION-SCREEN END OF BLOCK b01.

START-OF-SELECTION.

  PERFORM f_seleciona_dados.

  PERFORM f_monta_tabela.

  IF t_saida[] IS NOT INITIAL.

    PERFORM f_monta_alv.

    PERFORM f_smartforms.

  ELSE.
    MESSAGE 'Sem dados' TYPE 'I'.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  SELECT bukrs kunnr belnr budat blart
         xblnr zfbdt zbd1t dmbtr
    FROM bsid
    INTO CORRESPONDING FIELDS OF TABLE t_bsid
      WHERE   bukrs IN s_bukrs
        AND   belnr IN s_belnr
        AND   kunnr IN s_kunnr
        AND   budat IN s_budat
        AND   xblnr NE ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_TABELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_tabela .
  LOOP AT t_bsid INTO w_bsid.

    CLEAR w_saida.

    SELECT SINGLE *
      FROM bkpf
      INTO w_bkpf
      WHERE belnr EQ w_bsid-belnr
        AND stblg EQ ''
        AND xreversal EQ ''.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE name1
      FROM kna1
      INTO w_kna1
      WHERE kunnr EQ w_bsid-kunnr.

    MOVE-CORRESPONDING w_bsid TO w_saida.

    w_saida-clien = w_bsid-kunnr.

    w_saida-name1 = w_kna1-name1.

    w_saida-belnr = w_bkpf-belnr.

    w_saida-zfbdt = w_bsid-zfbdt + w_bsid-zbd1t.

    w_saida-saldo = w_saida-dmbtr + w_saida-juros + w_saida-varia
                    + w_saida-multa - w_saida-desco.

    IF w_saida-saldo LT sy-datum.
      w_saida-venci = w_saida-saldo.
    ENDIF.

    IF w_saida-saldo GT sy-datum.
      w_saida-avenc = w_saida-saldo.
    ENDIF.

    APPEND w_saida TO t_saida.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_monta_alv .

  PERFORM f_define_fieldcat.

  PERFORM f_ordena.

  PERFORM f_layout.

  PERFORM f_imprimi_alv.

ENDFORM.
FORM f_define_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE    =
      i_structure_name   = 'ZSRLT1'
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER =
      i_internal_tabname = 'T_SAIDA'
    CHANGING
      ct_fieldcat        = t_fieldcat
* EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR      = 2
*     OTHERS             = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  w_fieldcat-checkbox = 'X'.
  w_fieldcat-edit = 'X'.
  w_fieldcat-no_out = 'X'.

  MODIFY t_fieldcat FROM w_fieldcat INDEX 2 TRANSPORTING no_out.
  MODIFY t_fieldcat FROM w_fieldcat INDEX sy-tabix TRANSPORTING checkbox edit.

ENDFORM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout .
  w_layout-zebra = 'X'. "Deixa a linhas da tabela em azul e cinza
  w_layout-no_rowmark = 'X'.
  w_layout-cwidth_opt = 'X'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIMI_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprimi_alv .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
*     I_INTERFACE_CHECK      = ' '
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
      i_callback_program     = sy-repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
      i_callback_top_of_page = 'F_CABECALHO'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME       =
*     I_BACKGROUND_ID        = ' '
*     I_GRID_TITLE           =
*     I_GRID_SETTINGS        =
      is_layout_lvc          = w_layout
      it_fieldcat_lvc        = t_fieldcat
*     IT_EXCLUDING           =
*     IT_SPECIAL_GROUPS_LVC  =
      it_sort_lvc            = t_ordena
*     IT_FILTER_LVC          =
*     IT_HYPERLINK           =
*     IS_SEL_HIDE            =
*     I_DEFAULT              = 'X'
*     I_SAVE                 = ' '
*     IS_VARIANT             =
*     IT_EVENTS              =
*     IT_EVENT_EXIT          =
*     IS_PRINT_LVC           =
*     IS_REPREP_ID_LVC       =
*     I_SCREEN_START_COLUMN  = 0
*     I_SCREEN_START_LINE    = 0
*     I_SCREEN_END_COLUMN    = 0
*     I_SCREEN_END_LINE      = 0
*     I_HTML_HEIGHT_TOP      =
*     I_HTML_HEIGHT_END      =
*     IT_ALV_GRAPHICS        =
*     IT_EXCEPT_QINFO_LVC    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER =
    TABLES
      t_outtab               = t_saida
*   EXCEPTIONS
*     PROGRAM_ERROR          = 1
*     OTHERS                 = 2
    .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.

FORM f_cabecalho.

  CLEAR w_header.
  REFRESH t_header.

  DESCRIBE TABLE t_saida LINES v_total_linhas.

  CLEAR: v_linha_selec, v_tprinc, v_tjuros,
         v_tvaria, v_tmulta, v_tdesco,
         v_tvenci, v_tavenc, v_tsaldo.

  LOOP AT t_saida INTO w_saida WHERE flag EQ 'X'.

    CLEAR:  v_juros, v_varia, v_multa,
            v_desco, v_venci, v_avenc, v_saldo.

    v_linha_selec = v_linha_selec + 1.

    v_princ  = w_saida-dmbtr.
    v_tprinc = v_tprinc + v_princ.

    v_juros  = w_saida-juros.
    v_tjuros = v_tjuros + v_juros.

    v_varia  = w_saida-varia.
    v_tvaria = v_tvaria + v_varia.

    v_multa  = w_saida-multa.
    v_tmulta = v_tmulta + v_multa.

    v_desco  = w_saida-desco.
    v_tdesco = v_tdesco + v_desco.

    v_venci  = w_saida-venci.
    v_tvenci = v_tvenci + v_venci.

    v_avenc  = w_saida-avenc.
    v_tavenc = v_tavenc + v_avenc.

    v_saldo  = w_saida-saldo.
    v_tsaldo = v_tsaldo + v_saldo.

  ENDLOOP.

  DATA: v_vprinc(13) TYPE c,
        v_vjuros(13) TYPE c,
        v_vvaria(13) TYPE c,
        v_vmulta(13) TYPE c,
        v_vdesco(13) TYPE c,
        v_vvenci(13) TYPE c,
        v_vavenc(13) TYPE c,
        v_vsaldo(13) TYPE c.

  w_header-typ  = 'H'.
  w_header-info = TEXT-007.
  APPEND w_header TO t_header.

  w_header-typ  = 'S'.
  w_header-info = v_linha_selec && ' - ' && v_total_linhas.
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  WRITE v_tprinc TO v_vprinc CURRENCY 'BRL'.
  w_header-info = 'VALOR PRINCIPAL:' && v_vprinc.
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  w_header-info = 'JUROS:' && v_tjuros.
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  w_header-info = 'VARIAÇÃO:' && v_tvaria.
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  w_header-info = 'MULTA:' && v_tmulta.
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  w_header-info = 'DESCONTO:' && v_tdesco.
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  WRITE v_tvenci TO v_vvenci CURRENCY 'BRL'.
  w_header-info = 'VENCIDOS:' && v_vvenci.
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  w_header-info = 'A VENCER:' && v_tavenc.
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  WRITE v_tsaldo TO v_vsaldo CURRENCY 'BRL'.
  w_header-info = 'SALDO:' && v_vsaldo.
  APPEND w_header TO t_header.

* função LOGOTIPO
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_header
      i_logo             = 'ENJOYSAP_LOGO'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SMARTFORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_smartforms .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname = 'ZFORM_RLT1'
    IMPORTING
      fm_name  = v_name.

  IF NOT v_name IS INITIAL.

    CALL FUNCTION v_name
*      EXPORTING
*       ARCHIVE_INDEX    =
*       ARCHIVE_INDEX_TAB          =
*       ARCHIVE_PARAMETERS         =
*       CONTROL_PARAMETERS         =
*       MAIL_APPL_OBJ    =
*       MAIL_RECIPIENT   =
*       MAIL_SENDER      =
*       OUTPUT_OPTIONS   =
*       USER_SETTINGS    = 'X'
*        CABE             = W_CABE
*     IMPORTING
*       DOCUMENT_OUTPUT_INFO       =
*       JOB_OUTPUT_INFO  =
*       JOB_OUTPUT_OPTIONS         =
      TABLES
        relatorio        = t_saida
        cliente          = t_saida
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ORDENA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ordena .
  CLEAR w_ordena.

  w_ordena-fieldname = 'KUNNR'.
  w_ordena-up = 'X'.
  APPEND w_ordena TO t_ordena.

ENDFORM.
