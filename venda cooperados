*&---------------------------------------------------------------------*
*& Report ZRELATORIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelatorio.
*Tabelas transparentes
TABLES: vbak,
        mara,
        kna1,
        vbap,
        vbkd,
        t023t,
        vbpa,
        tvgrt,
        j_1bnfdoc,
        j_1bnflin,
        vbrk,
        vbrp,
        konv.

" Tipos para tabela VBAP
TYPES: BEGIN OF ty_vbap,
         erdat  TYPE vbak-erdat,
         vbeln  TYPE vbak-vbeln,
         auart  TYPE vbak-auart,
         vkorg  TYPE vbak-vkorg,
         kunnr  TYPE vbak-kunnr,
         vkgrp  TYPE vbak-vkgrp,
         knumv  TYPE vbak-knumv,
         vbeln1 TYPE vbap-vbeln,
         posnr  TYPE vbap-posnr,
         matnr  TYPE vbap-matnr,
         matkl  TYPE vbap-matkl,
         werks  TYPE vbap-werks,
         zmeng  TYPE vbap-zmeng,
         matnr1 TYPE mara-matnr,
         matkl1 TYPE mara-matkl,
         spart  TYPE mara-spart,
       END OF ty_vbap.

" Tipos de tabela para KNA1
TYPES: BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         sortl TYPE kna1-sortl,
       END OF ty_kna1.

" Tipos de tabela para TVGRT
TYPES: BEGIN OF ty_tvgrt,
         vkgrp TYPE tvgrt-vkgrp,
         bezei TYPE tvgrt-bezei,
       END OF ty_tvgrt.

" Tipos de tabela para VBKD
TYPES: BEGIN OF ty_vbkd,
         bstkd TYPE vbkd-bstkd,
         vbeln TYPE vbkd-vbeln,
       END OF ty_vbkd.

" Tipos de tabela para T023T
TYPES: BEGIN OF ty_t023t,
         matkl TYPE t023t-matkl,
         wgbez TYPE t023t-wgbez,
       END OF ty_t023t.

" Tipos de tabela para VBPA
TYPES: BEGIN OF ty_vbpa,
         parvw TYPE vbpa-parvw,
         kunnr TYPE vbpa-kunnr,
       END OF ty_vbpa.

" Tipos de tabela para VBRK
TYPES: BEGIN OF ty_vbrk,
         vbeln TYPE vbrk-vbeln,
         fksto TYPE vbrk-fksto,
       END OF ty_vbrk.

" Tipos de tabela para VBRP
TYPES: BEGIN OF ty_vbrp,
         posnr TYPE vbrp-posnr,
         vbeln TYPE vbrp-vbeln,
       END OF ty_vbrp.

" Tipos de tabela para J_1BNFLIN
TYPES: BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
       END OF ty_j_1bnflin.

" Tipos de tabela para J_1BNFDOC
TYPES: BEGIN OF ty_j_1bnfdoc,
         nfenum TYPE j_1bnfdoc-nfenum,
       END OF ty_j_1bnfdoc.

" Tipo de tabela para VBFA
TYPES: BEGIN OF ty_vbfa,
         vbeln TYPE vbfa-vbeln,
         posnn TYPE vbfa-posnn,
         rfmng TYPE vbfa-rfmng,
       END OF ty_vbfa.

" Tipos de tabela para VBRP1
TYPES: BEGIN OF ty_vbrp1,
         posnr TYPE vbrp-posnr,
         vbeln TYPE vbrp-vbeln,
       END OF ty_vbrp1.


" Tipos de tabela para KONV
TYPES: BEGIN OF ty_konv,
         knumv TYPE konv-knumv,
         kposn TYPE konv-kposn,
         kbetr TYPE konv-kbetr,
         kwert TYPE konv-kwert,
       END OF ty_konv.

* Tabelas internas
DATA: t_saida     TYPE TABLE OF zsrlt,
      t_vbap      TYPE TABLE OF ty_vbap,
      t_kna1      TYPE TABLE OF ty_kna1,
      t_tvgrt     TYPE TABLE OF ty_tvgrt,
      t_vbkd      TYPE TABLE OF ty_vbkd,
      t_t023t     TYPE TABLE OF ty_t023t,
      t_vbpa      TYPE TABLE OF ty_vbpa,
      t_vbrk      TYPE TABLE OF ty_vbrk,
      t_vbrp      TYPE TABLE OF ty_vbrp,
      t_j_1bnflin TYPE TABLE OF ty_j_1bnflin,
      t_j_1bnfdoc TYPE TABLE OF ty_j_1bnfdoc,
      t_vbfa      TYPE TABLE OF ty_vbfa,
      t_vbrp1     TYPE TABLE OF ty_vbrp1,
      t_konv      TYPE TABLE OF ty_konv,
      t_fieldcat  TYPE slis_t_fieldcat_alv, "Tipo de grupo
      t_sort      TYPE slis_t_sortinfo_alv,
      t_header    TYPE slis_t_listheader,
      v_name      TYPE rs38l_fnam.

* WORK AREA
DATA: w_saida      TYPE  zsrlt,
      w_vbap       TYPE  ty_vbap,
      w_kna1       TYPE  ty_kna1,
      w_tvgrt      TYPE  ty_tvgrt,
      w_vbkd       TYPE  ty_vbkd,
      w_t023t      TYPE  ty_t023t,
      w_vbpa       TYPE  ty_vbpa,
      w_vbrk       TYPE  ty_vbrk,
      w_vbrp       TYPE  ty_vbrp,
      w_vbfa       TYPE  ty_vbfa,
      w_vbrp1      TYPE  ty_vbrp1,
      w_j_1bnflin  TYPE  ty_j_1bnflin,
      w_j_1bnflin1 TYPE  ty_j_1bnflin,
      w_j_1bnfdoc  TYPE  ty_j_1bnfdoc,
      w_j_1bnfdoc1 TYPE  ty_j_1bnfdoc,
      w_konv       TYPE  ty_konv,
      w_fieldcat   TYPE  slis_fieldcat_alv,
      w_sort       TYPE  slis_sortinfo_alv,
      w_layout     TYPE  slis_layout_alv,
      w_header     TYPE  slis_listheader.

* Tela de Seleção
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_erdat     FOR vbak-erdat OBLIGATORY,
                s_auart     FOR vbak-auart OBLIGATORY,
                s_vbeln     FOR vbak-vbeln,
                s_vkorg     FOR vbak-vkorg,
                s_matkl     FOR mara-matkl,
                s_kunnr     FOR kna1-kunnr,
                s_werks     FOR vbap-werks.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_anali RADIOBUTTON GROUP gr1,
            p_sinte RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b02.

START-OF-SELECTION.

  PERFORM f_seleciona_dados.

  PERFORM f_tabela_saida.

  PERFORM f_alv.

  PERFORM f_smartforms.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .
* Seleção de dados VBAK, VBAP e MARA
  SELECT a~erdat a~vbeln a~auart a~vkorg
         a~kunnr a~vkgrp a~knumv b~vbeln
         b~posnr b~matnr b~matkl b~werks
         b~zmeng c~matnr c~matkl c~spart
    FROM vbak AS a
    INNER JOIN vbap AS b
    ON a~vbeln = b~vbeln
    INNER JOIN mara AS c
    ON b~matnr = c~matnr
    INTO TABLE  t_vbap
      WHERE a~erdat   IN  s_erdat
        AND a~auart   IN  s_auart
        AND a~vbeln   IN  s_vbeln
        AND a~vkorg   IN  s_vkorg
        AND c~matkl   IN  s_matkl
        AND b~werks   IN  s_werks
        AND b~abgru   EQ   ''.

* Seleção de dados VBKD
  SELECT bstkd vbeln
    FROM vbkd
    INTO TABLE t_vbkd
    FOR ALL ENTRIES IN t_vbap
    WHERE vbeln EQ t_vbap-vbeln.

* Seleção de dados T023T
  SELECT matkl wgbez
    FROM t023t
    INTO TABLE t_t023t
    FOR ALL ENTRIES IN t_vbap
    WHERE matkl EQ t_vbap-matkl.

* Seleção de dados KNA1
  SELECT kunnr name1 sortl
    FROM kna1
    INTO TABLE t_kna1
    FOR ALL ENTRIES IN t_vbap
    WHERE kunnr EQ t_vbap-kunnr.

* Seleção de dados TVGRT
  SELECT vkgrp bezei
    FROM tvgrt
    INTO TABLE t_tvgrt
    FOR ALL ENTRIES IN t_vbap
    WHERE spras EQ 'P'
      AND vkgrp EQ  t_vbap-vkgrp.

* Seleção de dados VBPA
  SELECT parvw kunnr
    FROM vbpa
    INTO TABLE t_vbpa
    FOR ALL ENTRIES IN t_vbap
    WHERE vbeln EQ t_vbap-vbeln
      AND parvw EQ 'WE'.

* Seleção de dados KONV
  SELECT knumv kposn kbetr kwert
   FROM konv
   INTO TABLE t_konv
   FOR ALL ENTRIES IN t_vbap
   WHERE knumv EQ t_vbap-vbeln
     AND kposn EQ t_vbap-posnr
     AND kschl EQ 'ICMI'.

* Nota Fiscal Mãe
*  SELECT POSNR VBELN
*    FROM VBRP
*    INTO TABLE T_VBRP
*    FOR ALL ENTRIES IN T_VBAP
*   WHERE AUBEL EQ T_VBAP-VBELN.
*
*  SELECT VBELN FKSTO
*    FROM VBRK
*    INTO TABLE T_VBRK
*    FOR ALL ENTRIES IN T_VBRP
*   WHERE VBELN EQ T_VBRP-VBELN
*     AND FKSTO EQ ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TABELA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_tabela_saida .


  LOOP AT t_vbap INTO w_vbap.

    CLEAR: w_saida, w_vbrp, w_vbrk, w_j_1bnflin, w_j_1bnfdoc, w_vbfa.

    MOVE-CORRESPONDING w_vbap TO w_saida.

    READ TABLE t_vbkd INTO w_vbkd WITH KEY vbeln = w_vbap-vbeln.

    IF sy-subrc IS INITIAL.
      w_saida-bstkd = w_vbkd-bstkd.
    ENDIF.

    READ TABLE t_t023t INTO w_t023t WITH KEY matkl = w_vbap-matkl.

    IF sy-subrc IS INITIAL.
      w_saida-matkl = w_t023t-matkl.
      w_saida-wgbez = w_t023t-wgbez.
    ENDIF.

    READ TABLE t_kna1 INTO w_kna1 WITH KEY kunnr = w_vbap-kunnr.

    IF sy-subrc IS INITIAL.
      w_saida-kunnr = w_kna1-kunnr.
      w_saida-name1 = w_kna1-name1.
      w_saida-sortl = w_kna1-sortl.
    ENDIF.

    READ TABLE t_vbpa INTO w_vbpa WITH KEY kunnr = w_kna1-kunnr.

    IF sy-subrc IS INITIAL.
      w_saida-kunnr1 = w_vbpa-kunnr.
    ENDIF.

    READ TABLE t_tvgrt INTO w_tvgrt WITH KEY vkgrp = w_vbap-vkgrp.

    IF sy-subrc IS INITIAL.
      w_saida-vkgrp   = w_tvgrt-vkgrp.
      w_saida-bezei   = w_tvgrt-bezei.
    ENDIF.

* NF VENDA
    SELECT SINGLE posnr vbeln
      FROM vbrp
      INTO w_vbrp
      WHERE aubel EQ w_vbap-vbeln
        AND aupos EQ w_vbap-posnr.

     IF sy-subrc IS INITIAL.
       SELECT SINGLE vbeln fksto
       FROM vbrk
       INTO w_vbrk
      WHERE vbeln = w_vbrp-vbeln
        AND fksto = ''.
     ENDIF.

    SELECT SINGLE docnum
      FROM j_1bnflin
      INTO w_j_1bnflin
      WHERE refkey EQ w_vbrk-vbeln
        AND refitm EQ w_vbrp-posnr
        AND reftyp EQ 'BI'.

    SELECT SINGLE nfenum
      FROM j_1bnfdoc
      INTO w_j_1bnfdoc
      WHERE docnum EQ w_j_1bnflin-docnum.

    IF sy-subrc IS INITIAL.
      w_saida-nfenum  = w_j_1bnfdoc-nfenum.
    ENDIF.

    IF w_saida-nfenum EQ ''.
      CHECK sy-subrc IS INITIAL.
    ENDIF.

* RELACIONAMENTO COM A NOTA FISCAL FILHA
    SELECT SINGLE vbeln posnn rfmng
      FROM vbfa
      INTO w_vbfa
      WHERE vbelv = w_vbrp-vbeln
        AND posnv = w_vbrp-posnr
        AND vbtyp_n = 'C'.

    IF sy-subrc IS INITIAL.
* NF filha
      SELECT SINGLE posnr vbeln
        FROM vbrp
        INTO w_vbrp1
        WHERE vgbel EQ w_vbfa-vbeln
          AND vgpos EQ w_vbfa-posnn.
    ENDIF.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE docnum
        FROM j_1bnflin
        INTO w_j_1bnflin1
        WHERE refkey EQ w_vbrp1-vbeln
          AND refitm EQ w_vbrp1-posnr
          AND reftyp EQ 'BI'.
    ENDIF.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE nfenum
        FROM j_1bnfdoc
        INTO w_j_1bnfdoc1
        WHERE docnum = w_j_1bnflin1-docnum.

        w_saida-nfenum1  =  w_j_1bnfdoc1-nfenum.
    ENDIF.

    w_saida-vbeln1   =  w_vbfa-vbeln.
    w_saida-zmeng1   =  w_vbfa-rfmng.
    w_saida-saldo    =  w_saida-zmeng - w_saida-zmeng1.

    " Valor total e unitário
    READ TABLE t_konv INTO w_konv WITH KEY knumv = w_vbap-vbeln
                                           kposn = w_vbap-posnr.

    IF sy-subrc IS INITIAL.
      w_saida-kwert   = w_konv-kwert.
      w_saida-kbetr   = w_konv-kbetr.
    ENDIF.

    APPEND w_saida TO t_saida.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv .

  PERFORM f_tipo_fieldcat.

  PERFORM f_define_fieldcat. "Colunas

  PERFORM f_layout. "Estilo do Relatório

  PERFORM f_imprimi_alv. "Imprimir ALV


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DEFINE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_define_fieldcat .


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'T_SAIDA'
*     I_STRUCTURE_NAME       = 'TY_SAIDA'
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_INCLNAME             =
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE TEXT-006 TYPE 'I'.
  ENDIF.

ENDFORM.
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
  w_layout-colwidth_optimize = 'X'. "Otimiza as colunas

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

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK      = ' '
*     I_BYPASSING_BUFFER     = ' '
*     I_BUFFER_ACTIVE        = ' '
      i_callback_program     = sy-repid
*     I_CALLBACK_PF_STATUS_SET      = ' '
*     I_CALLBACK_USER_COMMAND       = 'USER_COMMAND'
      i_callback_top_of_page = 'F_CABECALHO'
*     I_CALLBACK_HTML_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_END_OF_LIST   = ' '
*     I_STRUCTURE_NAME       =
*     I_BACKGROUND_ID        = ' '
*     I_GRID_TITLE           =
*     I_GRID_SETTINGS        =
      is_layout              = w_layout
      it_fieldcat            = t_fieldcat
*     IT_EXCLUDING           =
*     IT_SPECIAL_GROUPS      =
*     IT_SORT                = T_SORT
*     IT_FILTER              =
*     IS_SEL_HIDE            =
*     I_DEFAULT              = 'X'
*     I_SAVE                 = 'X '
*     IS_VARIANT             =
*     IT_EVENTS              =
*     IT_EVENT_EXIT          =
*     IS_PRINT               =
*     IS_REPREP_ID           =
*     I_SCREEN_START_COLUMN  = 0
*     I_SCREEN_START_LINE    = 0
*     I_SCREEN_END_COLUMN    = 0
*     I_SCREEN_END_LINE      = 0
*     I_HTML_HEIGHT_TOP      = 0
*     I_HTML_HEIGHT_END      = 0
*     IT_ALV_GRAPHICS        =
*     IT_HYPERLINK           =
*     IT_ADD_FIELDCAT        =
*     IT_EXCEPT_QINFO        =
*     IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER =
    TABLES
      t_outtab               = t_saida
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TIPO_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_tipo_fieldcat .

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'ERDAT'.
  w_fieldcat-seltext_m = 'Período'.
  w_fieldcat-col_pos = 1.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'VBELN'.
  w_fieldcat-seltext_m = 'N°OV'.
  w_fieldcat-col_pos = 2.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'POSNR'.
  w_fieldcat-seltext_m = 'Item OV'.
  w_fieldcat-col_pos = 3.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'BSTKD'.
  w_fieldcat-seltext_m = 'Ref. Cliente'.
  w_fieldcat-col_pos = 4.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'AUART'.
  w_fieldcat-seltext_m = 'Tipo de OV'.
  w_fieldcat-col_pos = 5.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'VKORG'.
  w_fieldcat-seltext_m = 'Org. Vendas'.
  w_fieldcat-col_pos = 6.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'MATNR'.
  w_fieldcat-seltext_m = 'Material'.
  w_fieldcat-col_pos = 7.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'ZMENG'.
  w_fieldcat-seltext_m = 'Quant. Comprada'.
  w_fieldcat-col_pos = 8.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'MATKL'.
  w_fieldcat-seltext_m = 'Grp Mercadoria'.
  w_fieldcat-col_pos = 9.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'WGBEZ'.
  w_fieldcat-seltext_m = 'Texto Grp Mercadoria'.
  w_fieldcat-col_pos = 10.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'SPART'.
  w_fieldcat-seltext_m = 'Setor de Atividade'.
  w_fieldcat-col_pos = 11.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'KUNNR'.
  w_fieldcat-seltext_m = 'Cliente'.
  w_fieldcat-col_pos = 12.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'NAME1'.
  w_fieldcat-seltext_m = 'Nome Cliente'.
  w_fieldcat-col_pos = 13.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'KUNNR1'.
  w_fieldcat-seltext_m = 'Propriedade'.
  w_fieldcat-col_pos = 14.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'SORTL'.
  w_fieldcat-seltext_m = 'Nome Propriedade'.
  w_fieldcat-col_pos = 15.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'WERKS'.
  w_fieldcat-seltext_m = 'Centro'.
  w_fieldcat-col_pos = 16.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'VKGRP'.
  w_fieldcat-seltext_m = 'Vendedor'.
  w_fieldcat-col_pos = 17.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'BEZEI'.
  w_fieldcat-seltext_m = 'Nome vendedor'.
  w_fieldcat-col_pos = 18.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'NFENUM'.
  w_fieldcat-seltext_m = 'NF Venda'.
  w_fieldcat-col_pos = 19.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'KWERT'.
  w_fieldcat-seltext_m = 'Valor Unit.'.
  w_fieldcat-col_pos = 20.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'KBETR'.
  w_fieldcat-seltext_m = 'Valor Total'.
  w_fieldcat-col_pos = 21.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'VBELN1'.
  w_fieldcat-seltext_m = 'OV Remessa'.
  w_fieldcat-col_pos = 22.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'ZMENG1'.
  w_fieldcat-seltext_m = 'Quant. Retirada'.
  w_fieldcat-col_pos = 23.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'NFENUM1'.
  w_fieldcat-seltext_m = 'NF Remessa'.
  w_fieldcat-col_pos = 24.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'SALDO'.
  w_fieldcat-seltext_m = 'Saldo atual'.
  w_fieldcat-col_pos = 25.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.
FORM f_cabecalho.

  CLEAR w_header.
  REFRESH t_header.

  w_header-typ  = 'H'. " Campo TYP -> Diferencia o tipo do texto, type 'h' é HEADER
  w_header-info = TEXT-007. "Relatório Saldo Venda Cooperados
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  w_header-key = TEXT-008. "Data:
  WRITE sy-datum TO w_header-info. " Data formato inglês convertida para br
  APPEND w_header TO t_header.

  w_header-typ = 'S'.
  w_header-key = TEXT-009. "Hora:
  WRITE sy-uzeit  TO w_header-info.
  APPEND w_header TO t_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE' "
    EXPORTING
      it_list_commentary = t_header
      i_logo             = 'ENJOYSAP_LOGO'.
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =

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
      formname = 'ZFORM_RLT'
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
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
  ENDIF.

ENDFORM.
