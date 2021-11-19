*&---------------------------------------------------------------------*
*& Report  ZSD_B2B_RAYDIUM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZSD_B2B_RAYDIUM MESSAGE-ID ZB.

TABLES: LIKP, LIPS, KNA1, VBRK, VBKD, SSCRFIELDS, VBFA,
        VEKP, VEPO, MARA,
        ZB2BI1, ZB2BI2, ZB2BI_CODE, ZMM29, ZSDEL, ZG20, ZSDDEST.

TYPES: BEGIN OF STR_HEAD,
        SELEC TYPE C,
        VKORG TYPE VKORG,
        KUNAG TYPE KUNAG,
        KUNNR TYPE KUNWE,
        VBELN TYPE VBELN_VF,
        VBTYP TYPE VBTYP,
        WADAT TYPE WADAT_IST,
        SFLAG TYPE C,                 "已寄送過Flag
        DLBIL TYPE C,                 "Billing Cancel
        KNUMV TYPE KNUMV,             "Billing串KONV用
        FNCTN TYPE C,                 "P = POPL, B = BILL
        RESON TYPE PVARFIELD,
       END OF STR_HEAD.
TYPES: BEGIN OF STR_POPL,
        VGBEL       TYPE VBELN_VL,
        WADAT       TYPE WADAT_IST,
        NAME1       TYPE NAME1_GP,
        GUINO       TYPE CHAR14,      "[X]NoUsed
        VBELN       TYPE VBELN_VF,    "[X]NoUsed
        SIDAT       TYPE ZSIDAT,
        VGPOS       TYPE POSNR_VL,
        NAME2(35)   TYPE C,
        CHARG       TYPE CHARG_D,
        ELOTN(50)   TYPE C,
        KDMAT       TYPE KDMAT,
        VDEVC(50)   TYPE C,           "[X]NoUsed
        BSTKD       TYPE BSTKD,
        LFIMG       TYPE LFIMG,
        MEINS       TYPE MEINS,       "[X]NoShow
        SEQTY(38)   TYPE C,           "[X]NoUsed
        WAFER(100)  TYPE C,
        SHMK1(75)   TYPE C,           "[X]NoUsed
        SHMK2(75)   TYPE C,           "[X]NoUsed
        SHMK3(75)   TYPE C,           "[X]NoUsed
        FORWD(75)   TYPE C,           "[X]NoUsed
        CARTN       TYPE EXIDV,
        NTGEW	      TYPE NTGEW_15,
        BRGEW	      TYPE BRGEW_15,
        GEWEI	      TYPE GEWEI,       "[X]NoShow
        FLIGT(75)   TYPE C,           "[X]NoUsed
        ZMAWA(75)   TYPE C,           "[X]NoUsed
        ZHAWA(75)   TYPE C,           "[X]NoUsed
        EXIDV       TYPE EXIDV,
        DIMEN(75)   TYPE C,
       RMARK(255)   TYPE C,           "[X]NoUsed
        ZMARK(75)   TYPE C,
    END OF STR_POPL.
TYPES: BEGIN OF STR_BILL,
        VBELN       TYPE VBELN_VF,
        SIDAT       TYPE SYDATUM,
        GUINO       TYPE CHAR14,
        GUIDT       TYPE SYDATUM,
        NAME1       TYPE NAME1_GP,
        NAME2       TYPE NAME1_GP,
        WAERK       TYPE WAERK,       "Currency
        KURRF       TYPE KURRF,       "Exchange rate
        PWAER       TYPE WAERK,       "Pay Currency
        WRBTR       TYPE WRBTR,       "Invoice Total
        TAXID(10)   TYPE C,           "Tax Type
        WMWST       TYPE WMWST,       "Tax Amount
        TOTAL       TYPE WRBTR,       "Total Amount
        VGBEL       TYPE VBELN_VL,    "DN
        VGPOS       TYPE POSNR_VL,    "DN Item
        BSTKD       TYPE BSTKD,       "Cust PO
        KDMAT       TYPE KDMAT,       "Cust Material
        WORKS(50)   TYPE C,           "[X]NoUsed
        BINPT(50)   TYPE C,           "[X]NoUsed
        SUWKS(255)  TYPE C,           "[X]NoUsed
        CHARG       TYPE CHARG_D,
        OCHAR       TYPE CHARG_D,
        LFIMG       TYPE LFIMG,
        MEINS       TYPE MEINS,       "[X]NoShow
        SEQTY(38)   TYPE C,
        NETPR       TYPE NETPR,       "Unit price
        NETWR       TYPE NETWR,       "Invoice Subtotal
        GNETW       TYPE NETWR,       "GUI Subtotal
        KURSK       TYPE KURSK,       "GUI Exchange Rate
    END OF STR_BILL.
TYPES: BEGIN OF STR_EROR,
        "FNCTN     TYPE C,             "P = POPL, B = BILL
        VBELN     TYPE VBELN_VF,
        TEXTS(50) TYPE C,
       END OF STR_EROR.

CONTROLS: TC100_HEAD TYPE TABLEVIEW USING SCREEN 100.

DATA: OK_CODE         TYPE SYUCOMM,
      I_HEAD          TYPE STANDARD TABLE OF STR_HEAD WITH HEADER LINE,
      I_POPL          TYPE STANDARD TABLE OF STR_POPL,
      I_BILL          TYPE STANDARD TABLE OF STR_BILL,
      I_EROR          TYPE STANDARD TABLE OF STR_EROR,
      V_FCUST_PL(20)  TYPE C VALUE 'RAD-DN',
      V_FCUST_BI(20)  TYPE C VALUE 'RAD-INV',
      V_FCUST_BC(20)  TYPE C VALUE 'RAD-CANINV',
      SALV_TB         TYPE REF TO cl_salv_table,
      SALV_CONTINER   TYPE REF TO cl_gui_custom_container.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (33) TEXT-H01 FOR FIELD P_KUNAG.         "TEXT-H01 = 'Sold-to party'
  PARAMETERS  P_KUNAG TYPE KUNAG DEFAULT '0000002520'.
  SELECTION-SCREEN COMMENT (30) V_CNAME.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-T01.      "TEXT-T01 = 'Sales Org.'
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS  P_AORG RADIOBUTTON GROUP R1 USER-COMMAND AC DEFAULT 'X'.
    SELECTION-SCREEN COMMENT (16) TEXT-R01.
    PARAMETERS  P_MAX1 RADIOBUTTON GROUP R1.
    SELECTION-SCREEN COMMENT (16) TEXT-R02.
    PARAMETERS  P_PSC1 RADIOBUTTON GROUP R1.
    SELECTION-SCREEN COMMENT (16) TEXT-R03.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK 001.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (30) V_FKDAT FOR FIELD S_FKDAT.
  SELECT-OPTIONS:  S_FKDAT FOR LIKP-WADAT.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (30) V_VBELN FOR FIELD S_VBELN.
  SELECT-OPTIONS:  S_VBELN FOR LIKP-VBELN.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK 002 WITH FRAME TITLE TEXT-H02.      "TEXT-H02 = 'B2B Type'
   PARAMETERS: P_POPL RADIOBUTTON GROUP R2 DEFAULT 'X' USER-COMMAND AC,
               P_BILL RADIOBUTTON GROUP R2.
SELECTION-SCREEN END OF BLOCK 002.





INITIALIZATION.


AT SELECTION-SCREEN OUTPUT.
  PERFORM GET_CUST_NAME USING     P_KUNAG
                        CHANGING  V_CNAME.
  PERFORM GET_CRITERIA_TEXT.
  PERFORM SCREEN_CONTROL.

AT SELECTION-SCREEN.
  PERFORM CHECK_INPUT_DATA.


START-OF-SELECTION.
  "收集Head Data
  PERFORM GET_POPL_HEADER_DATA USING    P_POPL.
  PERFORM GET_BILL_HEADER_DATA USING    P_BILL.
*  PERFORM SAMPLE_DATA USING 'HEAD'.
  CHECK I_HEAD[] IS NOT INITIAL.
  PERFORM GET_B2B_ITEM_DATA.
  "只有非JOB才會RUN以下二個
  PERFORM SHOW_ERROR_LOG.
  PERFORM CALL_SCREEN.
  "只有JOB時才送Error Mail及SEND B2B(同時會刪I_HEAD有ERROR LOG的ITEM)
  PERFORM SEND_ERROR_MAIL.
  "送完ERROR MAIL有可能再刪I_HEAD
  CHECK I_HEAD[] IS NOT INITIAL.
  PERFORM PREPARE_DATA_TO_SEND.

















*&---------------------------------------------------------------------*
*&      Form  GET_CUST_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_KUNAG  text
*      <--P_V_CNAME  text
*----------------------------------------------------------------------*
FORM GET_CUST_NAME  USING    PFV_KUNAG_I
                    CHANGING PFV_NAME1_O.
  DATA: PFWA_KNA1 TYPE KNA1.

  CLEAR: PFV_NAME1_O.
  PERFORM GET_WORKAREA_KNA1 USING     PFV_KUNAG_I
                            CHANGING  PFWA_KNA1.
  CHECK PFWA_KNA1 IS NOT INITIAL.
  PFV_NAME1_O = PFWA_KNA1-NAME1.
ENDFORM.                    " GET_CUST_NAME
*&---------------------------------------------------------------------*
*&      Form  GET_WORKAREA_KNA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PFV_KUNAG_I  text
*      <--P_PFWA_KNA1  text
*----------------------------------------------------------------------*
FORM GET_WORKAREA_KNA1  USING    PFV_CSTID_I
                        CHANGING PFWA_KNA1_O TYPE KNA1.
  CLEAR: PFWA_KNA1_O.
  SELECT SINGLE * FROM KNA1
    INTO CORRESPONDING FIELDS OF PFWA_KNA1_O
                              WHERE KUNNR = PFV_CSTID_I.
ENDFORM.                    " GET_WORKAREA_KNA1
*&---------------------------------------------------------------------*
*&      Form  GET_CRITERIA_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CRITERIA_TEXT .
  IF P_POPL IS NOT INITIAL.
    V_FKDAT = 'PGI Date'.
    V_VBELN = 'Delivery No'.
  ENDIF.
  IF P_BILL IS NOT INITIAL.
    V_FKDAT = 'Sales Invoice Date'.
    V_VBELN = 'Invoice No'.
  ENDIF.
ENDFORM.                    " GET_CRITERIA_TEXT
*&---------------------------------------------------------------------*
*&      Form  SCREEN_CONTROL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SCREEN_CONTROL .
  LOOP AT SCREEN.
    PERFORM CONTROL_SCREEN_FUNCTION USING: 'NAME' '' 'INPUT' 'P_KUNAG' 0.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_CONTROL
*&---------------------------------------------------------------------*
*&      Form  CONTROL_SCREEN_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0271   text
*      -->P_0272   text
*      -->P_0273   text
*      -->P_0274   text
*      -->P_0      text
*----------------------------------------------------------------------*
FORM CONTROL_SCREEN_FUNCTION  USING    PFV_GPFLD_I        "判斷是FIELD / GROUP
                                       PFV_GPNUM_I        "Group的編號
                                       PFV_FNCTN_I        "INPUT / ACTIVE...
                                       PFV_GFVAL_I        "FIELD / GROUP的值
                                       PFV_ACTON_I.       "Action
  IF PFV_GPFLD_I = 'NAME'.
    CHECK SCREEN-NAME = PFV_GFVAL_I.
  ENDIF.

  IF PFV_GPFLD_I = 'GROUP'.
    CASE PFV_GPNUM_I.
      WHEN 1.
        CHECK SCREEN-GROUP1 = PFV_GFVAL_I.
      WHEN 2.
        CHECK SCREEN-GROUP2 = PFV_GFVAL_I.
      WHEN 3.
        CHECK SCREEN-GROUP3 = PFV_GFVAL_I.
      WHEN 4.
        CHECK SCREEN-GROUP4 = PFV_GFVAL_I.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

  CASE PFV_FNCTN_I.
    WHEN 'INPUT'.
        SCREEN-INPUT  = PFV_ACTON_I.
    WHEN 'ACTIVE'.
        SCREEN-ACTIVE = PFV_ACTON_I.
    WHEN 'INTEN'.
        SCREEN-INTENSIFIED = PFV_ACTON_I.
    WHEN 'INVIS'.
        SCREEN-INVISIBLE = PFV_ACTON_I.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " CONTROL_SCREEN_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_DATA .
*使用者一定要下範圍
  CHECK SSCRFIELDS-UCOMM = 'ONLI'.
  CHECK S_VBELN[] IS INITIAL AND
        S_FKDAT[] IS INITIAL.
  CLEAR SSCRFIELDS.
  MESSAGE S000 WITH TEXT-E01.         "TEXT-E01 = '為防止系統的效能不佳,請輸入範圍!!!!'
ENDFORM.                    " CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_POPL_HEADER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_POPL  text
*      <--P_V_STPPG  text
*----------------------------------------------------------------------*
FORM GET_POPL_HEADER_DATA  USING    PFV_FNCTN_I.
  DATA: PFWA_HEAD   TYPE STR_HEAD,
        PFWA_LIKP   TYPE LIKP,
        PFWA_ZB2BI1 TYPE ZB2BI1,
        PF_LIKP     TYPE STANDARD TABLE OF LIKP.

  CHECK PFV_FNCTN_I IS NOT INITIAL.
  CLEAR: I_HEAD[].
  SELECT * FROM LIKP
    INTO CORRESPONDING FIELDS OF  TABLE PF_LIKP
                                  WHERE  VBELN     IN S_VBELN
                                  AND    WADAT_IST IN S_FKDAT
                                  AND    KUNAG     =  P_KUNAG
                                  AND    VBTYP     =  'J'
                                  AND    WADAT_IST <> '00000000'.   "防止User直接給未PGI的DN

  IF P_MAX1 IS NOT INITIAL.
    DELETE PF_LIKP WHERE VKORG <> 'MAX1'.
  ENDIF.
  IF P_PSC1 IS NOT INITIAL.
    DELETE PF_LIKP WHERE VKORG <> 'PSC1'.
  ENDIF.
  "條件內是否有值的檢查
  IF PF_LIKP[] IS INITIAL.
    CHECK SY-BATCH IS INITIAL.
    MESSAGE I080 WITH 'No data in selection criterion(POPL)'.
    EXIT.
  ENDIF.

  LOOP AT PF_LIKP INTO PFWA_LIKP.
    CLEAR: PFWA_HEAD.
    PFWA_HEAD-VBELN = PFWA_LIKP-VBELN.
    PFWA_HEAD-VKORG = PFWA_LIKP-VKORG.
    PFWA_HEAD-KUNAG = PFWA_LIKP-KUNAG.
    PFWA_HEAD-KUNNR = PFWA_LIKP-KUNNR.
    PFWA_HEAD-VBTYP = PFWA_LIKP-VBTYP.
    PFWA_HEAD-WADAT = PFWA_LIKP-WADAT_IST.
    PFWA_HEAD-FNCTN = 'P'.
    "檢查是否已經送過
    PERFORM GET_WORKAREA_ZB2BI1 USING     PFWA_LIKP-VBELN
                                          PFWA_LIKP-KUNAG
                                          V_FCUST_PL
                                CHANGING  PFWA_ZB2BI1.
    IF PFWA_ZB2BI1 IS NOT INITIAL.
      PFWA_HEAD-SFLAG = 'X'.
    ENDIF.

    APPEND PFWA_HEAD TO I_HEAD.
  ENDLOOP.

  "BackGround Job的檢查,若有送過就刪掉
  PERFORM BATCH_JOB_DELSENT_ITEM.
ENDFORM.                    " GET_POPL_HEADER_DATA
*&---------------------------------------------------------------------*
*&      Form  BATCH_JOB_DELSENT_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BATCH_JOB_DELSENT_ITEM .
  CHECK SY-BATCH IS NOT INITIAL.
  DELETE I_HEAD WHERE SFLAG IS NOT INITIAL.
ENDFORM.                    " BATCH_JOB_DELSENT_ITEM
*&---------------------------------------------------------------------*
*&      Form  GET_WORKAREA_ZB2BI1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PFWA_LIKP_VBELN  text
*      -->P_PFWA_LIKP_KUNAG  text
*      -->P_V_FCUST_PL  text
*      <--P_PFWA_ZB2BI1  text
*----------------------------------------------------------------------*
FORM GET_WORKAREA_ZB2BI1  USING    PFV_VBELN_I
                                   PFV_KUNAG_I
                                   PFV_FCUST_I
                          CHANGING PFWA_ZB2BI1_O TYPE ZB2BI1.
  CLEAR: PFWA_ZB2BI1_O.
  SELECT SINGLE * FROM ZB2BI1
    INTO CORRESPONDING FIELDS OF PFWA_ZB2BI1_O
                              WHERE VBELN    = PFV_VBELN_I
                              AND   KUNAG    = PFV_KUNAG_I
                              AND   FOR_CUST = PFV_FCUST_I.
ENDFORM.                    " GET_WORKAREA_ZB2BI1
*&---------------------------------------------------------------------*
*&      Form  GET_BILL_HEADER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BILL  text
*      <--P_V_STPPG  text
*----------------------------------------------------------------------*
FORM GET_BILL_HEADER_DATA  USING    PFV_FNCTN_I.
  DATA: PFWA_HEAD   TYPE STR_HEAD,
        PFWA_VBRK   TYPE VBRK,
        PFWA_ZB2BI1 TYPE ZB2BI1,
        PF_VBRK     TYPE STANDARD TABLE OF VBRK,
        PF_VBRK_CAN TYPE STANDARD TABLE OF VBRK.


  CHECK PFV_FNCTN_I IS NOT INITIAL.
  CLEAR: I_HEAD[].
  "取正常的Billing
  SELECT * FROM VBRK
    INTO CORRESPONDING FIELDS OF  TABLE  PF_VBRK
                                  WHERE  VBTYP  = 'M'
                                  AND    FKSTO  = ''
                                  AND    VBELN  IN S_VBELN
                                  AND    ZSIDAT IN S_FKDAT"抓Sales Inv. Date
                                  AND    KUNAG  =  P_KUNAG.
  "取得Billing有Cancel的部份
  PERFORM GET_CANCEL_BILLING TABLES PF_VBRK_CAN.
  "處理SaleOrg的部份
  IF P_MAX1 IS NOT INITIAL.
    DELETE PF_VBRK WHERE VKORG <> 'MAX1'.
    DELETE PF_VBRK_CAN WHERE VKORG <> 'MAX1'.
  ENDIF.
  IF P_PSC1 IS NOT INITIAL.
    DELETE PF_VBRK WHERE VKORG <> 'PSC1'.
    DELETE PF_VBRK_CAN WHERE VKORG <> 'PSC1'.
  ENDIF.
  "都沒有取到值就跳訊息,同時離開
  IF PF_VBRK[] IS INITIAL AND
     PF_VBRK_CAN[] IS INITIAL.
    CHECK SY-BATCH IS INITIAL.
    MESSAGE I080 WITH 'No data in selection criterion(BILL)'.
    EXIT.
  ENDIF.
  "收集Head Data(正常Billing)
  LOOP AT PF_VBRK INTO PFWA_VBRK.
    CLEAR: PFWA_HEAD.
    PFWA_HEAD-VBELN = PFWA_VBRK-VBELN.
    PFWA_HEAD-VKORG = PFWA_VBRK-VKORG.
    PFWA_HEAD-KUNAG = PFWA_VBRK-KUNAG.
    PFWA_HEAD-VBTYP = PFWA_VBRK-VBTYP.
    PFWA_HEAD-KNUMV = PFWA_VBRK-KNUMV.
    PFWA_HEAD-WADAT = PFWA_VBRK-ZSIDAT.
    PFWA_HEAD-FNCTN = 'B'.
    "檢查是否已經送過
    PERFORM GET_WORKAREA_ZB2BI1 USING     PFWA_VBRK-VBELN
                                          PFWA_VBRK-KUNAG
                                          V_FCUST_BI
                                CHANGING  PFWA_ZB2BI1.
    IF PFWA_ZB2BI1 IS NOT INITIAL.
      PFWA_HEAD-SFLAG = 'X'.
    ENDIF.

    APPEND PFWA_HEAD TO I_HEAD.
  ENDLOOP.
  "收集Head Data(Cancel Billing)
  LOOP AT PF_VBRK_CAN INTO PFWA_VBRK.
    CLEAR: PFWA_HEAD.
    PFWA_HEAD-VBELN = PFWA_VBRK-VBELN.
    PFWA_HEAD-VKORG = PFWA_VBRK-VKORG.
    PFWA_HEAD-KUNAG = PFWA_VBRK-KUNAG.
    PFWA_HEAD-VBTYP = PFWA_VBRK-VBTYP.
    PFWA_HEAD-FNCTN = 'B'.
    PFWA_HEAD-DLBIL = 'X'.
    "檢查是否已經送過
    PERFORM GET_WORKAREA_ZB2BI1 USING     PFWA_VBRK-VBELN
                                          PFWA_VBRK-KUNAG
                                          V_FCUST_BC
                                CHANGING  PFWA_ZB2BI1.
    IF PFWA_ZB2BI1 IS NOT INITIAL.
      PFWA_HEAD-SFLAG = 'X'.
    ENDIF.
    APPEND PFWA_HEAD TO I_HEAD.
  ENDLOOP.
  "BackGround Job的檢查,若有送過就刪掉
  PERFORM BATCH_JOB_DELSENT_ITEM.
ENDFORM.                    " GET_BILL_HEADER_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_B2B_ITEM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_B2B_ITEM_DATA .
  PERFORM GET_POPL_ITEM_DATA USING P_POPL.
  PERFORM CHECK_POPL_ITEM_DATA USING P_POPL.
  PERFORM GET_BILL_ITEM_DATA USING P_BILL.
  PERFORM CHECK_BILL_ITEM_DATA USING P_BILL.
ENDFORM.                    " GET_B2B_ITEM_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_POPL_ITEM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_POPL  text
*----------------------------------------------------------------------*
FORM GET_POPL_ITEM_DATA  USING    PFV_FLGPL_I.
  DATA: PFWA_HEAD TYPE STR_HEAD,
        PFWA_LIPS TYPE LIPS,
        PFWA_POPL TYPE STR_POPL,
        PF_LIPS   TYPE STANDARD TABLE OF LIPS,
        PF_VEKP   TYPE STANDARD TABLE OF VEKP,
        PF_VEPO   TYPE STANDARD TABLE OF VEPO,
        PF_VBRK   TYPE STANDARD TABLE OF VBRK,
        PF_VBKD   TYPE STANDARD TABLE OF VBKD,
        PF_VBFA   TYPE STANDARD TABLE OF VBFA,
        PF_BCDE   TYPE STANDARD TABLE OF ZB2BI_CODE,
        PF_ZMM29  TYPE STANDARD TABLE OF ZMM29.

  CHECK PFV_FLGPL_I IS NOT INITIAL.
  CLEAR: I_POPL[].
  "FLOW DATA
  PERFORM GET_VBFA_FRM_HEAD TABLES PF_VBFA.
  "Billing Data
  PERFORM GET_VBRK_FRM_VBFA TABLES PF_VBRK
                                   PF_VBFA.
  "Hand Unit
  PERFORM GET_VEKP_VEPO_FRM_VBFA TABLES PF_VEKP
                                        PF_VEPO
                                        PF_VBFA.
  "DN ITEM
  PERFORM GET_LIPS_FRM_HEAD TABLES PF_LIPS.
  "自訂SHIPTO NAME
  PERFORM GET_ZB2BICODE_FRM_HEAD TABLES PF_BCDE.
  "客戶PO資訊
  PERFORM GET_VBKD_FRM_LIPS TABLES PF_VBKD
                                   PF_LIPS.
  "Wafer ID資訊
  PERFORM GET_ZMM29_FRM_LIPS TABLES PF_ZMM29
                                    PF_LIPS.

  LOOP AT I_HEAD INTO PFWA_HEAD WHERE FNCTN = 'P'.
    LOOP AT PF_LIPS INTO PFWA_LIPS WHERE VBELN = PFWA_HEAD-VBELN.
      PFWA_POPL-NAME2 = 'PSMC'.
      PFWA_POPL-ZMARK = 'GOOD'.
      PFWA_POPL-WADAT = PFWA_HEAD-WADAT.
      PFWA_POPL-SIDAT = PFWA_HEAD-WADAT.        "若不是Free且已Billing後面會抓VBRK的資料

      PFWA_POPL-VGBEL = PFWA_LIPS-VBELN.
      PFWA_POPL-CHARG = PFWA_LIPS-CHARG.
      PFWA_POPL-ELOTN = PFWA_LIPS-CHARG.
      PFWA_POPL-VGPOS = PFWA_LIPS-POSNR+1(5).
      PFWA_POPL-KDMAT = PFWA_LIPS-KDMAT.
      PFWA_POPL-LFIMG = PFWA_LIPS-LFIMG.
      PFWA_POPL-MEINS = PFWA_LIPS-MEINS.
      "NAME1
      PERFORM GET_SHIPTO_NAME TABLES    PF_BCDE
                              USING     PFWA_HEAD
                              CHANGING  PFWA_POPL.
      "BSTKD
      PERFORM GET_CUST_PO_INFO TABLES    PF_VBKD
                               USING     PFWA_LIPS
                               CHANGING  PFWA_POPL-BSTKD.
      "WAFER
      PERFORM GET_WAFER_ID TABLES   PF_ZMM29
                           USING    PFWA_LIPS
                           CHANGING PFWA_POPL.
      "SIDAT
      PERFORM GET_TO_BE_SHIP_DATE TABLES    PF_VBFA
                                            PF_VBRK
                                  USING     PFWA_LIPS
                                  CHANGING  PFWA_POPL.
      "CARTN / BRGEW / NTGEW / GEWEI / EXIDV / DIMEN
      PERFORM GET_CARTON_WEIGHT_INFO TABLES   PF_VEKP
                                              PF_VEPO
                                     USING    PFWA_LIPS
                                     CHANGING PFWA_POPL.
      APPEND PFWA_POPL TO I_POPL.
      CLEAR: PFWA_POPL.
    ENDLOOP.
  ENDLOOP.
  SORT I_POPL BY VGBEL VGPOS.
ENDFORM.                    " GET_POPL_ITEM_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_LIPS_FRM_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PF_LIPS  text
*----------------------------------------------------------------------*
FORM GET_LIPS_FRM_HEAD  TABLES PF_LIPS_O STRUCTURE LIPS.
  DATA: PF_HEAD TYPE STANDARD TABLE OF STR_HEAD.

  CLEAR: PF_LIPS_O, PF_LIPS_O[].
  APPEND LINES OF I_HEAD TO PF_HEAD.
  DELETE PF_HEAD WHERE FNCTN <> 'P'.
  CHECK PF_HEAD[] IS NOT INITIAL.
  SELECT * FROM LIPS
    INTO CORRESPONDING FIELDS OF TABLE PF_LIPS_O
                                 FOR ALL ENTRIES IN PF_HEAD
                                 WHERE VBELN =  PF_HEAD-VBELN
                                 AND   CHARG <> ''.
                                 "以前是用UECHA <>  '000000'.
ENDFORM.                    " GET_LIPS_FRM_HEAD
*&---------------------------------------------------------------------*
*&      Form  GET_ZB2BICODE_FRM_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_BCDE  text
*----------------------------------------------------------------------*
FORM GET_ZB2BICODE_FRM_HEAD  TABLES   PF_BCDE_O STRUCTURE ZB2BI_CODE.
  DATA: PF_HEAD TYPE STANDARD TABLE OF STR_HEAD.

  CLEAR: PF_BCDE_O, PF_BCDE_O[].
  APPEND LINES OF I_HEAD TO PF_HEAD.
  DELETE PF_HEAD WHERE FNCTN <> 'P'.
  CHECK PF_HEAD[] IS NOT INITIAL.
  SELECT * FROM ZB2BI_CODE
    INTO CORRESPONDING FIELDS OF TABLE PF_BCDE_O
                                 FOR ALL ENTRIES IN PF_HEAD
                                 WHERE KUNAG = PF_HEAD-KUNAG
                                 AND   KUNNR = PF_HEAD-KUNNR.
ENDFORM.                    " GET_ZB2BICODE_FRM_HEAD
*&---------------------------------------------------------------------*
*&      Form  GET_VBKD_FRM_LIPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_VBKD  text
*      -->P_PF_LIPS  text
*----------------------------------------------------------------------*
FORM GET_VBKD_FRM_LIPS  TABLES   PF_VBKD_O STRUCTURE VBKD
                                 PF_LIPS_I STRUCTURE LIPS.
  CLEAR: PF_VBKD_O, PF_VBKD_O[].
  CHECK PF_LIPS_I[] IS NOT INITIAL.
  "先全抓..外面再去用SO ITEM串
  SELECT * FROM VBKD
    INTO CORRESPONDING FIELDS OF TABLE PF_VBKD_O
                                 FOR ALL ENTRIES IN PF_LIPS_I
                                 WHERE VBELN = PF_LIPS_I-VGBEL.
ENDFORM.                    " GET_VBKD_FRM_LIPS
*&---------------------------------------------------------------------*
*&      Form  GET_ZMM29_FRM_LIPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_ZMM29  text
*      -->P_PF_LIPS  text
*----------------------------------------------------------------------*
FORM GET_ZMM29_FRM_LIPS  TABLES   PF_ZMM29_O STRUCTURE ZMM29
                                  PF_LIPS_I  STRUCTURE LIPS.
  DATA: PFWA_LIPS TYPE LIPS.
  CLEAR: PF_ZMM29_O, PF_ZMM29_O[].
  CHECK PF_LIPS_I[] IS NOT INITIAL.
  "12"沒有Material在ZMM29..所以先只抓ZMM29
  "LOT的宣告長度不同..不能用FOR ALL ENTRY
  LOOP AT PF_LIPS_I INTO PFWA_LIPS.
    SELECT * FROM ZMM29
      APPENDING CORRESPONDING FIELDS OF TABLE PF_ZMM29_O
                                        WHERE KEYNO = PFWA_LIPS-CHARG.
  ENDLOOP.
ENDFORM.                    " GET_ZMM29_FRM_LIPS
*&---------------------------------------------------------------------*
*&      Form  GET_SHIPTO_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_BCDE  text
*      -->P_PFWA_HEAD  text
*      <--P_PFWA_POPL  text
*----------------------------------------------------------------------*
FORM GET_SHIPTO_NAME  TABLES   PF_BCDE_I    STRUCTURE ZB2BI_CODE
                      USING    PFWA_HEAD_I  TYPE STR_HEAD
                      CHANGING PFWA_POPL_IO TYPE STR_POPL.
  DATA: PFWA_BCDE TYPE ZB2BI_CODE.

  CLEAR: PFWA_POPL_IO-NAME1.
  READ TABLE PF_BCDE_I INTO PFWA_BCDE WITH KEY KUNNR = PFWA_HEAD_I-KUNNR.
  CHECK PFWA_BCDE IS NOT INITIAL.
  PFWA_POPL_IO-NAME1 = PFWA_BCDE-CODE.
ENDFORM.                    " GET_SHIPTO_NAME
*&---------------------------------------------------------------------*
*&      Form  GET_CUST_PO_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_VBKD  text
*      -->P_PFWA_HEAD  text
*      <--P_PFWA_POPL  text
*----------------------------------------------------------------------*
FORM GET_CUST_PO_INFO  TABLES   PF_VBKD_I    STRUCTURE VBKD
                       USING    PFWA_LIPS_I  TYPE LIPS
                       CHANGING PFV_BSTKD_O.
  DATA: PFWA_VBKD TYPE VBKD.

  CLEAR: PFV_BSTKD_O.
  READ TABLE PF_VBKD_I
    INTO PFWA_VBKD WITH KEY VBELN = PFWA_LIPS_I-VGBEL
                            POSNR = PFWA_LIPS_I-VGPOS.
  IF PFWA_VBKD IS INITIAL.
    READ TABLE PF_VBKD_I
      INTO PFWA_VBKD WITH KEY VBELN = PFWA_LIPS_I-VGBEL.
  ENDIF.
  PFV_BSTKD_O = PFWA_VBKD-BSTKD.
ENDFORM.                    " GET_CUST_PO_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_WAFER_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_ZMM29  text
*      -->P_PFWA_LIPS  text
*      <--P_PFWA_POPL  text
*----------------------------------------------------------------------*
FORM GET_WAFER_ID  TABLES   PF_ZMM29_I    STRUCTURE ZMM29
                   USING    PFWA_LIPS_I   TYPE LIPS
                   CHANGING PFWA_POPL_IO  TYPE STR_POPL.
  DATA: PFWA_ZMM29 TYPE ZMM29.
  CLEAR: PFWA_POPL_IO-WAFER.
  READ TABLE PF_ZMM29_I INTO PFWA_ZMM29 WITH KEY KEYNO = PFWA_LIPS_I-CHARG.
  CHECK PFWA_ZMM29 IS NOT INITIAL.
  PFWA_POPL_IO-WAFER = PFWA_ZMM29-ZWAFER.
ENDFORM.                    " GET_WAFER_ID
*&---------------------------------------------------------------------*
*&      Form  GET_VBFA_FRM_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_VBFA  text
*----------------------------------------------------------------------*
FORM GET_VBFA_FRM_HEAD  TABLES   PF_VBFA_O STRUCTURE VBFA.

  DATA: PF_HEAD   TYPE STANDARD TABLE OF STR_HEAD.

  CLEAR: PF_VBFA_O, PF_VBFA_O[].
  APPEND LINES OF I_HEAD TO PF_HEAD.
  DELETE PF_HEAD WHERE FNCTN <> 'P'.
  CHECK PF_HEAD[] IS NOT INITIAL.
  SELECT * FROM VBFA
    INTO CORRESPONDING FIELDS OF TABLE PF_VBFA_O
                                 FOR ALL ENTRIES IN PF_HEAD
                                 WHERE VBELV    =  PF_HEAD-VBELN
                                 AND   VBTYP_N  IN ('M', 'X').
ENDFORM.                    " GET_VBFA_FRM_HEAD
*&---------------------------------------------------------------------*
*&      Form  GET_TO_BE_SHIP_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_VBFA  text
*      -->P_PF_VBRK  text
*      -->P_PFWA_LIPS  text
*      <--P_PFWA_POPL  text
*----------------------------------------------------------------------*
FORM GET_TO_BE_SHIP_DATE  TABLES   PF_VBFA_I    STRUCTURE VBFA
                                   PF_VBRK_I    STRUCTURE VBRK
                          USING    PFWA_LIPS_I  TYPE LIPS
                          CHANGING PFWA_POPL_IO TYPE STR_POPL.
  DATA: PFWA_VBFA TYPE VBFA,
        PFWA_VBRK TYPE VBRK.

  READ TABLE PF_VBFA_I INTO PFWA_VBFA WITH KEY VBELV   = PFWA_LIPS_I-VBELN
                                               POSNV   = PFWA_LIPS_I-UECHA
                                               VBTYP_N = 'M'.
  CHECK PFWA_VBFA IS NOT INITIAL.
  READ TABLE PF_VBRK_I INTO PFWA_VBRK WITH KEY VBELN = PFWA_VBFA-VBELN.
  CHECK PFWA_VBRK IS NOT INITIAL.
  PFWA_POPL_IO-SIDAT = PFWA_VBRK-ZSIDAT.
ENDFORM.                    " GET_TO_BE_SHIP_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_CARTON_WEIGHT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PFWA_LIPS  text
*      <--P_PFWA_POPL  text
*----------------------------------------------------------------------*
FORM GET_CARTON_WEIGHT_INFO  TABLES   PF_VEKP_I     STRUCTURE VEKP
                                      PF_VEPO_I     STRUCTURE VEPO
                             USING    PFWA_LIPS_I   TYPE LIPS
                             CHANGING PFWA_POPL_IO  TYPE STR_POPL.
  DATA: PFV_LINES    TYPE I,
        PFWA_VEKP    TYPE VEKP,
        PFWA_VEPO    TYPE VEPO,
        PF_VEKP_CURR TYPE STANDARD TABLE OF VEKP,
        PF_VEPO_CURR TYPE STANDARD TABLE OF VEPO.

  CLEAR: PFWA_POPL_IO-CARTN, PFWA_POPL_IO-BRGEW, PFWA_POPL_IO-NTGEW,
         PFWA_POPL_IO-GEWEI, PFWA_POPL_IO-EXIDV, PFWA_POPL_IO-DIMEN.
  "取得現在HandUnit Item資料
  LOOP AT PF_VEPO_I WHERE VBELN = PFWA_LIPS_I-VBELN.
    MOVE-CORRESPONDING PF_VEPO_I TO PFWA_VEPO.
    APPEND PFWA_VEPO TO PF_VEPO_CURR.
  ENDLOOP.
  "取得現在HandUnit Head資料
  LOOP AT PF_VEPO_CURR INTO PFWA_VEPO.
    LOOP AT PF_VEKP_I WHERE VENUM = PFWA_VEPO-VENUM.
      MOVE-CORRESPONDING PF_VEKP_I TO PFWA_VEKP.
      APPEND PFWA_VEKP TO PF_VEKP_CURR.
    ENDLOOP.
  ENDLOOP.

  "該DN所有的箱數
  SORT PF_VEKP_CURR BY VENUM.
  DELETE ADJACENT DUPLICATES FROM PF_VEKP_CURR COMPARING VENUM.
  DESCRIBE TABLE PF_VEKP_CURR LINES PFV_LINES.
  PFWA_POPL_IO-CARTN = PFV_LINES.
  PERFORM CONVERSION_EXIT_ALPHA_INPUT CHANGING PFWA_POPL_IO-CARTN.
  "該Lot所在的箱號
  READ TABLE PF_VEPO_CURR INTO PFWA_VEPO WITH KEY VBELN = PFWA_LIPS_I-VBELN
                                                  POSNR = PFWA_LIPS_I-POSNR.
  READ TABLE PF_VEKP_CURR INTO PFWA_VEKP WITH KEY VENUM = PFWA_VEPO-VENUM.
  PFWA_POPL_IO-EXIDV = PFWA_VEKP-EXIDV.
  "該箱的重量
  IF PFWA_VEKP-NTGEW < '0.1'.
    PFWA_VEKP-NTGEW = '0.1'.
  ENDIF.
  ""Gross Weight
  PFWA_POPL_IO-BRGEW = PFWA_VEKP-BRGEW.
  ""Net Weight
  PFWA_POPL_IO-NTGEW = PFWA_VEKP-NTGEW.
  ""Unit
  PFWA_POPL_IO-GEWEI = PFWA_VEKP-GEWEI.
  "Box Size
  PERFORM GET_BOX_DIMENSION USING     PFWA_VEKP
                            CHANGING  PFWA_POPL_IO-DIMEN.
ENDFORM.                    " GET_CARTON_WEIGHT_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_VBRK_FRM_VBFA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_VBRK  text
*      -->P_PF_VBFA  text
*----------------------------------------------------------------------*
FORM GET_VBRK_FRM_VBFA  TABLES   PF_VBRK_O STRUCTURE VBRK
                                 PF_VBFA_I STRUCTURE VBFA.
  DATA: PF_VBFA_M TYPE STANDARD TABLE OF VBFA.

  CLEAR: PF_VBRK_O, PF_VBRK_O[].
  "取得Billing Head
  APPEND LINES OF PF_VBFA_I TO PF_VBFA_M.
  DELETE PF_VBFA_M WHERE VBTYP_N <> 'M'.
  CHECK PF_VBFA_M[] IS NOT INITIAL.
  SELECT * FROM VBRK
    INTO CORRESPONDING FIELDS OF TABLE PF_VBRK_O
                                 FOR ALL ENTRIES IN PF_VBFA_M
                                 WHERE VBELN = PF_VBFA_M-VBELN.
ENDFORM.                    " GET_VBRK_FRM_VBFA
*&---------------------------------------------------------------------*
*&      Form  GET_VEKP_VEPO_FRM_VBFA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_VEKP  text
*      -->P_PF_VEPO  text
*      -->P_PF_VBFA  text
*----------------------------------------------------------------------*
FORM GET_VEKP_VEPO_FRM_VBFA  TABLES   PF_VEKP_O STRUCTURE VEKP
                                      PF_VEPO_O STRUCTURE VEPO
                                      PF_VBFA_I STRUCTURE VBFA.
  DATA: PF_VBFA TYPE STANDARD TABLE OF VBFA.
  CLEAR: PF_VEKP_O, PF_VEKP_O[], PF_VEPO_O, PF_VEPO_O[].

  APPEND LINES OF PF_VBFA_I TO PF_VBFA.
  DELETE PF_VBFA WHERE VBTYP_N <> 'X'.
  SORT PF_VBFA BY VBELN.
  DELETE ADJACENT DUPLICATES FROM PF_VBFA COMPARING VBELN.
  CHECK PF_VBFA[] IS NOT INITIAL.
  SELECT * FROM VEKP
    INTO CORRESPONDING FIELDS OF TABLE PF_VEKP_O
                                 FOR ALL ENTRIES IN PF_VBFA
                                 WHERE VENUM = PF_VBFA-VBELN.
  SELECT * FROM VEPO
    INTO CORRESPONDING FIELDS OF TABLE PF_VEPO_O
                                 FOR ALL ENTRIES IN PF_VBFA
                                 WHERE VENUM = PF_VBFA-VBELN.
ENDFORM.                    " GET_VEKP_VEPO_FRM_VBFA
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PFWA_POPL_IO_CARTN  text
*----------------------------------------------------------------------*
FORM CONVERSION_EXIT_ALPHA_INPUT  CHANGING PFV_ALPHA_IO.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT         = PFV_ALPHA_IO
    IMPORTING
      OUTPUT        = PFV_ALPHA_IO.
ENDFORM.                    " CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_BOX_DIMENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PFWA_VEKP  text
*      <--P_PFWA_POPL_IO_DIMEN  text
*----------------------------------------------------------------------*
FORM GET_BOX_DIMENSION  USING    PFWA_VEKP_I TYPE VEKP
                        CHANGING PFV_DIMEN_O.
  DATA: PFWA_MARA TYPE MARA,
        PFN_LAENG TYPE I,
        PFN_BREIT TYPE I,
        PFN_HOEHE TYPE I,
        PFV_LAENG TYPE STRING,
        PFV_BREIT TYPE STRING,
        PFV_HOEHE TYPE STRING.
  CLEAR: PFV_DIMEN_O.
  IF PFWA_VEKP_I-LAENG IS INITIAL AND
     PFWA_VEKP_I-BREIT IS INITIAL AND
     PFWA_VEKP_I-HOEHE IS INITIAL.
    PERFORM GET_WORKAREA_MARA USING     PFWA_VEKP_I-VHILM
                              CHANGING  PFWA_MARA.
    IF PFWA_MARA-GROES+0(1) = 'x' OR
       PFWA_MARA-GROES+0(1) = 'X'.
      MESSAGE E700 WITH 'Format of Dimension is not correct.'.
      EXIT.
    ENDIF.

    PFV_DIMEN_O = PFWA_MARA-GROES.
    TRANSLATE PFV_DIMEN_O TO LOWER CASE.
    EXIT.
  ENDIF.

  PFN_LAENG = PFWA_VEKP_I-LAENG.                                                                  "Length
  PFN_BREIT = PFWA_VEKP_I-BREIT.                                                                  "Breadth
  PFN_HOEHE = PFWA_VEKP_I-HOEHE.                                                                  "Height
  PFV_LAENG = PFN_LAENG.
  PFV_BREIT = PFN_BREIT.
  PFV_HOEHE = PFN_HOEHE.
  CONCATENATE PFV_LAENG 'x' PFV_BREIT 'x' PFV_HOEHE INTO PFV_DIMEN_O
      SEPARATED BY SPACE.
ENDFORM.                    " GET_BOX_DIMENSION
*&---------------------------------------------------------------------*
*&      Form  GET_WORKAREA_MARA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PFWA_VEKP_I_VHILM  text
*      <--P_PFWA_MARA  text
*----------------------------------------------------------------------*
FORM GET_WORKAREA_MARA  USING    PFV_MATNR_I
                        CHANGING PFWA_MARA_O TYPE MARA.
  CLEAR: PFWA_MARA_O.
  SELECT SINGLE * FROM MARA
    INTO CORRESPONDING FIELDS OF PFWA_MARA_O
                              WHERE MATNR = PFV_MATNR_I.
ENDFORM.                    " GET_WORKAREA_MARA
*&---------------------------------------------------------------------*
*&      Form  CHECK_POPL_ITEM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_POPL  text
*----------------------------------------------------------------------*
FORM CHECK_POPL_ITEM_DATA  USING PFV_FLGPL_I.
  RANGES: PFR_VGBEL FOR LIKP-VBELN.

  DATA: PFWA_POPL TYPE STR_POPL,
        PFWA_EROR TYPE STR_EROR.


  CHECK PFV_FLGPL_I IS NOT INITIAL.
  CLEAR: I_EROR[].
  "把沒有LOT ID的刪除
  LOOP AT I_POPL INTO PFWA_POPL WHERE CHARG IS INITIAL.
    DELETE I_POPL FROM PFWA_POPL.
    PFR_VGBEL-SIGN   = 'I'.
    PFR_VGBEL-OPTION = 'EQ'.
    PFR_VGBEL-LOW    = PFWA_POPL-VGBEL.
    APPEND PFR_VGBEL.
    CLEAR: PFR_VGBEL.
  ENDLOOP.
  SORT PFR_VGBEL BY LOW.
  DELETE ADJACENT DUPLICATES FROM PFR_VGBEL COMPARING ALL FIELDS.
  "如果ITEM都沒值,HEAD就不要留
  LOOP AT PFR_VGBEL.
    READ TABLE I_POPL INTO PFWA_POPL WITH KEY VGBEL = PFR_VGBEL-LOW.
    CHECK PFWA_POPL IS INITIAL.
    DELETE I_HEAD WHERE VBELN = PFR_VGBEL-LOW.
  ENDLOOP.
  "把沒有SHIP-TO NAME的收集下來
  LOOP AT I_POPL INTO PFWA_POPL WHERE NAME1 IS INITIAL.
*    PFWA_EROR-FNCTN = 'P'.
    PFWA_EROR-VBELN = PFWA_POPL-VGBEL.
    PFWA_EROR-TEXTS = 'NO Ship To Code(ZB2BI1_CODE)'.
    APPEND PFWA_EROR TO I_EROR.
  ENDLOOP.
ENDFORM.                    " CHECK_POPL_ITEM_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_BILL_ITEM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BILL  text
*----------------------------------------------------------------------*
FORM GET_BILL_ITEM_DATA  USING PFV_FLGBI_I.
  DATA: PFWA_HEAD TYPE STR_HEAD,
        PFWA_VBRP TYPE VBRP,
        PFWA_LIPS TYPE LIPS,
        PFWA_BILL TYPE STR_BILL,
        PF_VBRP TYPE STANDARD TABLE OF VBRP,
        PF_LIPS TYPE STANDARD TABLE OF LIPS,
        PF_ZG20 TYPE STANDARD TABLE OF ZG20,
        PF_VBKD TYPE STANDARD TABLE OF VBKD,
        PF_KONV TYPE STANDARD TABLE OF KONV.

  CHECK PFV_FLGBI_I IS NOT INITIAL.
  "Billing Item
  PERFORM GET_VBRP_FRM_HEAD TABLES PF_VBRP.
  "DN Item
  PERFORM GET_LIPS_FRM_VBRP TABLES PF_LIPS
                                   PF_VBRP.
  "發票資訊
  PERFORM GET_ZG20_FRM_HEAD TABLES PF_ZG20.
  "客戶PO資訊
  PERFORM GET_VBKD_FRM_LIPS TABLES PF_VBKD
                                   PF_LIPS.
  "價格Conditions
  PERFORM GET_KONV_FRM_HEAD TABLES PF_KONV.

  LOOP AT I_HEAD INTO PFWA_HEAD WHERE FNCTN = 'B'.
    LOOP AT PF_VBRP INTO PFWA_VBRP WHERE VBELN = PFWA_HEAD-VBELN.
      LOOP AT PF_LIPS INTO PFWA_LIPS WHERE VBELN = PFWA_VBRP-VGBEL
                                     AND   UECHA = PFWA_VBRP-VGPOS.
        PFWA_BILL-NAME1 = PFWA_BILL-NAME2 = 'PSMC'.
        PFWA_BILL-WAERK = PFWA_BILL-PWAER = 'USD'.
        PFWA_BILL-SEQTY = '0'.
        IF PFWA_BILL-WAERK = PFWA_BILL-PWAER.
          PFWA_BILL-KURRF = 1.
        ENDIF.

        PFWA_BILL-VBELN = PFWA_HEAD-VBELN.
        PFWA_BILL-GNETW = PFWA_VBRP-NETWR.
        PFWA_BILL-LFIMG = PFWA_LIPS-LFIMG.
        PFWA_BILL-MEINS = PFWA_LIPS-MEINS.
        PFWA_BILL-VGBEL = PFWA_LIPS-VBELN.
        PFWA_BILL-VGPOS = PFWA_LIPS-POSNR+1(5).
        PFWA_BILL-KDMAT = PFWA_LIPS-KDMAT.
        PFWA_BILL-CHARG = PFWA_LIPS-CHARG.
        PFWA_BILL-OCHAR = PFWA_LIPS-CHARG.
        "GUINO/WRBTR/WMWST/KURSK/TAXID/TOTAL/SIDAT/GUIDT
        PERFORM GET_GUI_INFO TABLES   PF_ZG20
                             USING    PFWA_HEAD-VBELN
                             CHANGING PFWA_BILL.
        "BSTKD
        PERFORM GET_CUST_PO_INFO TABLES    PF_VBKD
                                 USING     PFWA_LIPS
                                 CHANGING  PFWA_BILL-BSTKD.
        "NETPR/NETWR
        PERFORM GET_PRICE_INFO TABLES   PF_KONV
                               USING    PFWA_HEAD-KNUMV
                                        PFWA_VBRP-POSNR
                               CHANGING PFWA_BILL.
        APPEND PFWA_BILL TO I_BILL.
        CLEAR: PFWA_BILL.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_BILL_ITEM_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_SCREEN .
  CHECK SY-BATCH IS INITIAL.
  CALL SCREEN 100.
ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SEND_ERROR_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_ERROR_MAIL.
  RANGES: PFR_VBELN FOR VBRK-VBELN.
  DATA: PFWA_EROR   TYPE STR_EROR,
        PFWA_SUBJT  TYPE SODOCCHGI1,
        PFWA_VKORG  TYPE STR_HEAD,
        PF_RECE     TYPE STANDARD TABLE OF SOMLRECI1,
        PF_CONT     TYPE STANDARD TABLE OF SOLISTI1,
        PF_PACK     TYPE STANDARD TABLE OF SOPCKLSTI1,
        PF_VKORG    TYPE STANDARD TABLE OF STR_HEAD.

  CHECK SY-BATCH IS NOT INITIAL.
  CHECK I_EROR[] IS NOT INITIAL.

  LOOP AT I_EROR INTO PFWA_EROR.
    PFR_VBELN-SIGN   = 'I'.
    PFR_VBELN-OPTION = 'EQ'.
    PFR_VBELN-LOW    = PFWA_EROR-VBELN.
    APPEND PFR_VBELN.
    CLEAR: PFR_VBELN.
  ENDLOOP.
  "
  APPEND LINES OF I_HEAD TO PF_VKORG.
  DELETE PF_VKORG WHERE VBELN NOT IN PFR_VBELN.
  SORT PF_VKORG BY VKORG.
  DELETE ADJACENT DUPLICATES FROM PF_VKORG COMPARING VKORG.

  "JOB不要留下有錯的
  DELETE I_HEAD WHERE VBELN IN PFR_VBELN.

  LOOP AT PF_VKORG INTO PFWA_VKORG.
    "收件人
    PERFORM GET_MAIL_LIST TABLES PF_RECE
                          USING  PFWA_VKORG-VKORG.
    "主旨
    PERFORM GET_MAIL_SUBJECT  USING    PFWA_VKORG-VKORG
                              CHANGING PFWA_SUBJT.
    "內容
    PERFORM GET_MAIL_CONTENT TABLES PF_CONT
                                    PF_PACK
                             USING  PFWA_VKORG-VKORG.
    "沒有收件人就不寄
    CHECK PF_RECE[] IS NOT INITIAL.
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        DOCUMENT_DATA              = PFWA_SUBJT
        PUT_IN_OUTBOX              = 'X'
        COMMIT_WORK                = 'X'
      TABLES
        PACKING_LIST               = PF_PACK
*        CONTENTS_BIN               =
        RECEIVERS                  = PF_RECE
        CONTENTS_TXT               = PF_CONT
      EXCEPTIONS
        TOO_MANY_RECEIVERS         = 1
        DOCUMENT_NOT_SENT          = 2
        DOCUMENT_TYPE_NOT_EXIST    = 3
        OPERATION_NO_AUTHORIZATION = 4
        PARAMETER_ERROR            = 5
        X_ERROR                    = 6
        ENQUEUE_ERROR              = 7
        OTHERS                     = 8.
    CHECK SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    CONTINUE.
  ENDLOOP.
ENDFORM.                    " SEND_ERROR_MAIL
*&---------------------------------------------------------------------*
*&      Form  GET_MAIL_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_RECE  text
*----------------------------------------------------------------------*
FORM GET_MAIL_LIST  TABLES PF_RECE_O STRUCTURE SOMLRECI1
                    USING  PFV_VKORG_I.
  DATA: PFWA_RECE  TYPE SOMLRECI1,
        PFWA_ZSDEL TYPE ZSDEL,
        PF_ZSDEL   TYPE STANDARD TABLE OF ZSDEL,
        PFV_MTYPE  TYPE CHAR2 VALUE 'P'.
  CLEAR: PF_RECE_O, PF_RECE_O[].

  "預設是A,如果是Billing就要換B
  IF P_BILL IS NOT INITIAL.
    PFV_MTYPE = 'B'.
  ENDIF.

  "先抓不分ORG
  SELECT * FROM ZSDEL
    INTO CORRESPONDING FIELDS OF TABLE PF_ZSDEL
                                 WHERE REPID = SY-CPROG
                                 AND   TYPE  = PFV_MTYPE
                                 AND   KUNAG = P_KUNAG
                                 AND   VKORG = ''.
  "再抓By ORG
  SELECT * FROM ZSDEL
    APPENDING CORRESPONDING FIELDS OF TABLE PF_ZSDEL
                                 WHERE REPID = SY-CPROG
                                 AND   TYPE  = PFV_MTYPE
                                 AND   KUNAG = P_KUNAG
                                 AND   VKORG = PFV_VKORG_I.

  LOOP AT PF_ZSDEL INTO PFWA_ZSDEL.
    PFWA_RECE-RECEIVER = PFWA_ZSDEL-RECEXTNAM.
    PFWA_RECE-REC_TYPE = 'U'.
    APPEND PFWA_RECE TO PF_RECE_O.
    CLEAR: PFWA_RECE.
  ENDLOOP.

ENDFORM.                    " GET_MAIL_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_MAIL_SUBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_PFWA_SUBJT  text
*----------------------------------------------------------------------*
FORM GET_MAIL_SUBJECT USING    PFV_VKORG_I
                      CHANGING PFWA_SUBJT_O TYPE SODOCCHGI1.

  CLEAR: PFWA_SUBJT_O-OBJ_DESCR.
  IF P_POPL IS NOT INITIAL.
    CONCATENATE '[注意]Send DN to Raydium(' PFV_VKORG_I ')有Error!(確認完請記得重送)'
      INTO PFWA_SUBJT_O-OBJ_DESCR.
  ENDIF.
  IF P_BILL IS NOT INITIAL.
    CONCATENATE '[注意]Send Bill to Raydium(' PFV_VKORG_I ')有Error!(確認完請記得重送)'
      INTO PFWA_SUBJT_O-OBJ_DESCR.
  ENDIF.
ENDFORM.                    " GET_MAIL_SUBJECT
*&---------------------------------------------------------------------*
*&      Form  GET_MAIL_CONTENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_CONT  text
*----------------------------------------------------------------------*
FORM GET_MAIL_CONTENT  TABLES   PF_CONT_O STRUCTURE SOLISTI1
                                PF_PACK_O STRUCTURE SOPCKLSTI1
                       USING    PFV_VKORG_I.
  DATA: PFWA_CONT     TYPE SOLISTI1,
        PFWA_PACK     TYPE SOPCKLSTI1,
        PFWA_EROR     TYPE STR_EROR,
        PFWA_HEAD     TYPE STR_HEAD,
        PFV_LINES     TYPE I,
        PFV_DATES(10) TYPE C,
        PFV_TIMES(10) TYPE C.

*定義信件內容

  "1.Error Log
  LOOP AT I_EROR INTO PFWA_EROR.
    "檢查該筆ERROR是否屬於這次MAIL內容所屬的Sales Org.
    READ TABLE I_HEAD INTO PFWA_HEAD WITH KEY VBELN = PFWA_EROR-VBELN
                                              VKORG = PFV_VKORG_I.
    CHECK PFWA_HEAD IS NOT INITIAL.
    WRITE: PFWA_EROR-VBELN TO PFV_DATES.
    CONCATENATE PFV_DATES '-' PFWA_EROR-TEXTS
      INTO PFWA_CONT-LINE SEPARATED BY SPACE.
    APPEND PFWA_CONT TO PF_CONT_O.
    CLEAR: PFWA_CONT.
  ENDLOOP.
  "2.做空白行
  APPEND  PFWA_CONT TO PF_CONT_O.
  "3.建立時間
  WRITE: SY-DATUM TO PFV_DATES,
         SY-UZEIT TO PFV_TIMES.
  CONCATENATE 'Create on :' PFV_DATES PFV_TIMES
    INTO PFWA_CONT-LINE SEPARATED BY SPACE.
  APPEND PFWA_CONT TO PF_CONT_O.
  CLEAR: PFWA_CONT.
  "4.程式資訊
  CONCATENATE 'Program ID:' SY-CPROG
    INTO PFWA_CONT-LINE SEPARATED BY SPACE.
  APPEND PFWA_CONT TO PF_CONT_O.
  CLEAR: PFWA_CONT.

  "計算行數
  DESCRIBE TABLE PF_CONT_O LINES PFV_LINES.

  "Mail內容的定義
  PFWA_PACK-TRANSF_BIN  = ''.
  PFWA_PACK-HEAD_START  = 1.
  PFWA_PACK-HEAD_NUM    = 0.
  PFWA_PACK-BODY_START  = 1.
  PFWA_PACK-BODY_NUM    = PFV_LINES.
  PFWA_PACK-DOC_TYPE    = 'TXT'.
  PFWA_PACK-OBJ_NAME    = ''.
  APPEND PFWA_PACK TO PF_PACK_O.
ENDFORM.                    " GET_MAIL_CONTENT
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA_TO_SEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREPARE_DATA_TO_SEND .
  DATA: PFWA_HEAD TYPE STR_HEAD.
  CHECK SY-BATCH IS NOT INITIAL.
  PFWA_HEAD-SELEC = 'X'.
  "有送過的就不再送(這個在最前面的篩選就篩掉了)
  MODIFY I_HEAD FROM PFWA_HEAD
    TRANSPORTING SELEC
    WHERE SELEC IS INITIAL.

  PERFORM SEND_DATA_TO_B2BSERVER_POPL USING P_POPL.
  PERFORM SEND_DATA_TO_B2BSERVER_BILL USING P_BILL.

ENDFORM.                    " PREPARE_DATA_TO_SEND
*&---------------------------------------------------------------------*
*&      Form  GET_CANCEL_BILLING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_VBRK_CAN  text
*----------------------------------------------------------------------*
FORM GET_CANCEL_BILLING  TABLES   PF_CVBRK_O STRUCTURE VBRK.
  DATA: PF_ZB2BI1 TYPE STANDARD TABLE OF ZB2BI1.
  CLEAR: PF_CVBRK_O, PF_CVBRK_O[].

  PERFORM GET_CURRENT_MONTH_SENT_LOG TABLES PF_ZB2BI1.

  CHECK PF_ZB2BI1[] IS NOT INITIAL.
  SELECT * FROM VBRK
    INTO CORRESPONDING FIELDS OF TABLE PF_CVBRK_O
                                 FOR ALL ENTRIES IN PF_ZB2BI1
                                 WHERE VBELN = PF_ZB2BI1-VBELN
                                 AND   FKSTO = 'X'.
ENDFORM.                    " GET_CANCEL_BILLING
*&---------------------------------------------------------------------*
*&      Form  GET_MONTH_BEGIN_AND_END_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DATUM  text
*      <--P_PFV_SDATE  text
*      <--P_PFV_EDATE  text
*----------------------------------------------------------------------*
FORM GET_MONTH_BEGIN_AND_END_DATE  USING    PFV_DATUM_I
                                   CHANGING PFV_SDATE_O
                                            PFV_EDATE_O.
  CLEAR: PFV_SDATE_O, PFV_EDATE_O.
  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      IV_DATE                   = PFV_DATUM_I
    IMPORTING
      EV_MONTH_BEGIN_DATE       = PFV_SDATE_O
      EV_MONTH_END_DATE         = PFV_EDATE_O.
ENDFORM.                    " GET_MONTH_BEGIN_AND_END_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_MONTH_SENT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_ZB2BI1  text
*----------------------------------------------------------------------*
FORM GET_CURRENT_MONTH_SENT_LOG  TABLES PF_ZB2BI1_O STRUCTURE ZB2BI1.
  DATA: PFV_SDATE TYPE SYDATUM,
        PFV_EDATE TYPE SYDATUM.


  PERFORM GET_MONTH_BEGIN_AND_END_DATE USING    SY-DATUM
                                       CHANGING PFV_SDATE
                                                PFV_EDATE.

  SELECT * FROM  ZB2BI1
    INTO CORRESPONDING FIELDS OF TABLE PF_ZB2BI1_O
                                 WHERE FOR_CUST = V_FCUST_BI
                                 AND   KUNAG    = P_KUNAG
                                 AND   DATUM    BETWEEN PFV_SDATE AND PFV_EDATE.
ENDFORM.                    " GET_CURRENT_MONTH_SENT_LOG
*&---------------------------------------------------------------------*
*&      Form  GET_VBRP_FRM_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_VBRP  text
*----------------------------------------------------------------------*
FORM GET_VBRP_FRM_HEAD  TABLES   PF_VBRP_O STRUCTURE VBRP.
  DATA: PF_HEAD TYPE STANDARD TABLE OF STR_HEAD.

  CLEAR: PF_VBRP_O, PF_VBRP_O[].

  APPEND LINES OF I_HEAD TO PF_HEAD.
  DELETE PF_HEAD WHERE FNCTN <> 'B'.
  CHECK PF_HEAD[] IS NOT INITIAL.
  SELECT * FROM VBRP
    INTO CORRESPONDING FIELDS OF TABLE PF_VBRP_O
                                 FOR ALL ENTRIES IN PF_HEAD
                                 WHERE VBELN =  PF_HEAD-VBELN.
ENDFORM.                    " GET_VBRP_FRM_HEAD
*&---------------------------------------------------------------------*
*&      Form  GET_LIPS_FRM_VBRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_LIPS  text
*      -->P_PF_VBRP  text
*----------------------------------------------------------------------*
FORM GET_LIPS_FRM_VBRP  TABLES   PF_LIPS_O STRUCTURE LIPS
                                 PF_VBRP_I STRUCTURE VBRP.
  CLEAR: PF_LIPS_O, PF_LIPS_O[].
  CHECK PF_VBRP_I[] IS NOT INITIAL.
  SELECT * FROM LIPS
    INTO CORRESPONDING FIELDS OF TABLE PF_LIPS_O
                                 FOR ALL ENTRIES IN PF_VBRP_I
                                 WHERE VBELN = PF_VBRP_I-VGBEL
                                 AND   UECHA = PF_VBRP_I-VGPOS.
ENDFORM.                    " GET_LIPS_FRM_VBRP
*&---------------------------------------------------------------------*
*&      Form  GET_ZG20_FRM_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_ZG20  text
*----------------------------------------------------------------------*
FORM GET_ZG20_FRM_HEAD  TABLES   PF_ZG20_O STRUCTURE ZG20.
  DATA: PF_HEAD TYPE STANDARD TABLE OF STR_HEAD.

  CLEAR: PF_ZG20_O, PF_ZG20_O[].

  APPEND LINES OF I_HEAD TO PF_HEAD.
  DELETE PF_HEAD WHERE FNCTN <> 'B'.
  CHECK PF_HEAD[] IS NOT INITIAL.
  SELECT * FROM ZG20
    INTO CORRESPONDING FIELDS OF TABLE PF_ZG20_O
                                 FOR ALL ENTRIES IN PF_HEAD
                                 WHERE INV_NO =  PF_HEAD-VBELN.
ENDFORM.                    " GET_ZG20_FRM_HEAD
*&---------------------------------------------------------------------*
*&      Form  GET_KONV_FRM_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_KONV  text
*----------------------------------------------------------------------*
FORM GET_KONV_FRM_HEAD  TABLES   PF_KONV_O STRUCTURE KONV.
  DATA: PF_HEAD TYPE STANDARD TABLE OF STR_HEAD.

  CLEAR: PF_KONV_O, PF_KONV_O[].

  APPEND LINES OF I_HEAD TO PF_HEAD.
  DELETE PF_HEAD WHERE FNCTN <> 'B'.
  CHECK PF_HEAD[] IS NOT INITIAL.
  SELECT * FROM KONV
    INTO CORRESPONDING FIELDS OF TABLE PF_KONV_O
                                 FOR ALL ENTRIES IN PF_HEAD
                                 WHERE KNUMV = PF_HEAD-KNUMV
                                 "AND   KPOSN = I_VBRP-POSNR
                                 AND   KSCHL = 'PR00'
                                 AND   KINAK <> 'X'
                                 AND   KPEIN <> '0'.
ENDFORM.                    " GET_KONV_FRM_HEAD
*&---------------------------------------------------------------------*
*&      Form  GET_GUI_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PFWA_HEAD_VBELN  text
*      <--P_PFWA_BILL  text
*----------------------------------------------------------------------*
FORM GET_GUI_INFO  TABLES   PF_ZG20_I    STRUCTURE ZG20
                   USING    PFV_VBELN_I
                   CHANGING PFWA_BILL_IO TYPE STR_BILL.
  DATA: PFWA_ZG20 TYPE ZG20.

  READ TABLE PF_ZG20_I INTO PFWA_ZG20 WITH KEY INV_NO = PFV_VBELN_I.
  CHECK PFWA_ZG20 IS NOT INITIAL.
  PFWA_BILL_IO-GUINO = PFWA_ZG20-GUI_NO.
  PFWA_BILL_IO-WRBTR = PFWA_ZG20-S_AMT.
  PFWA_BILL_IO-WMWST = PFWA_ZG20-T_AMT.
  PFWA_BILL_IO-KURSK = PFWA_ZG20-RATE.
  PFWA_BILL_IO-SIDAT = PFWA_BILL_IO-GUIDT = PFWA_ZG20-POST_DATE.
  PFWA_BILL_IO-TOTAL = PFWA_ZG20-S_AMT + PFWA_ZG20-T_AMT.
  PFWA_BILL_IO-TAXID = PFWA_ZG20-MWSKZ.
  IF PFWA_ZG20-MWSKZ = 'S3'.
    PFWA_BILL_IO-TAXID = '免稅'.
  ENDIF.
ENDFORM.                    " GET_GUI_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_PRICE_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_KONV  text
*      <--P_PFWA_BILL  text
*----------------------------------------------------------------------*
FORM GET_PRICE_INFO  TABLES   PF_KONV_I    STRUCTURE KONV
                     USING    PFV_KNUMV_I
                              PFV_POSNR_I
                     CHANGING PFWA_BILL_IO TYPE STR_BILL.
  DATA: PF_KONV_CURR TYPE STANDARD TABLE OF KONV,
        PFWA_KONV    TYPE KONV.

  CLEAR: PFWA_BILL_IO-NETPR, PFWA_BILL_IO-NETWR.
  APPEND LINES OF PF_KONV_I TO PF_KONV_CURR.
  DELETE PF_KONV_CURR WHERE KNUMV <> PFV_KNUMV_I.
  DELETE PF_KONV_CURR WHERE KPOSN <> PFV_POSNR_I.
  CHECK PF_KONV_CURR[] IS NOT INITIAL.
  READ TABLE PF_KONV_CURR
    INTO PFWA_KONV INDEX 1.
  PFWA_BILL_IO-NETPR = PFWA_KONV-KBETR / PFWA_KONV-KPEIN.
  PFWA_BILL_IO-NETWR = PFWA_BILL_IO-LFIMG * PFWA_KONV-KBETR / PFWA_KONV-KPEIN.
ENDFORM.                    " GET_PRICE_INFO
*&---------------------------------------------------------------------*
*&      Form  CHECK_BILL_ITEM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BILL  text
*----------------------------------------------------------------------*
FORM CHECK_BILL_ITEM_DATA  USING    PFV_FLGBI_I.
  DATA: PFWA_HEAD TYPE STR_HEAD,
        PFWA_BILL TYPE STR_BILL,
        PFWA_EROR TYPE STR_EROR.

  CHECK PFV_FLGBI_I IS NOT INITIAL.
  CLEAR: I_EROR[].

  LOOP AT I_BILL INTO PFWA_BILL WHERE VBELN = PFWA_HEAD-VBELN
                                AND   GUINO IS INITIAL.
*    PFWA_EROR-FNCTN = 'B'.
    PFWA_EROR-VBELN = PFWA_BILL-VBELN.
    PFWA_EROR-TEXTS = '還未開立統一發票/收據'.
    APPEND PFWA_EROR TO I_EROR.
    CLEAR: PFWA_EROR.
  ENDLOOP.
ENDFORM.                    " CHECK_BILL_ITEM_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_ERROR_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_ERROR_LOG .
  CHECK SY-BATCH IS INITIAL.
  CHECK I_EROR[] IS NOT INITIAL.
  CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
    EXPORTING
      ENDPOS_COL         =  70
      ENDPOS_ROW         =  12
      STARTPOS_COL       =  29
      STARTPOS_ROW       =  1
      TITLETEXT          =  'ERROR LOG'
*   IMPORTING
*     CHOISE             =
    TABLES
      VALUETAB           =  I_EROR
    EXCEPTIONS
      BREAK_OFF          =  1.
ENDFORM.                    " SHOW_ERROR_LOG
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA_TO_B2BSERVER_POPL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_POPL  text
*----------------------------------------------------------------------*
FORM SEND_DATA_TO_B2BSERVER_POPL  USING PFV_FLGPL_I.
  DATA: PFV_DESTN(20) TYPE C,
        PFWA_HEAD     TYPE STR_HEAD,
        PFWA_POPL     TYPE STR_POPL,
        PFWA_RADM     TYPE ZSTR_RADYIUM,
        PFWA_B2B1     TYPE ZB2BI1,
        PFWA_B2B2     TYPE ZB2BI2,
        PF_ZB2BI1     TYPE STANDARD TABLE OF ZB2BI1,
        PF_ZB2BI2     TYPE STANDARD TABLE OF ZB2BI2,
        PF_MRAD       TYPE STANDARD TABLE OF ZSTR_RADYIUM,"8"資料
        PF_PRAD       TYPE STANDARD TABLE OF ZSTR_RADYIUM."12"資料
  CHECK PFV_FLGPL_I IS NOT INITIAL.
  CHECK I_HEAD[] IS NOT INITIAL AND
        I_POPL[] IS NOT INITIAL.
  "把有選的部份分別放入8"及12"的TABLE
  LOOP AT I_HEAD INTO PFWA_HEAD WHERE SELEC IS NOT INITIAL
                                AND   FNCTN = 'P'.
    PFWA_B2B1-VBELN    = PFWA_B2B2-VBELN    = PFWA_HEAD-VBELN.
    PFWA_B2B1-KUNAG    = PFWA_HEAD-KUNAG.
    PFWA_B2B1-FOR_CUST = PFWA_B2B2-FOR_CUST = V_FCUST_PL.
    PFWA_B2B1-DATUM    = PFWA_B2B2-DATUM    = SY-DATUM.
    PFWA_B2B1-UZEIT    = PFWA_B2B2-UZEIT    = SY-UZEIT.
    PFWA_B2B1-ERNAM    = PFWA_B2B2-ERNAM    = SY-UNAME.
    PFWA_B2B1-REMARK   = PFWA_B2B2-REMARK   = PFWA_HEAD-RESON.      "重寄會有REASON
    CONCATENATE SY-REPID '-RAYDIUM Shipping'
      INTO PFWA_B2B2-REMARK.
    LOOP AT I_POPL INTO PFWA_POPL WHERE VGBEL = PFWA_HEAD-VBELN.
      WRITE:  PFWA_POPL-VGBEL TO PFWA_RADM-VBELN,
              PFWA_POPL-LFIMG UNIT PFWA_POPL-MEINS TO PFWA_RADM-LFIMG,
              PFWA_POPL-CARTN TO PFWA_RADM-CARTN,
              PFWA_POPL-NTGEW UNIT PFWA_POPL-GEWEI TO PFWA_RADM-NWEIG,
              PFWA_POPL-BRGEW UNIT PFWA_POPL-GEWEI TO PFWA_RADM-GWEIG,
              PFWA_POPL-EXIDV TO PFWA_RADM-LCART.

      CONCATENATE PFWA_POPL-WADAT+0(4) '/' PFWA_POPL-WADAT+4(2) '/' PFWA_POPL-WADAT+6(2)
        INTO PFWA_RADM-WADAT_IST.
      CONCATENATE PFWA_POPL-SIDAT+0(4) '/' PFWA_POPL-SIDAT+4(2) '/' PFWA_POPL-SIDAT+6(2)
        INTO PFWA_RADM-INVDT.

      PFWA_RADM-NAME1 = PFWA_POPL-NAME1.
      PFWA_RADM-POSNR = PFWA_POPL-VGPOS.
      SHIFT PFWA_RADM-POSNR LEFT DELETING LEADING '0'.
      PFWA_RADM-NAME2 = PFWA_POPL-NAME2.
      PFWA_RADM-CHARG = PFWA_POPL-CHARG.
      PFWA_RADM-ELOTN = PFWA_POPL-ELOTN.
      PFWA_RADM-KDMAT = PFWA_POPL-KDMAT.
      PFWA_RADM-BSTKD = PFWA_POPL-BSTKD.
      PFWA_RADM-WAFER = PFWA_POPL-WAFER.
      PFWA_RADM-DIMEN = PFWA_POPL-DIMEN.
      PFWA_RADM-ZMARK = PFWA_POPL-ZMARK.

      IF PFWA_HEAD-VKORG = 'MAX1'.
        APPEND PFWA_RADM TO PF_MRAD.
      ENDIF.
      IF PFWA_HEAD-VKORG = 'PSC1'.
        APPEND PFWA_RADM TO PF_PRAD.
      ENDIF.
      CLEAR: PFWA_RADM.

      PFWA_B2B2-CHARG = PFWA_POPL-CHARG.
      APPEND PFWA_B2B2 TO PF_ZB2BI2.
      CLEAR: PFWA_B2B2-CHARG.
    ENDLOOP.

    APPEND PFWA_B2B1 TO PF_ZB2BI1.
    CLEAR: PFWA_B2B1, PFWA_B2B2.
  ENDLOOP.

  IF PF_MRAD[] IS NOT INITIAL.
    PERFORM GET_B2B_SERVER_DESTINATION USING    'MAX1'
                                       CHANGING PFV_DESTN.
    "X=>連線有問題
    CHECK PFV_DESTN <> 'X'.

    CALL FUNCTION 'Z_RFC_SEND_SHIPALERT_TO_RAD'
         DESTINATION PFV_DESTN
      TABLES
        SI            =  PF_MRAD.
  ENDIF.

  IF PF_PRAD[] IS NOT INITIAL.
    PERFORM GET_B2B_SERVER_DESTINATION USING    'PSC1'
                                       CHANGING PFV_DESTN.
    "X=>連線有問題
    CHECK PFV_DESTN <> 'X'.

    CALL FUNCTION 'Z_RFC_SEND_SHIPALERT_TO_RAD'
         DESTINATION PFV_DESTN
      TABLES
        SI            =  PF_PRAD.
  ENDIF.

  CHECK SY-SUBRC = 0.
  PERFORM UPDATE_TABLE_ZB2BIX TABLES PF_ZB2BI1
                                     PF_ZB2BI2.
  CHECK SY-BATCH IS INITIAL.
  MESSAGE I080 WITH 'Send Ship Complete...'.
  "修改回現在的 I_HEAD資料
  PERFORM MODIFY_SEND_FLAG_FOR_HEAD.
ENDFORM.                    " SEND_DATA_TO_B2BSERVER_POPL
*&---------------------------------------------------------------------*
*&      Form  GET_B2B_SERVER_DESTINATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3805   text
*      <--P_PFV_DESTN  text
*----------------------------------------------------------------------*
FORM GET_B2B_SERVER_DESTINATION  USING    PFV_VKORG_I
                                 CHANGING PFV_DESTN_O.
  DATA: PFV_DITEM TYPE C,
        PFWA_DEST TYPE ZSDDEST.

  CLEAR: PFV_DESTN_O.
  PFV_DITEM = PFV_VKORG_I+0(1).

  IF SY-SYSID <> 'PRD'.
    CASE PFV_VKORG_I.
      WHEN 'MAX1'.
        PFV_DITEM = 'N'.
      WHEN 'PSC1'.
        PFV_DITEM = 'Q'.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

  PERFORM GET_WORKAREA_ZSDDEST USING    PFV_DITEM
                               CHANGING PFWA_DEST.
  IF PFWA_DEST IS INITIAL.
    CHECK SY-BATCH IS INITIAL.
    MESSAGE I080 WITH '請維護Destination的資訊(ZSDDEST)!'.
    PFV_DESTN_O = 'X'.
    EXIT.
  ENDIF.
  PFV_DESTN_O = PFWA_DEST-DEST.
  CALL FUNCTION 'RFC_PING'
    DESTINATION PFV_DESTN_O
    EXCEPTIONS
      SYSTEM_FAILURE        = 1
      COMMUNICATION_FAILURE = 2.
  IF SY-SUBRC <> 0.
    CHECK SY-BATCH IS INITIAL.
    MESSAGE I080 WITH
         'Connection error with B2B Server, please check!(' PFV_VKORG_I ')'.
    PFV_DESTN_O = 'X'.
    EXIT.
  ENDIF.

ENDFORM.                    " GET_B2B_SERVER_DESTINATION
*&---------------------------------------------------------------------*
*&      Form  GET_WORKAREA_ZSDDEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PFV_DITEM  text
*      <--P_PFWA_DEST  text
*----------------------------------------------------------------------*
FORM GET_WORKAREA_ZSDDEST  USING    PFV_DITEM_I
                           CHANGING PFWA_DEST_O TYPE ZSDDEST.
  CLEAR: PFWA_DEST_O.
  SELECT SINGLE * FROM ZSDDEST
    INTO CORRESPONDING FIELDS OF PFWA_DEST_O
                              WHERE REPID = SY-CPROG
                              AND   ITEM  = PFV_DITEM_I.

ENDFORM.                    " GET_WORKAREA_ZSDDEST
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE_ZB2BIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PF_ZB2BI1  text
*      -->P_PF_ZB2BI2  text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE_ZB2BIX  TABLES   PF_ZB2BI1_I STRUCTURE ZB2BI1
                                   PF_ZB2BI2_I STRUCTURE ZB2BI2.
  IF PF_ZB2BI1_I[] IS NOT INITIAL.
    MODIFY ZB2BI1 FROM TABLE PF_ZB2BI1_I.
  ENDIF.

  IF PF_ZB2BI2_I[] IS NOT INITIAL.
    MODIFY ZB2BI2 FROM TABLE PF_ZB2BI2_I.
  ENDIF.
ENDFORM.                    " UPDATE_TABLE_ZB2BIX
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA_TO_B2BSERVER_BILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BILL  text
*----------------------------------------------------------------------*
FORM SEND_DATA_TO_B2BSERVER_BILL  USING    PFV_FLGBI_I.
  DATA: PFV_DESTN(20) TYPE C,
        PFWA_HEAD     TYPE STR_HEAD,
        PFWA_BILL     TYPE STR_BILL,
        PFWA_RADM     TYPE ZSTR_RADYIUM_BILL,
        PFWA_B2B1     TYPE ZB2BI1,
        PFWA_B2B2     TYPE ZB2BI2,
        PF_ZB2BI1     TYPE STANDARD TABLE OF ZB2BI1,
        PF_ZB2BI2     TYPE STANDARD TABLE OF ZB2BI2,
        PF_MRAD       TYPE STANDARD TABLE OF ZSTR_RADYIUM_BILL,     "8"資料
        PF_PRAD       TYPE STANDARD TABLE OF ZSTR_RADYIUM_BILL.     "12"資料

  CHECK PFV_FLGBI_I IS NOT INITIAL.
  CHECK I_HEAD[] IS NOT INITIAL AND
        I_BILL[] IS NOT INITIAL.

  "把有選的部份分別放入8"及12"的TABLE
  LOOP AT I_HEAD INTO PFWA_HEAD WHERE SELEC IS NOT INITIAL
                                AND   FNCTN = 'B'.
    PFWA_B2B1-VBELN    = PFWA_B2B2-VBELN    = PFWA_HEAD-VBELN.
    PFWA_B2B1-KUNAG    = PFWA_HEAD-KUNAG.
    PFWA_B2B1-FOR_CUST = PFWA_B2B2-FOR_CUST = V_FCUST_BI.
    PFWA_B2B1-DATUM    = PFWA_B2B2-DATUM    = SY-DATUM.
    PFWA_B2B1-UZEIT    = PFWA_B2B2-UZEIT    = SY-UZEIT.
    PFWA_B2B1-ERNAM    = PFWA_B2B2-ERNAM    = SY-UNAME.
    PFWA_B2B1-REMARK   = PFWA_B2B2-REMARK   = PFWA_HEAD-RESON.      "重寄會有REASON
    CONCATENATE SY-REPID '-RAYDIUM GUI'
      INTO PFWA_B2B2-REMARK.
    IF PFWA_HEAD-DLBIL IS NOT INITIAL.
      PFWA_B2B1-FOR_CUST = PFWA_B2B2-FOR_CUST = V_FCUST_BC.
    ENDIF.
    LOOP AT I_BILL INTO PFWA_BILL WHERE VBELN = PFWA_HEAD-VBELN.
      WRITE: PFWA_BILL-VBELN TO PFWA_RADM-INVNO,
             PFWA_BILL-WRBTR CURRENCY PFWA_BILL-WAERK TO PFWA_RADM-ITOTL,
             PFWA_BILL-WMWST CURRENCY PFWA_BILL-WAERK TO PFWA_RADM-TAXAM,
             PFWA_BILL-TOTAL CURRENCY PFWA_BILL-WAERK TO PFWA_RADM-TOTAL,
             PFWA_BILL-VGBEL TO PFWA_RADM-VBELN,
             PFWA_BILL-LFIMG UNIT PFWA_BILL-MEINS TO PFWA_RADM-LFIMG,
             PFWA_BILL-NETPR CURRENCY PFWA_BILL-WAERK TO PFWA_RADM-KBETR,
             PFWA_BILL-NETWR CURRENCY PFWA_BILL-WAERK TO PFWA_RADM-NETWR,
             PFWA_BILL-GNETW CURRENCY PFWA_BILL-WAERK TO PFWA_RADM-GUIST.

      CONCATENATE PFWA_BILL-SIDAT+0(4) '/' PFWA_BILL-SIDAT+4(2) '/' PFWA_BILL-SIDAT+6(2)
        INTO PFWA_RADM-SIDAT.
      CONCATENATE PFWA_BILL-GUIDT+0(4) '/' PFWA_BILL-GUIDT+4(2) '/' PFWA_BILL-GUIDT+6(2)
        INTO PFWA_RADM-GUIDT.

      PFWA_RADM-GUINO = PFWA_BILL-GUINO.
      PFWA_RADM-NAME1 = PFWA_BILL-NAME1.
      PFWA_RADM-NAME2 = PFWA_BILL-NAME2.
      PFWA_RADM-WAERK = PFWA_BILL-WAERK.
      PFWA_RADM-KURRF = PFWA_BILL-KURRF.
      PFWA_RADM-PWAER = PFWA_BILL-PWAER.
      PFWA_RADM-TAXID = PFWA_BILL-TAXID.
      PFWA_RADM-POSNR = PFWA_BILL-VGPOS.
      SHIFT PFWA_RADM-POSNR LEFT DELETING LEADING '0'.
      PFWA_RADM-BSTKD = PFWA_BILL-BSTKD.
      PFWA_RADM-KDMAT = PFWA_BILL-KDMAT.
      PFWA_RADM-CHARG = PFWA_BILL-CHARG.
      PFWA_RADM-OCHAR = PFWA_BILL-OCHAR.
      PFWA_RADM-SEQTY = PFWA_BILL-SEQTY.
      PFWA_RADM-KURSK = PFWA_BILL-KURSK.

      IF PFWA_HEAD-VKORG = 'MAX1'.
        APPEND PFWA_RADM TO PF_MRAD.
      ENDIF.
      IF PFWA_HEAD-VKORG = 'PSC1'.
        APPEND PFWA_RADM TO PF_PRAD.
      ENDIF.
      CLEAR: PFWA_RADM.

      PFWA_B2B2-CHARG = PFWA_BILL-CHARG.
      APPEND PFWA_B2B2 TO PF_ZB2BI2.
      CLEAR: PFWA_B2B2-CHARG.
    ENDLOOP.

    APPEND PFWA_B2B1 TO PF_ZB2BI1.
    CLEAR: PFWA_B2B1, PFWA_B2B2.
  ENDLOOP.
  IF PF_MRAD[] IS NOT INITIAL.
    PERFORM GET_B2B_SERVER_DESTINATION USING    'MAX1'
                                       CHANGING PFV_DESTN.
    "X=>連線有問題
    CHECK PFV_DESTN <> 'X'.

    CALL FUNCTION 'Z_RFC_SEND_BILLING_TO_RAD'
         DESTINATION PFV_DESTN
      TABLES
        SI            =  PF_MRAD.
  ENDIF.

  IF PF_PRAD[] IS NOT INITIAL.
    PERFORM GET_B2B_SERVER_DESTINATION USING    'PSC1'
                                       CHANGING PFV_DESTN.
    "X=>連線有問題
    CHECK PFV_DESTN <> 'X'.

    CALL FUNCTION 'Z_RFC_SEND_BILLING_TO_RAD'
         DESTINATION PFV_DESTN
      TABLES
        SI            =  PF_PRAD.
  ENDIF.

  CHECK SY-SUBRC = 0.
  PERFORM UPDATE_TABLE_ZB2BIX TABLES PF_ZB2BI1
                                     PF_ZB2BI2.
  CHECK SY-BATCH IS INITIAL.
  MESSAGE I080 WITH 'Send Invoice Complete...'.
  "修改回現在的 I_HEAD資料
  PERFORM MODIFY_SEND_FLAG_FOR_HEAD.
ENDFORM.                    " SEND_DATA_TO_B2BSERVER_BILL
*&---------------------------------------------------------------------*
*&      Module  STATUS_BY_DYNNR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_BY_DYNNR OUTPUT.
  DATA: MV_DOCTP(10)  TYPE C,
        MV_PSTOP      TYPE C.
  PERFORM GET_TITLE_BY_FUNCTION.
  SET PF-STATUS 'GS100'.
  DESCRIBE TABLE I_HEAD LINES TC100_HEAD-LINES.
  PERFORM GET_VALUE_BY_FUNCTION CHANGING MV_DOCTP.
  PERFORM SAVL_DISPLAY_IN_CONTAINER.
  CLEAR: OK_CODE.
ENDMODULE.                 " STATUS_BY_DYNNR  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_TITLE_BY_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_TITLE_BY_FUNCTION .
  IF P_POPL IS NOT INITIAL.
    SET TITLEBAR 'GEN' WITH 'Send Ship to Radyium'.
  ENDIF.

  IF P_BILL IS NOT INITIAL.
    SET TITLEBAR 'GEN' WITH 'Send GUI to Radyium'.
  ENDIF.
ENDFORM.                    " GET_TITLE_BY_FUNCTION

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  CASE OK_CODE.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_SCREEN_ENTRY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PAI_SCREEN_ENTRY INPUT.
  MODIFY I_HEAD INDEX TC100_HEAD-CURRENT_LINE.
ENDMODULE.                 " PAI_SCREEN_ENTRY  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_BY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_BY_SCREEN INPUT.

  CASE OK_CODE.
    WHEN 'SEND'.
      PERFORM CHECK_AND_REBUILD_SELECTION CHANGING MV_PSTOP.
      CHECK MV_PSTOP IS INITIAL.
      PERFORM SEND_DATA_TO_B2BSERVER_POPL USING P_POPL.
      PERFORM SEND_DATA_TO_B2BSERVER_BILL USING P_BILL.
    WHEN 'ELOG'.
      PERFORM SHOW_ERROR_LOG.
    WHEN 'SALL'.
      I_HEAD-SELEC = 'X'.
      MODIFY I_HEAD TRANSPORTING SELEC
        WHERE SELEC IS INITIAL
        AND   SFLAG IS INITIAL.
    WHEN 'DALL'.
      CLEAR: I_HEAD-SELEC.
      MODIFY I_HEAD TRANSPORTING SELEC
        WHERE SELEC IS NOT INITIAL.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_BY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_VALUE_BY_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VALUE_BY_FUNCTION CHANGING PFV_DOCTP_O.

  CLEAR: PFV_DOCTP_O.
  IF P_POPL IS NOT INITIAL.
    PFV_DOCTP_O = 'Delivery'.
  ENDIF.

  IF P_BILL IS NOT INITIAL.
    PFV_DOCTP_O = 'Billing'.
  ENDIF.
ENDFORM.                    " GET_VALUE_BY_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  CHECK_AND_REBUILD_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_MV_PSTOP  text
*----------------------------------------------------------------------*
FORM CHECK_AND_REBUILD_SELECTION  CHANGING PFV_PSTOP_O.
  DATA: PFV_ANSER   TYPE C,
        PFWA_HEAD   TYPE STR_HEAD,
        PF_HEAD_TMP TYPE STANDARD TABLE OF STR_HEAD.
  CLEAR: PFV_PSTOP_O.

  "處理已寄送過的部份
  LOOP AT I_HEAD INTO PFWA_HEAD WHERE SELEC IS NOT INITIAL
                                AND   SFLAG IS NOT INITIAL.


    PERFORM ASK_RESEND_QUESTION USING     PFWA_HEAD-VBELN
                                          PFWA_HEAD-FNCTN
                                CHANGING  PFV_ANSER
                                          PFWA_HEAD-RESON.
    CHECK PFV_ANSER <> 'J'.
    CLEAR: PFWA_HEAD-SELEC.
    MODIFY I_HEAD FROM PFWA_HEAD.
  ENDLOOP.
  "檢查全部都沒有選
  APPEND LINES OF I_HEAD TO PF_HEAD_TMP.
  DELETE PF_HEAD_TMP WHERE SELEC IS INITIAL.
  IF PF_HEAD_TMP[] IS INITIAL.
    MESSAGE I000 WITH '請至少選擇一筆寄送!!'.
    PFV_PSTOP_O = 'X'.
    EXIT.
  ENDIF.


ENDFORM.                    " CHECK_AND_REBUILD_SELECTION
*&---------------------------------------------------------------------*
*&      Form  ASK_RESEND_QUESTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PFWA_HEAD_VBELN  text
*      -->P_PFWA_HEAD_FNCTN  text
*      <--P_PFV_ANSER  text
*----------------------------------------------------------------------*
FORM ASK_RESEND_QUESTION  USING    PFV_VBELN_I
                                   PFV_FNCTN_I
                          CHANGING PFV_ANSER_O
                                   PFV_RESON_O.
  DATA: PFV_TEXT1(70) TYPE C,
        PFV_TEXT2(70) TYPE C,
        PFV_TEXT3(70) TYPE C,
        PFV_TITLE(40) TYPE C.
  CLEAR: PFV_ANSER_O, PFV_RESON_O.

  WRITE PFV_VBELN_I TO PFV_TEXT1.

  CASE PFV_FNCTN_I.
    WHEN 'P'.
      PFV_TEXT2 = '已送過POPL(Ship) to Raydium'.
      PFV_TITLE = '確認是否重送POPL(Ship)'.
    WHEN 'B'.
      PFV_TEXT2 = '已送過BI(Invoice) to Radyium'.
      PFV_TITLE = '確認是否重送BI(Invoice)'.
    WHEN OTHERS.
  ENDCASE.
  PFV_TEXT3 = '若要重送,請輸入簡單的原因:'.


  CALL FUNCTION 'ZPOPUP_TO_GET_ONE_VALUE'
    EXPORTING
      TEXTLINE1   = PFV_TEXT1
      TEXTLINE2   = PFV_TEXT2
      TEXTLINE3   = PFV_TEXT3
      TITEL       = PFV_TITLE
      VALUELENGTH = 40
    IMPORTING
      ANSWER      = PFV_ANSER_O
      VALUE1      = PFV_RESON_O.
ENDFORM.                    " ASK_RESEND_QUESTION
*&---------------------------------------------------------------------*
*&      Form  SAVL_DISPLAY_IN_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVL_DISPLAY_IN_CONTAINER .
  "建一個容器
  PERFORM SAVL_CREATE_CONTAINER.
  "建ALV架構
  PERFORM SALV_CREATE_ALVOBJECT.
  "處理預設的Button
  PERFORM SALV_FUNCTIONS.
  "顯示設定(斑馬條紋...)
  PERFORM SALV_DISPLAY_SETTINGS.
  "Layout設定(存變式....)
  PERFORM SALV_LAYOUT.
  "選擇設定(單選,多選....)
  PERFORM SALV_SET_SELECTIONS.
  "Column的設定
  PERFORM SALV_COLUMNS.

  SALV_TB->display( ).
ENDFORM.                    " SAVL_DISPLAY_IN_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  SAVL_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVL_CREATE_CONTAINER .
  CHECK SALV_CONTINER IS NOT BOUND.
  CREATE OBJECT SALV_CONTINER
      EXPORTING
        container_name = 'CT_ITEM'.   "Screen上自行定義
ENDFORM.                    " SAVL_CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  SALV_CREATE_ALVOBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALV_CREATE_ALVOBJECT .
  FIELD-SYMBOLS: <rpt_tab> TYPE STANDARD TABLE.

  UNASSIGN <rpt_tab>.
  "POPL Item Data To FIELD-SYMBOLS
  IF P_POPL IS NOT INITIAL.
    ASSIGN I_POPL TO <rpt_tab>.
  ENDIF.
  "BILL Item Data To FIELD-SYMBOLS
  IF P_BILL IS NOT INITIAL.
    ASSIGN I_BILL TO <rpt_tab>.
  ENDIF.

  FREE SALV_TB.

  TRY.
    cl_salv_table=>factory(
      EXPORTING
        r_container = SALV_CONTINER
        container_name = 'CT_ITEM'
      IMPORTING
        r_salv_table = SALV_TB
      CHANGING
        t_table      = <rpt_tab>[] ).
  CATCH cx_salv_msg.
    FREE SALV_TB.
  ENDTRY.
ENDFORM.                    " SALV_CREATE_ALVOBJECT
*&---------------------------------------------------------------------*
*&      Form  SALV_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALV_FUNCTIONS .
  DATA: PCL_FUNCTIONS TYPE REF TO   cl_salv_functions_list.

  PCL_FUNCTIONS = SALV_TB->get_functions( ).
  PCL_FUNCTIONS->set_all( abap_true ).

ENDFORM.                    " SALV_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  SALV_DISPLAY_SETTINGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALV_DISPLAY_SETTINGS .
  DATA: PCLWA_DISPLAY TYPE REF TO cl_salv_display_settings.

  PCLWA_DISPLAY = SALV_TB->get_display_settings( ).
  "斑馬條紋
  PCLWA_DISPLAY->set_striped_pattern( cl_salv_display_settings=>true ).
ENDFORM.                    " SALV_DISPLAY_SETTINGS
*&---------------------------------------------------------------------*
*&      Form  SALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALV_LAYOUT .
  DATA: PCLWA_LAYOUT TYPE REF TO cl_salv_layout,
        PFWA_PROGRAM TYPE salv_s_layout_key.

  PCLWA_LAYOUT = SALV_TB->get_layout( ).
  PFWA_PROGRAM-REPORT = SY-CPROG.
  PCLWA_LAYOUT->set_key( PFWA_PROGRAM ).
  PCLWA_LAYOUT->set_save_restriction( cl_salv_layout=>restrict_none )."允許存變式
ENDFORM.                    " SALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SALV_SET_SELECTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALV_SET_SELECTIONS .
  DATA: PCLWA_SELECTIONS TYPE REF TO cl_salv_selections.
  PCLWA_SELECTIONS = SALV_TB->get_selections( ).
  PCLWA_SELECTIONS->set_selection_mode( if_salv_c_selection_mode=>row_column ).
*  if_salv_c_selection_mode=>SINGLE     單筆篩選
*  if_salv_c_selection_mode=>MULTIPLE   多筆篩選
*  if_salv_c_selection_mode=>CELL       貯存格篩選
*  if_salv_c_selection_mode=>ROW_COLUMN row/column都篩選
*  if_salv_c_selection_mode=>NONE       不做row/column篩選
ENDFORM.                    " SALV_SET_SELECTIONS
*&---------------------------------------------------------------------*
*&      Form  SALV_COLUMNS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SALV_COLUMNS .
  DATA: PCL_COLUMNS   TYPE REF TO cl_salv_columns.

  IF P_POPL IS NOT INITIAL.
    PERFORM SALV_COLUMN_SETTING USING:
          "Col.   Stext     Mtext     LText     Ref.    Ref.Tp EditMask  NoShow
          'VGBEL' ''        ''        ''        ''      ''     '==ALPHA' '',  "Delivery
          'WADAT' ''        ''        ''        ''      ''     ''        '',  "PGI Date
          'NAME1' TEXT-H03  TEXT-H03  TEXT-H03  ''      ''     ''        '',  "ShipTo
          'GUINO' TEXT-H04  TEXT-H04  TEXT-H04  ''      ''     ''        'X', "GUI No.
          'VBELN' ''        ''        ''        ''      ''     '==ALPHA' 'X', "Invoice No.
          'SIDAT' ''        ''        ''        ''      ''     ''        '',  "Invoice Date
          'VGPOS' ''        ''        ''        ''      ''     ''        '',  "Delivery Item
          'NAME2' TEXT-H05  TEXT-H05  TEXT-H05  ''      ''     ''        '',  "VendorName
          'CHARG' ''        ''        ''        ''      ''     ''        '',  "Batch Number
          'ELOTN' TEXT-H06  TEXT-H07  TEXT-H07  ''      ''     ''        '',  "Ex LotNo
          'KDMAT' ''        ''        ''        ''      ''     ''        '',  "Cust. Material Number
          'VDEVC' TEXT-H08  TEXT-H09  TEXT-H09  ''      ''     ''        'X', "Vendor Device
          'BSTKD' ''        ''        ''        ''      ''     ''        '',  "Customer purchase order number
          'LFIMG' ''        ''        ''        'MEINS' 'Q'    ''        '',  "Qty
          'MEINS' ''        ''        ''        ''      ''     ''        'X', "Unit
          'SEQTY' TEXT-H10  TEXT-H11  TEXT-H11  ''      ''     ''        'X', "Ship Qty Each
          'WAFER' TEXT-H12  TEXT-H12  TEXT-H12  ''      ''     ''        '',  "Wafer ID
          'SHMK1' TEXT-H13  TEXT-H13  TEXT-H13  ''      ''     ''        'X', "ShipMark1
          'SHMK2' TEXT-H14  TEXT-H14  TEXT-H14  ''      ''     ''        'X', "ShipMark2
          'SHMK3' TEXT-H15  TEXT-H15  TEXT-H15  ''      ''     ''        'X', "ShipMark3
          'FORWD' TEXT-H16  TEXT-H17  TEXT-H17  ''      ''     ''        'X', "貨運承攬商名稱
          'CARTN' TEXT-H18  TEXT-H18  TEXT-H18  ''      ''     '==ALPHA' '',  "合計箱數
          'NTGEW' ''        ''        ''        'GEWEI' 'Q'    ''        '',  "NetWeight
          'BRGEW' ''        ''        ''        'GEWEI' 'Q'    ''        '',  "GrossWeight
          'GEWEI' ''        ''        ''        ''      ''     ''        'X', "WeightUnit
          'FLIGT' TEXT-H19  TEXT-H19  TEXT-H19  ''      ''     ''        'X', "航班編號
          'ZMAWA' TEXT-H20  TEXT-H20  TEXT-H20  ''      ''     ''        'X', "大提單號
          'ZHAWA' TEXT-H21  TEXT-H21  TEXT-H21  ''      ''     ''        'X', "小提單號
          'EXIDV' TEXT-H22  TEXT-H22  TEXT-H22  ''      ''     '==ALPHA' '',  "箱號
          'DIMEN' TEXT-H23  TEXT-H23  TEXT-H23  ''      ''     ''        '',  "材積
          'RMARK' TEXT-H24  TEXT-H24  TEXT-H24  ''      ''     ''        'X', "備註
          'ZMARK' TEXT-H25  TEXT-H26  TEXT-H26  ''      ''     ''        ''.  "GOODS QUALITY

  ENDIF.

  IF P_BILL IS NOT INITIAL.
    PERFORM SALV_COLUMN_SETTING USING:
          "Col.   Stext     Mtext     LText     Ref.    Ref.Tp EditMask  NoShow
          'VBELN' ''        ''        ''        ''      ''     '==ALPHA' '',  "Billing
          'SIDAT' ''        ''        ''        ''      ''     ''        '',  "Invoice Date
          'GUINO' TEXT-H04  TEXT-H04  TEXT-H04  ''      ''     ''        '',  "GUI No.
          'GUIDT' TEXT-H27  TEXT-H27  TEXT-H27  ''      ''     ''        '',  "GUI Date
          'NAME1' TEXT-H05  TEXT-H05  TEXT-H05  ''      ''     ''        '',  "VendorName
          'NAME2' TEXT-H28  TEXT-H28  TEXT-H28  ''      ''     ''        '',  "VendorSite
          'WAERK' ''        ''        ''        ''      ''     ''        '',  "Currency
          'KURRF' ''        ''        ''        ''      ''     ''        '',  "Exchange Rate
          'PWAER' TEXT-H29  TEXT-H30  TEXT-H30  ''      ''     ''        '',  "Pay Currency
          'WRBTR' TEXT-H31  TEXT-H32  TEXT-H32  'WAERK' 'C'    ''        '',  "Invoice Total
          'TAXID' TEXT-H33  TEXT-H33  TEXT-H33  ''      ''     ''        '',  "Tax Type
          'WMWST' TEXT-H34  TEXT-H34  TEXT-H34  'WAERK' 'C'    ''        '',  "Tax Amt
          'TOTAL' TEXT-H35  TEXT-H35  TEXT-H35  'WAERK' 'C'    ''        '',  "Total Amt
          'VGBEL' ''        ''        ''        ''      ''     '==ALPHA' '',  "Delivery
          'VGPOS' ''        ''        ''        ''      ''     ''        '',  "Delivery Item
          'KDMAT' ''        ''        ''        ''      ''     ''        '',  "Cust. Material Number
          'BSTKD' ''        ''        ''        ''      ''     ''        '',  "Customer purchase order number
          'WORKS' TEXT-H36  TEXT-H36  TEXT-H36  ''      ''     ''        'X', "站點
          'BINPT' TEXT-H37  TEXT-H37  TEXT-H37  ''      ''     ''        'X', "BIN別
          'SUWKS' TEXT-H38  TEXT-H38  TEXT-H38  ''      ''     ''        'X', "子站點
          'CHARG' ''        ''        ''        ''      ''     ''        '',  "Batch Number
          'OCHAR' TEXT-H06  TEXT-H07  TEXT-H07  ''      ''     ''        '',  "Ex LotNo
          'LFIMG' ''        ''        ''        'MEINS' 'Q'    ''        '',  "Qty
          'MEINS' ''        ''        ''        ''      ''     ''        'X', "Unit
          'SEQTY' TEXT-H10  TEXT-H11  TEXT-H11  ''      ''     ''        '',  "Ship Qty Each
          'NETPR' TEXT-H39  TEXT-H39  TEXT-H39  'WAERK' 'C'    ''        '',  "UnitPrice
          'NETWR' TEXT-H40  TEXT-H41  TEXT-H41  'WAERK' 'C'    ''        '',  "Invoice Subtotal
          'GNETW' TEXT-H42  TEXT-H43  TEXT-H43  'WAERK' 'C'    ''        '',  "GUI Subtotal
          'KURSK' TEXT-H44  TEXT-H45  TEXT-H45  ''      ''     ''        ''.  "GUI ExchangeRate

  ENDIF.
  PCL_COLUMNS = SALV_TB->get_columns( ).
  "自動調整寬度
  PCL_COLUMNS->set_optimize( abap_true ).
ENDFORM.                    " SALV_COLUMNS
*&---------------------------------------------------------------------*
*&      Form  SALV_COLUMN_SETTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4999   text
*      -->P_TEXT_H04  text
*      -->P_TEXT_H04  text
*      -->P_TEXT_H04  text
*      -->P_5003   text
*      -->P_5004   text
*      -->P_5005   text
*      -->P_5006   text
*      -->P_PFV_RTYPE_I  text
*----------------------------------------------------------------------*
FORM SALV_COLUMN_SETTING  USING   PFV_FNAME_I         "Col. Name
                                  PFV_STTXT_I         "短說明
                                  PFV_MDTXT_I         "中說明
                                  PFV_LOTXT_I         "長說明
                                  PFV_REFCL_I         "Ref. Column
                                  PFV_REFTP_I         "Ref. Type(C=Currency, Q=Unit)/Col. Type
                                  PFV_EMASK_I         "EDIT MASK
                                  PFV_NOSHW_I.        "NoShow Column('X'=No Show).
  DATA: PFV_STEXT     TYPE SCRTEXT_S,
        PFV_MTEXT     TYPE SCRTEXT_M,
        PFV_LTEXT     TYPE SCRTEXT_L,
        PCL_COLUMNS   TYPE REF TO cl_salv_columns,
        PCLWA_COLUMN  TYPE REF TO cl_salv_column.

  CLEAR: PFV_STEXT, PFV_MTEXT, PFV_LTEXT.
  PFV_STEXT = PFV_STTXT_I.
  PFV_MTEXT = PFV_MDTXT_I.
  PFV_LTEXT = PFV_LOTXT_I.

  PCL_COLUMNS = SALV_TB->get_columns( ).


  "SET_ALIGNMENT =>對齊方式

  try.
      PCLWA_COLUMN = PCL_COLUMNS->get_column( PFV_FNAME_I ).

      IF PFV_NOSHW_I IS NOT INITIAL.
        PCLWA_COLUMN->set_visible( if_salv_c_bool_sap=>false  ).
      ENDIF.
      IF PFV_STEXT IS NOT INITIAL.
        PCLWA_COLUMN->set_short_text( PFV_STEXT ).
      ENDIF.
      IF PFV_MTEXT IS NOT INITIAL.
        PCLWA_COLUMN->set_medium_text( PFV_MTEXT ).
      ENDIF.
      IF PFV_LTEXT IS NOT INITIAL.
        PCLWA_COLUMN->set_long_text( PFV_LTEXT ).
        PCLWA_COLUMN->set_tooltip( PFV_LTEXT ).
      ENDIF.
      PCLWA_COLUMN->set_edit_mask( PFV_EMASK_I ).
      "Ref欄位設定
      IF PFV_REFTP_I = 'C'.
        PCLWA_COLUMN->set_currency_column( PFV_REFCL_I ).
      ENDIF.
      IF PFV_REFTP_I = 'Q'.
        PCLWA_COLUMN->set_quantity_column( PFV_REFCL_I ).
      ENDIF.
*      PCLWA_COLUMN->set_technical( if_salv_c_bool_sap=>true ).
*      IF PFV_FNAME_I = 'ICONS'.
*        PCLWA_COLUMN->set_alignment( if_salv_c_alignment=>centered ).
*      ENDIF.

    catch cx_salv_not_found           "#EC NO_HANDLER
          cx_salv_data_error.
  endtry.
ENDFORM.                    " SALV_COLUMN_SETTING
*&---------------------------------------------------------------------*
*&      Form  SAMPLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0637   text
*----------------------------------------------------------------------*
FORM SAMPLE_DATA  USING PFV_ZTYPE_I.
  CHECK SY-SYSID = 'DEV'.

  IF PFV_ZTYPE_I = 'HEAD'.
    IF P_POPL IS NOT INITIAL.
      I_HEAD-VKORG = 'MAX1'.
      I_HEAD-KUNAG = '0000002520'.
      I_HEAD-KUNNR = '0001000928'.
      I_HEAD-VBELN = '0080250108'.
      I_HEAD-VBTYP = 'LF'.
      I_HEAD-WADAT = '20190305'.
      I_HEAD-FNCTN = 'P'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0080250143'.
      I_HEAD-WADAT = '20190306'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0080250399'.
      I_HEAD-WADAT = '20190313'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0080250440'.
      I_HEAD-WADAT = '20190314'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0080250441'.
      I_HEAD-WADAT = '20190314'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0080250762'.
      I_HEAD-WADAT = '20190325'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0080250864'.
      I_HEAD-WADAT = '20190327'.
      APPEND I_HEAD.
    ENDIF.

    IF P_BILL IS NOT INITIAL.
      I_HEAD-VKORG = 'MAX1'.
      I_HEAD-KUNAG = '0000002520'.
      I_HEAD-VBELN = '0090220315'.
      I_HEAD-VBTYP = 'F2'.
      I_HEAD-WADAT = '20190305'.
      I_HEAD-FNCTN = 'B'.
      I_HEAD-KNUMV = '0000002101'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0090220334'.
      I_HEAD-WADAT = '20190306'.
      I_HEAD-KNUMV = '0000002135'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0090220572'.
      I_HEAD-WADAT = '20190313'.
      I_HEAD-KNUMV = '0000002472'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0090220611'.
      I_HEAD-WADAT = '20190314'.
      I_HEAD-KNUMV = '0000002535'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0090220612'.
      I_HEAD-WADAT = '20190314'.
      I_HEAD-KNUMV = '0000002536'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0090220870'.
      I_HEAD-WADAT = '20190325'.
      I_HEAD-KNUMV = '0000002950'.
      APPEND I_HEAD.
      I_HEAD-VBELN = '0090220951'.
      I_HEAD-WADAT = '20190327'.
      I_HEAD-KNUMV = '0000003069'.
      APPEND I_HEAD.
    ENDIF.
  ENDIF.

ENDFORM.                    " SAMPLE_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SEND_FLAG_FOR_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SEND_FLAG_FOR_HEAD .
  DATA: PFWA_HEAD TYPE STR_HEAD.

  PFWA_HEAD-SFLAG = 'X'.
  MODIFY I_HEAD FROM PFWA_HEAD TRANSPORTING SFLAG
    WHERE SFLAG IS INITIAL
    AND   SELEC IS NOT INITIAL.
ENDFORM.                    " MODIFY_SEND_FLAG_FOR_HEAD
