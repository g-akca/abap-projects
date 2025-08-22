*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_005_TOPDAT
*&---------------------------------------------------------------------*

TABLES sscrfields.

CLASS lcl_application DEFINITION DEFERRED.
DATA: mo_application TYPE REF TO lcl_application.

SELECTION-SCREEN BEGIN OF BLOCK select WITH FRAME TITLE TEXT-001.
PARAMETERS: p_sk  TYPE bkpf-bukrs OBLIGATORY,
            p_yil TYPE bkpf-gjahr OBLIGATORY.

DATA: gv_donem  TYPE bkpf-monat,
      gv_satici TYPE bseg-lifnr.

SELECT-OPTIONS: s_donem FOR gv_donem,
                s_satici FOR gv_satici.
SELECTION-SCREEN END OF BLOCK select.

SELECTION-SCREEN BEGIN OF BLOCK buttons WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN PUSHBUTTON 10(25) p_view USER-COMMAND but.
SELECTION-SCREEN PUSHBUTTON 40(25) p_upd USER-COMMAND but2.
SELECTION-SCREEN END OF BLOCK buttons.
