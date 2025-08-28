*&---------------------------------------------------------------------*
*& Include          ZGKC_SAS_PR01_TOPDAT
*&---------------------------------------------------------------------*

CLASS lcl_application DEFINITION DEFERRED.
DATA: mo_application TYPE REF TO lcl_application.

SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY.

DATA: gv_ebeln TYPE ebeln,
      gv_erdat TYPE erdat,
      gv_ernam TYPE ernam.

SELECT-OPTIONS: s_ebeln FOR gv_ebeln,
                s_erdat FOR gv_erdat,
                s_ernam FOR gv_ernam.
SELECTION-SCREEN END OF BLOCK selection.

SELECTION-SCREEN BEGIN OF BLOCK selection2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_yellow  AS CHECKBOX DEFAULT 'X',
            p_green AS CHECKBOX,
            p_red    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK selection2.