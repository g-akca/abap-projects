*&---------------------------------------------------------------------*
*& Include          ZGKC_SAS_PR01_TOPDAT
*&---------------------------------------------------------------------*

CLASS lcl_application DEFINITION DEFERRED.
DATA: mo_application TYPE REF TO lcl_application.

TABLES: SSCRFIELDS.
TYPE-POOLS: ICON.

SELECTION-SCREEN: FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs TYPE bukrs.

DATA: gv_ebeln TYPE ebeln,
      gv_erdat TYPE erdat,
      gv_ernam TYPE ernam.

SELECT-OPTIONS: s_ebeln FOR gv_ebeln,
                s_erdat FOR gv_erdat,
                s_ernam FOR gv_ernam.
SELECTION-SCREEN END OF BLOCK selection.
