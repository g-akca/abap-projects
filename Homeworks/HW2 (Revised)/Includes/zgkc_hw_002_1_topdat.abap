*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_002_1_TOPDAT
*&---------------------------------------------------------------------*

CLASS school_application DEFINITION DEFERRED.
DATA: mo_application TYPE REF TO school_application.

SELECTION-SCREEN BEGIN OF BLOCK std_data.
  PARAMETERS: p_ogrno TYPE char10 OBLIGATORY,
              p_name TYPE string OBLIGATORY,
              p_sname TYPE string OBLIGATORY,
              p_mt1 TYPE p DECIMALS 2 OBLIGATORY,
              p_mt2 TYPE p DECIMALS 2 OBLIGATORY,
              p_final TYPE p DECIMALS 2 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK std_data.