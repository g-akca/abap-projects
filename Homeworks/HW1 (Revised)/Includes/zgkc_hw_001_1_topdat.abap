*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_001_1_TOPDAT
*&---------------------------------------------------------------------*

CLASS calc_application DEFINITION DEFERRED.
DATA: mo_application TYPE REF TO calc_application.

SELECTION-SCREEN BEGIN OF BLOCK shape WITH FRAME TITLE TEXT-001.
PARAMETERS: p_shape AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND usr.
SELECTION-SCREEN END OF BLOCK shape.

SELECTION-SCREEN BEGIN OF BLOCK calc WITH FRAME TITLE TEXT-002.
PARAMETERS: p_area  RADIOBUTTON GROUP r1,
            p_perim RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK calc.

SELECTION-SCREEN BEGIN OF BLOCK var WITH FRAME TITLE TEXT-003.
PARAMETERS: p_sq_sd  TYPE p DECIMALS 2 MODIF ID gr1,
            p_rc_sd1 TYPE p DECIMALS 2 MODIF ID gr2,
            p_rc_sd2 TYPE p DECIMALS 2 MODIF ID gr2,
            p_base   TYPE p DECIMALS 2 MODIF ID gr3,
            p_height TYPE p DECIMALS 2 MODIF ID gr3.
SELECTION-SCREEN END OF BLOCK var.