*&---------------------------------------------------------------------*
*& Report ZGKC_HW_004
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgkc_hw_004.

INCLUDE zgkc_hw_004_topdat.
INCLUDE zgkc_hw_004_frm.
INCLUDE zgkc_hw_004_mod.

INITIALIZATION.
  gv_rapor_created = abap_false.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM create_alv.
