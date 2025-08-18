*&---------------------------------------------------------------------*
*& Report ZGKC_HW_002_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_HW_002_1.

INCLUDE ZGKC_HW_002_1_TOPDAT.
INCLUDE ZGKC_HW_002_1_CLS.

LOAD-OF-PROGRAM.
  mo_application = school_application=>instance_app( ).

INITIALIZATION.
  mo_application->initialization( ).

AT SELECTION-SCREEN OUTPUT.
  mo_application->at_selection_screen_output( ).

START-OF-SELECTION.
  mo_application->start_of_selection( ).