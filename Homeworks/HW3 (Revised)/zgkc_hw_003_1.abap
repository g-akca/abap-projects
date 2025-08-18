*&---------------------------------------------------------------------*
*& Report ZGKC_HW_003_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_HW_003_1.

INCLUDE ZGKC_HW_003_1_TOPDAT.
INCLUDE ZGKC_HW_003_1_CLS.

LOAD-OF-PROGRAM.
  mo_application = library_application=>instance_app( ).

INITIALIZATION.
  mo_application->initialization( ).

AT SELECTION-SCREEN OUTPUT.
  mo_application->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  mo_application->at_selection_screen( ).

START-OF-SELECTION.
  mo_application->start_of_selection( ).