*&---------------------------------------------------------------------*
*& Report ZGKC_HW_005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_HW_005.

INCLUDE ZGKC_HW_005_TOPDAT.
INCLUDE ZGKC_HW_005_CLS.
INCLUDE ZGKC_HW_005_PBO.
INCLUDE ZGKC_HW_005_PAI.

LOAD-OF-PROGRAM.
  mo_application = lcl_application=>instance_app( ).

INITIALIZATION.
  mo_application->initialization( ).

AT SELECTION-SCREEN.
  mo_application->at_selection_screen( ).

START-OF-SELECTION.
  mo_application->start_of_selection( ).
