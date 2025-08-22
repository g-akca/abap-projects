*&---------------------------------------------------------------------*
*& Report ZGKC_SAS_PR01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_SAS_PR01.

INCLUDE ZGKC_SAS_PR01_TOPDAT.
INCLUDE ZGKC_SAS_PR01_CLS.
INCLUDE ZGKC_SAS_PR01_PBO.
INCLUDE ZGKC_SAS_PR01_PAI.

LOAD-OF-PROGRAM.
  mo_application = lcl_application=>instance_app( ).

INITIALIZATION.
  mo_application->initialization( ).

AT SELECTION-SCREEN.
  mo_application->at_selection_screen( ).

START-OF-SELECTION.
  mo_application->start_of_selection( ).
