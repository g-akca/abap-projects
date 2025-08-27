*&---------------------------------------------------------------------*
*& Report ZGKC_SAS_PR02
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_SAS_PR02.

INCLUDE ZGKC_SAS_PR02_TOPDAT.
INCLUDE ZGKC_SAS_PR02_CLS.
INCLUDE ZGKC_SAS_PR02_PBO.
INCLUDE ZGKC_SAS_PR02_PAI.

LOAD-OF-PROGRAM.
  mo_application = lcl_application=>instance_app( ).

INITIALIZATION.
  mo_application->initialization( ).

START-OF-SELECTION.
  mo_application->start_of_selection( ).