*&---------------------------------------------------------------------*
*& Report ZGKC_TEST_004
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgkc_test_004.

DATA gv_loc TYPE i.

INCLUDE zgkc_test_004_top.
INCLUDE zgkc_test_004_frm.

INITIALIZATION.
  gv_inc = 10.
  gv_inc2 = 50.
  gv_loc = 5.

START-OF-SELECTION.
  WRITE: 'Included global variables: ', gv_inc, gv_inc2, /,
         'Global variable: ', gv_loc, /.
  PERFORM include_method USING gv_inc gv_inc2.
  PERFORM local_method.

END-OF-SELECTION.
  WRITE 'Testing 2 in END-OF-SELECTION, should appear right after Testing.'.

FORM local_method.
  WRITE 'Testing in START-OF-SELECTION, written by the local method.'.
ENDFORM.
