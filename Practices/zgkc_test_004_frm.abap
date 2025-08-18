*&---------------------------------------------------------------------*
*& Include ZGKC_TEST_004_FRM
*&---------------------------------------------------------------------*

FORM include_method USING i1 i2.
  DATA lv_result TYPE i.
  lv_result = i1 + i2 + gv_loc. "gv_inc and gv_inc2 are given as arguments, while gv_loc is not. For testing purposes.
  WRITE: 'The sum of them, calculated by the included method, stored in a local variable of the method: ', lv_result, /.
ENDFORM.
