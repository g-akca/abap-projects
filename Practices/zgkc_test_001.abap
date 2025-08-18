*&---------------------------------------------------------------------*
*& Report ZGKC_TEST_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_TEST_001.

*Declaration
DATA: gv_test1 TYPE p DECIMALS 3,
      gv_test2 TYPE p DECIMALS 2,
      gv_test3 TYPE int4,
      gv_test4 TYPE n LENGTH 6,
      gv_test5 TYPE c,
      gv_test6 TYPE string,
      gv_result TYPE p DECIMALS 1.

*Assignment
gv_test1 = '1.5'.
gv_test2 = '305.6588'.
gv_test3 = 100.
gv_test4 = 150000.
gv_test5 = 'G'.
gv_test6 = 'just testing'.
gv_result = gv_test1 + gv_test3.

*Output
WRITE: gv_test1,
       / gv_test2,
       / gv_test3,
       / gv_test4,
       / gv_test5,
       / gv_test6, /.

WRITE: / '1. ve 3. sayıların toplamı:', gv_result.
