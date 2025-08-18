*&---------------------------------------------------------------------*
*& Report ZGKC_TEST_003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_TEST_003.

DATA gv_age TYPE int2.

PARAMETERS: p_dec TYPE p DECIMALS 3,
            p_int TYPE int4,
            p_str TYPE string.

"idk how this works
SELECT-OPTIONS s_age FOR gv_age.

SELECTION-SCREEN BEGIN OF BLOCK selects WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_rem AS CHECKBOX,
              p_test1 RADIOBUTTON GROUP r1,
              p_test2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK selects.

SELECTION-SCREEN BEGIN OF BLOCK selects2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_test3 RADIOBUTTON GROUP r2,
              p_test4 RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF BLOCK selects2.

START-OF-SELECTION.
  WRITE: 'İlk girilen ondalık sayı (P_DEC): ', p_dec.
