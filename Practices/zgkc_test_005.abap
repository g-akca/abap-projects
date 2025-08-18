*&---------------------------------------------------------------------*
*& Report ZGKC_TEST_005
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_TEST_005.

SELECTION-SCREEN BEGIN OF BLOCK login WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_lgnm TYPE char12 OBLIGATORY,
              p_lgpw TYPE char16 OBLIGATORY.
  "SELECTION-SCREEN PUSHBUTTON 2(10) but1 USER-COMMAND lgn.
SELECTION-SCREEN END OF BLOCK login.

SELECTION-SCREEN BEGIN OF BLOCK signup WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_sgnm TYPE char12 OBLIGATORY,
              p_sgpw TYPE char16 OBLIGATORY.
  "SELECTION-SCREEN PUSHBUTTON 2(10) but2 USER-COMMAND sgn.
SELECTION-SCREEN END OF BLOCK signup.

INITIALIZATION.
  "but1 = 'Log In'.
  "but2 = 'Sign Up'.

START-OF-SELECTION.
