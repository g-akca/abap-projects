*&---------------------------------------------------------------------*
*& Include          ZGKC_SAS_PR01_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK' OR '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
