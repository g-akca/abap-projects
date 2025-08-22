*&---------------------------------------------------------------------*
*& Include          LZGKC_SAS_FG02I01
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK' OR '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
    WHEN '&POE'.
      CALL TRANSACTION 'ZGKC_PM01'.
    WHEN '&POA'.
      CALL TRANSACTION 'ZGKC_PM03'.
    WHEN '&AA'.
      CALL TRANSACTION 'ZGKC_PM04'.
    WHEN '&UMT'.

  ENDCASE.
ENDMODULE.
