*&---------------------------------------------------------------------*
*& Include          LZGKC_SAS_FG01I01
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK' OR '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
    WHEN '&KEY'.
      CALL SCREEN 0200 STARTING AT 80 10 ENDING AT 107 14.
    WHEN '&INSERT'.
      mo_application->add_new_item( ).
    WHEN '&CLEAR'.
      mo_application->clear_screen( ).
    WHEN '&SEND'.
      mo_application->send_to_approval( ).
  ENDCASE.
ENDMODULE.

MODULE validate_material INPUT.
  mo_application->validate_material( ).
ENDMODULE.

MODULE validate_inputs INPUT.
  mo_application->validate_inputs( ).
ENDMODULE.

MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE'.
      mo_application->update_key( ).
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.