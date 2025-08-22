*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_005_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0100.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&DETAY' OR '&PRINT'.
      mo_application->handle_user_command( e_ucomm = sy-ucomm ).
  ENDCASE.
ENDMODULE.

MODULE user_command_0200.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
