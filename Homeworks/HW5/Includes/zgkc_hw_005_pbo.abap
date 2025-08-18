*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_005_PBO
*&---------------------------------------------------------------------*

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STANDARD'.
ENDMODULE.

MODULE PBO_0100 OUTPUT.
  lcl_application=>app->alv_session(
    EXCEPTIONS
      contains_error = 1
      OTHERS         = 2  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDMODULE.

MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS '0200'.
ENDMODULE.

MODULE PBO_0200 OUTPUT.
  lcl_application=>app->display_popup_alv( ).
ENDMODULE.
