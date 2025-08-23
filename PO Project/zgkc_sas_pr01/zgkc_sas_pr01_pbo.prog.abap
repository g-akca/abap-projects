*&---------------------------------------------------------------------*
*& Include          ZGKC_SAS_PR01_PBO
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
ENDMODULE.

MODULE pbo_0100 OUTPUT.
  lcl_application=>app->alv_session(
    EXCEPTIONS
      contains_error = 1
      OTHERS         = 2  ).
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDMODULE.
