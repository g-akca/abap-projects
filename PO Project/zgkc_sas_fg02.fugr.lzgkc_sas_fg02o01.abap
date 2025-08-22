*&---------------------------------------------------------------------*
*& Include          LZGKC_SAS_FG02O01
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
ENDMODULE.

MODULE pbo_0100 OUTPUT.
  mo_application = lcl_application=>instance_app( ).
  mo_application->initialization( ).
ENDMODULE.
