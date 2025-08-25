*&---------------------------------------------------------------------*
*& Include          LZGKC_SAS_FG01O01
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  FREE mt_exclude.
  IF zgkc_po_item-ebeln IS NOT INITIAL AND zgkc_po_item-ebeln <> '$000000000'.
    APPEND '&KEY' TO mt_exclude.
    APPEND '&INSERT' TO mt_exclude.
    APPEND '&SEND' TO mt_exclude.
  ENDIF.

  SET PF-STATUS '0100' EXCLUDING mt_exclude.
ENDMODULE.

MODULE pbo_0100 OUTPUT.
  mo_application = lcl_application=>instance_app( ).
  mo_application->initialization( ).
  mo_application->after_send( ).
ENDMODULE.

MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
ENDMODULE.