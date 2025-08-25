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
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZGKC_APPROVER_T'.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.
  ENDCASE.
ENDMODULE.