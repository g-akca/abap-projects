*&---------------------------------------------------------------------*
*& Include          ZGKC_TEST_006_PAI
*&---------------------------------------------------------------------*

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

MODULE validate_ogrno INPUT.
  SELECT SINGLE ogrno
  FROM zgkc_ogrenci_t
  WHERE ogrno = @gv_ogrno
  INTO @DATA(lv_ogrno).
  IF sy-subrc <> 0.
    MESSAGE 'Öğrenci no bulunamadı.' TYPE 'E'.
  ENDIF.
ENDMODULE.

MODULE validate_kitapno INPUT.
  SELECT SINGLE kitapno
  FROM zgkc_kitap_t
  WHERE kitapno = @gv_kitapno
  INTO @DATA(lv_kitapno).
  IF sy-subrc <> 0.
    MESSAGE 'Kitap no bulunamadı.' TYPE 'E'.
  ENDIF.
ENDMODULE.

MODULE validate_input INPUT.
  SELECT SINGLE *
  FROM zgkc_islem_t
  WHERE ogrno = @gv_ogrno AND kitapno = @gv_kitapno
  INTO @gs_islem_t.
  IF sy-subrc = 0.
    gv_islem_text = |Bir işlem bulundu. No: { gs_islem_t-islemno }|.
  ELSE.
    gv_islem_text = ''. "silmiyor
    MESSAGE 'İşlem bulunamadı.' TYPE 'E'.
  ENDIF.
ENDMODULE.
