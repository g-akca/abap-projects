*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_004_MOD
*&---------------------------------------------------------------------*

" Kitap Teslim Ver
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.

  SELECT SINGLE nrlevel
  FROM nriv
  WHERE object = 'ZGKC_ISLEM'
  INTO @gv_islemno1.

  gv_islemno1 = gv_islemno1 + 1.

  IF gv_ogrno1 IS NOT INITIAL.
    SELECT SINGLE puan
    FROM zgkc_ogrenci_t
    INTO @gv_puan1
    WHERE ogrno EQ @gv_ogrno1.

    gv_puan_text = |{ gv_puan1 } kütüphane puanı var.|.
  ENDIF.
ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE'.
      IF gv_ogrno1 = ' ' OR gv_kitapno1 = ' ' OR gv_atarih1 = ' '.
        MESSAGE 'Lütfen öğrenci no, kitap no, ve alış tarihi alanlarını doldurunuz.' TYPE 'I' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      DATA: lv_bcount     TYPE int1,
            lv_spoint     TYPE int4,
            lv_checkogr   TYPE zgkc_ogrno_de,
            lv_checkkitap TYPE zgkc_kitapno_de,
            lv_checkislem TYPE zgkc_islemno_de.

      SELECT SINGLE COUNT(*)
      FROM zgkc_ogrenci_t
      WHERE ogrno = @gv_ogrno1
      INTO @lv_checkogr.

      SELECT SINGLE COUNT(*)
      FROM zgkc_kitap_t
      WHERE kitapno = @gv_kitapno1
      INTO @lv_checkkitap.

      IF lv_checkogr = 0.
        MESSAGE 'Lütfen sistemde mevcut bir öğrenci no giriniz!'  TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF lv_checkkitap = 0.
        MESSAGE 'Lütfen sistemde mevcut bir kitap no giriniz!'  TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      SELECT SINGLE COUNT(*)
      FROM zgkc_islem_t
      WHERE kitapno = @gv_kitapno1 AND ogrno = @gv_ogrno1 AND vtarih > @sy-datum
      INTO @lv_checkislem.

      IF lv_checkislem <> 0.
        MESSAGE 'Bu öğrenci bu kitabı henüz kütüphaneye geri vermedi.'  TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      SELECT SINGLE COUNT(*)
      FROM zgkc_islem_t
      WHERE ogrno = @gv_ogrno1 AND vtarih > @sy-datum
      INTO @lv_bcount.

      SELECT SINGLE puan
      FROM zgkc_ogrenci_t
      WHERE ogrno = @gv_ogrno1
      INTO @lv_spoint.

      IF lv_spoint <= 25 AND lv_bcount = 1.
        MESSAGE 'Bu öğrenci aynı anda sadece 1 kitap alabilir.'  TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF lv_spoint <= 50 AND lv_bcount = 2.
        MESSAGE 'Bu öğrenci aynı anda sadece 2 kitap alabilir.'  TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF lv_spoint <= 75 AND lv_bcount = 3.
        MESSAGE 'Bu öğrenci aynı anda sadece 3 kitap alabilir.'  TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZGKC_ISLEM'
        IMPORTING
          number      = gv_islemno1.

      gs_islem_t-islemno = gv_islemno1.
      gs_islem_t-ogrno = gv_ogrno1.
      gs_islem_t-kitapno = gv_kitapno1.
      gs_islem_t-atarih = gv_atarih1.

      INSERT zgkc_islem_t FROM gs_islem_t.

      MESSAGE 'Kitap başarıyla teslim edildi.' TYPE 'S'.
      CLEAR: gv_islemno1, gv_ogrno1, gv_kitapno1, gv_atarih1, gv_puan_text.
      FREE MEMORY.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

" Kitap Teslim Al
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0100'.

  READ TABLE gt_list INTO gs_list WITH KEY secim = abap_true.
  gv_islemno2 = gs_list-islemno.
  gv_ogrno2 = gs_list-ogrno.
  gv_kitapno2 = gs_list-kitapno.
  gv_vtarih2 = sy-datum.
ENDMODULE.

MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE'.
      UPDATE zgkc_islem_t
      SET vtarih = gv_vtarih2
      WHERE islemno = gv_islemno2.

      MESSAGE 'Kitap başarıyla teslim alındı.' TYPE 'S'.
      CLEAR: gv_islemno2, gv_ogrno2, gv_kitapno2, gv_vtarih2.
      FREE MEMORY.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

" Kitap Güncelle
MODULE status_0300 OUTPUT.
  SET PF-STATUS '0100'.

  READ TABLE gt_list INTO gs_list WITH KEY secim = abap_true.
  gv_islemno3 = gs_list-islemno.
  gv_ogrno3 = gs_list-ogrno.
  gv_kitapno3 = gs_list-kitapno.
  gv_atarih3 = gs_list-atarih.
  gv_vtarih3 = gs_list-vtarih.
ENDMODULE.

MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&SAVE'.
      IF gv_ogrno3 = ' ' OR gv_kitapno3 = ' ' OR gv_atarih3 = ' '.
        MESSAGE 'Lütfen öğrenci no, kitap no, ve alış tarihi alanlarını doldurunuz.' TYPE 'I' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      DATA: lv_checkogr2   TYPE zgkc_ogrno_de,
            lv_checkkitap2 TYPE zgkc_kitapno_de.

      SELECT SINGLE COUNT(*)
      FROM zgkc_ogrenci_t
      WHERE ogrno = @gv_ogrno3
      INTO @lv_checkogr2.

      SELECT SINGLE COUNT(*)
      FROM zgkc_kitap_t
      WHERE kitapno = @gv_kitapno3
      INTO @lv_checkkitap2.

      IF lv_checkogr2 = 0.
        MESSAGE 'Lütfen sistemde mevcut bir öğrenci no giriniz!'  TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ELSEIF lv_checkkitap2 = 0.
        MESSAGE 'Lütfen sistemde mevcut bir kitap no giriniz!'  TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF gv_vtarih3 IS NOT INITIAL AND gv_atarih3 > gv_vtarih3.
        MESSAGE 'Alım tarihi teslim tarihinden sonra olamaz.' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      UPDATE zgkc_islem_t
      SET ogrno = @gv_ogrno3, kitapno = @gv_kitapno3, atarih = @gv_atarih3, vtarih = @gv_vtarih3
      WHERE islemno = @gv_islemno3.

      MESSAGE 'İşlem başarıyla güncellendi.' TYPE 'S'.
      CLEAR: gv_islemno3, gv_ogrno3, gv_kitapno3, gv_atarih3, gv_vtarih3.
      FREE MEMORY.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
