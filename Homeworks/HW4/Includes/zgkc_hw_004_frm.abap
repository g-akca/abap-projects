*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_004_FRM
*&---------------------------------------------------------------------*

" Main ALV Formları
FORM create_alv.
  PERFORM get_data.
  PERFORM create_fieldcatalog.
  PERFORM create_layout.

  gs_event-name = slis_ev_pf_status_set.
  gs_event-form = 'PF_STATUS_SET'.
  APPEND gs_event TO gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcatalog
      it_events                = gt_events
    TABLES
      t_outtab                 = gt_list.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.

FORM get_data.
  SELECT i~islemno, o~ogrno, o~ograd, o~ogrsoyad, k~kitapno, k~kitapad,
         y~yazarno, y~yazarad, y~yazarsoyad, t~turno, t~turad, i~atarih, i~vtarih
  FROM zgkc_islem_t AS i
  JOIN zgkc_ogrenci_t AS o ON i~ogrno = o~ogrno
  JOIN zgkc_kitap_t AS k ON i~kitapno = k~kitapno
  JOIN zgkc_yazar_t AS y ON y~yazarno = k~yazarno
  JOIN zgkc_tur_t AS t ON t~turno = k~turno
  WHERE o~ogrno IN @s_ogrno AND k~kitapno IN @s_ktpno
  ORDER BY i~islemno
  INTO CORRESPONDING FIELDS OF TABLE @gt_list.

  LOOP AT gt_list ASSIGNING FIELD-SYMBOL(<gs_list>).
    <gs_list>-ogradsoyad = |{ <gs_list>-ograd } { <gs_list>-ogrsoyad }|.
    <gs_list>-yazaradsoyad = |{ <gs_list>-yazarad } { <gs_list>-yazarsoyad }|.
  ENDLOOP.

  LOOP AT gt_list INTO gs_list.
    IF gs_list-vtarih IS INITIAL.
      IF sy-datum - gs_list-atarih >= 15.
        gs_list-color = 'C611'.
        gs_list-istisna = '@0A@'.
      ELSE.
        gs_list-istisna = '@09@'.
      ENDIF.
    ELSE.
      gs_list-istisna = '@08@'.
    ENDIF.
    MODIFY gt_list FROM gs_list.
  ENDLOOP.
  CLEAR gs_list.
ENDFORM.

FORM create_fieldcatalog.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = sy-repid
      i_internal_tabname = 'gt_list'
      i_structure_name   = 'ZGKC_HW_004_S'
      i_inclname         = sy-repid
    CHANGING
      ct_fieldcat        = gt_fieldcatalog.

  LOOP AT gt_fieldcatalog INTO gs_fieldcatalog.
    DATA _text TYPE string.
    CASE gs_fieldcatalog-fieldname.
      WHEN 'ISTISNA'.
        _text = 'İstisna'.
      WHEN 'ISLEMNO'.
        _text = 'İşlem No'.
      WHEN 'OGRNO'.
        _text = 'Öğrenci No'.
      WHEN 'OGRADSOYAD'.
        _text = 'Ad Soyad'.
      WHEN 'KITAPNO'.
        _text = 'Kitap No'.
        gs_fieldcatalog-emphasize = 'C310'.
      WHEN 'KITAPAD'.
        _text = 'Kitap Adı'.
        gs_fieldcatalog-emphasize = 'C300'.
      WHEN 'YAZARNO'.
        _text = 'Yazar No'.
        gs_fieldcatalog-emphasize = 'C310'.
      WHEN 'YAZARADSOYAD'.
        _text = 'Yazar Adı'.
        gs_fieldcatalog-emphasize = 'C300'.
      WHEN 'TURNO'.
        _text = 'Tür No'.
        gs_fieldcatalog-emphasize = 'C310'.
      WHEN 'TURAD'.
        _text = 'Tür Tanımı'.
        gs_fieldcatalog-emphasize = 'C300'.
      WHEN 'ATARIH'.
        _text = 'Kitap Alım'.
      WHEN 'VTARIH'.
        _text = 'Kitap Teslim'.
    ENDCASE.

    gs_fieldcatalog-seltext_s    = _text.
    gs_fieldcatalog-seltext_m    = _text.
    gs_fieldcatalog-seltext_l    = _text.
    gs_fieldcatalog-reptext_ddic = _text.
    CLEAR _text.
    MODIFY gt_fieldcatalog FROM gs_fieldcatalog.
  ENDLOOP.
ENDFORM.

FORM create_layout.
  gs_layout-window_titlebar = 'İşlem Structure'.
  gs_layout-zebra = abap_true.
  gs_layout-box_fieldname = 'SECIM'.
  gs_layout-colwidth_optimize = abap_true.
  gs_layout-info_fieldname = 'COLOR'.
ENDFORM.

FORM pf_status_set USING p_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.

FORM user_command USING p_ucomm TYPE sy-ucomm
                        ps_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN '&TVER'.
      CALL SCREEN 0100 STARTING AT 60 10 ENDING AT 120 30.
      PERFORM get_data.

    WHEN '&TAL'.
      READ TABLE gt_list INTO gs_list WITH KEY secim = abap_true.
      IF gs_list IS INITIAL.
        MESSAGE 'Lütfen bir işlem seçin.' TYPE 'I'.
        EXIT.
      ELSEIF gs_list-vtarih IS NOT INITIAL.
        MESSAGE 'Bu işlemin teslim tarihi mevcut.' TYPE 'I'.
        EXIT.
      ENDIF.

      CALL SCREEN 0200 STARTING AT 60 10 ENDING AT 120 30.

    WHEN '&GNC'.
      READ TABLE gt_list INTO gs_list WITH KEY secim = abap_true.
      IF gs_list IS INITIAL.
        MESSAGE 'Lütfen bir işlem seçin.' TYPE 'I'.
        EXIT.
      ELSEIF gs_list-vtarih IS NOT INITIAL.
        MESSAGE 'Tamamlanan işlemler güncellenemez.' TYPE 'I'.
        EXIT.
      ENDIF.

      CALL SCREEN 0300 STARTING AT 60 10 ENDING AT 120 30.

    WHEN '&SIL'.
      DATA: lv_ans(1).

      READ TABLE gt_list INTO gs_list WITH KEY secim = abap_true.
      IF gs_list IS INITIAL.
        MESSAGE 'Lütfen bir işlem seçin.' TYPE 'I'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'İşlem gerçekleştiriliyor...'
          text_question         = 'Emin misiniz?'
          text_button_1         = 'Tamam'
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = 'İptal'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
          popup_type            = 'ICON_MESSAGE_ERROR'
        IMPORTING
          answer                = lv_ans.

      IF lv_ans EQ '1'.
        DELETE FROM zgkc_islem_t WHERE islemno = @gs_list-islemno.
        DELETE gt_list WHERE islemno = gs_list-islemno.
        MESSAGE 'İşlem başarıyla silindi.' TYPE 'I'.
      ELSEIF lv_ans EQ '2'.
        MESSAGE 'İşlem kullanıcı tarafından iptal edildi.' TYPE 'I'.
      ENDIF.

    WHEN '&RAP'.
      READ TABLE gt_list INTO gs_list WITH KEY secim = abap_true.
      IF gs_list IS INITIAL.
        MESSAGE 'Lütfen bir işlem seçin.' TYPE 'I'.
        EXIT.
      ENDIF.

      PERFORM create_rapor USING gs_list-ogrno.
  ENDCASE.
  ps_selfield-refresh = 'X'.
ENDFORM.

" Rapor Formları
FORM create_rapor USING current_ogrno.
  CLEAR gt_rapor.

  LOOP AT gt_list ASSIGNING FIELD-SYMBOL(<gs_list>) WHERE ogrno = current_ogrno.
    gs_rapor-ogrno = <gs_list>-ogrno.
    gs_rapor-ogradsoyad = <gs_list>-ogradsoyad.
    IF <gs_list>-istisna = '@0A@'.
      gs_rapor-geciken = gs_rapor-geciken + 1.
    ELSEIF <gs_list>-istisna = '@09@'.
      gs_rapor-verilmeyen = gs_rapor-verilmeyen + 1.
    ELSEIF <gs_list>-istisna = '@08@'.
      gs_rapor-teslim = gs_rapor-teslim + 1.
    ENDIF.
    gs_rapor-toplam = gs_rapor-toplam + 1.
  ENDLOOP.

  APPEND gs_rapor TO gt_rapor.

  IF gv_rapor_created = abap_false.
    PERFORM create_rapor_fc.
    PERFORM create_rapor_lo.
    gv_rapor_created = abap_true.
  ENDIF.

  gs_rapor_ev-name = slis_ev_pf_status_set.
  gs_rapor_ev-form = 'PF_STATUS_SET2'.
  APPEND gs_rapor_ev TO gt_rapor_ev.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'PF_STATUS_SET2'
      i_callback_user_command  = 'USER_COMMAND2'
      is_layout                = gs_rapor_lo
      it_fieldcat              = gt_rapor_fc
      it_events                = gt_rapor_ev
      i_screen_start_column    = 20
      i_screen_start_line      = 30
      i_screen_end_column      = 150
      i_screen_end_line        = 50
    TABLES
      t_outtab                 = gt_rapor.
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM create_rapor_fc.
  gs_rapor_fc-fieldname = 'OGRNO'.
  gs_rapor_fc-seltext_m = 'Öğrenci No'.
  APPEND gs_rapor_fc TO gt_rapor_fc.
  CLEAR gs_rapor_fc.
  gs_rapor_fc-fieldname = 'OGRADSOYAD'.
  gs_rapor_fc-seltext_m = 'Ad Soyad'.
  APPEND gs_rapor_fc TO gt_rapor_fc.
  CLEAR gs_rapor_fc.
  gs_rapor_fc-fieldname = 'TOPLAM'.
  gs_rapor_fc-seltext_m = 'Toplam Kitap'.
  APPEND gs_rapor_fc TO gt_rapor_fc.
  CLEAR gs_rapor_fc.
  gs_rapor_fc-fieldname = 'VERILMEYEN'.
  gs_rapor_fc-seltext_m = 'Verilmeyen'.
  APPEND gs_rapor_fc TO gt_rapor_fc.
  CLEAR gs_rapor_fc.
  gs_rapor_fc-fieldname = 'GECIKEN'.
  gs_rapor_fc-seltext_m = 'Geciken Kitap'.
  APPEND gs_rapor_fc TO gt_rapor_fc.
  CLEAR gs_rapor_fc.
  gs_rapor_fc-fieldname = 'TESLIM'.
  gs_rapor_fc-seltext_m = 'Teslim Kitap'.
  APPEND gs_rapor_fc TO gt_rapor_fc.
  CLEAR gs_rapor_fc.
ENDFORM.

FORM create_rapor_lo.
  gs_rapor_lo-window_titlebar = 'Öğrenci Kitap Raporu'.
  gs_rapor_lo-zebra = abap_true.
  gs_rapor_lo-colwidth_optimize = abap_true.
ENDFORM.

FORM pf_status_set2 USING p_extab TYPE slis_t_extab.
  SET PF-STATUS '0200'.
ENDFORM.

FORM user_command2 USING p_ucomm TYPE sy-ucomm
                        ps_selfield TYPE slis_selfield.
  CASE p_ucomm.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN '&ONAY'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.