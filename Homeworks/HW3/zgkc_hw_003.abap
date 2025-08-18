*&---------------------------------------------------------------------*
*& Report ZGKC_HW_003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgkc_hw_003.

INCLUDE zgkc_hw_003_top.
INCLUDE zgkc_hw_003_frm.

INITIALIZATION.
  p_but = 'Oluştur'.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 = 'SNO' OR screen-group1 = 'PNO' OR screen-group1 = 'BNO'.
      screen-input = 0.
    ENDIF.

    IF screen-group1 = 'STD' OR screen-group1 = 'PRC'
    OR screen-group1 = 'BO' OR screen-group1 = 'SNO'
    OR screen-group1 = 'PNO' OR screen-group1 = 'BNO'.
      screen-active = 0.
      IF p_std = abap_true AND ( screen-group1 = 'STD' OR screen-group1 = 'SNO' ).
        screen-active = 1.

        SELECT SINGLE nrlevel
        FROM nriv
        WHERE object = 'ZGKC_OGR'
        INTO @gv_ogrno.

        p_sno = gv_ogrno.
      ELSEIF p_prc = abap_true AND ( screen-group1 = 'PRC' OR screen-group1 = 'PNO' ).
        screen-active = 1.

        SELECT SINGLE nrlevel
        FROM nriv
        WHERE object = 'ZGKC_ISLEM'
        INTO @gv_islemno.

        p_pno = gv_islemno.
      ELSEIF p_book = abap_true AND ( screen-group1 = 'BO' OR screen-group1 = 'BNO' ).
        screen-active = 1.

        SELECT SINGLE nrlevel
        FROM nriv
        WHERE object = 'ZGKC_KITAP'
        INTO @gv_kitapno.

        p_bno = gv_kitapno.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'BUT'.
    sscrfields-ucomm = 'ONLI'.
  ENDIF.

START-OF-SELECTION.
  IF p_std = abap_true.
    PERFORM insert_student.
  ELSEIF p_prc = abap_true.
    PERFORM insert_process.
  ELSEIF p_book = abap_true.
    PERFORM insert_book.
  ENDIF.
