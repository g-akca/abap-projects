*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_003_1_CLS
*&---------------------------------------------------------------------*

CLASS library_application DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      app TYPE REF TO library_application.

    CLASS-METHODS:
      instance_app
        RETURNING
          VALUE(ro_app) TYPE REF TO library_application.

    DATA:
      ms_ogrenci_t TYPE zgkc_ogrenci_t,
      ms_islem_t   TYPE zgkc_islem_t,
      ms_kitap_t   TYPE zgkc_kitap_t,
      mv_ogrno     TYPE nriv-nrlevel,
      mv_islemno   TYPE nriv-nrlevel,
      mv_kitapno   TYPE nriv-nrlevel.

    METHODS:
      initialization,
      at_selection_screen_output,
      at_selection_screen,
      start_of_selection,
      insert_student,
      insert_process,
      insert_book.

ENDCLASS.

CLASS library_application IMPLEMENTATION.
  METHOD instance_app.
    FREE: ro_app.
    IF library_application=>app IS NOT BOUND.
      CREATE OBJECT library_application=>app.
    ENDIF.
    ro_app = app.
  ENDMETHOD.

  METHOD initialization.
    p_but = 'Oluştur'.
  ENDMETHOD.

  METHOD at_selection_screen_output.
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
          INTO @mv_ogrno.

          p_sno = mv_ogrno.
        ELSEIF p_prc = abap_true AND ( screen-group1 = 'PRC' OR screen-group1 = 'PNO' ).
          screen-active = 1.

          SELECT SINGLE nrlevel
          FROM nriv
          WHERE object = 'ZGKC_ISLEM'
          INTO @mv_islemno.

          p_pno = mv_islemno.
        ELSEIF p_book = abap_true AND ( screen-group1 = 'BO' OR screen-group1 = 'BNO' ).
          screen-active = 1.

          SELECT SINGLE nrlevel
          FROM nriv
          WHERE object = 'ZGKC_KITAP'
          INTO @mv_kitapno.

          p_bno = mv_kitapno.
        ENDIF.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD at_selection_screen.
    IF sscrfields-ucomm = 'BUT'.
      sscrfields-ucomm = 'ONLI'.
    ENDIF.
  ENDMETHOD.

  METHOD start_of_selection.
    IF p_std = abap_true.
      app->insert_student( ).
    ELSEIF p_prc = abap_true.
      app->insert_process( ).
    ELSEIF p_book = abap_true.
      app->insert_book( ).
    ENDIF.
  ENDMETHOD.

  METHOD insert_student.
    IF p_sname = ' ' OR p_ssname = ' ' OR p_sclass = ' ' OR p_spoint = ' '.
      MESSAGE 'Lütfen ad, soyad, sınıf, ve puan alanlarını doldurunuz.' TYPE 'I' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

    IF p_sgnd <> 'K' AND p_sgnd <> 'E'.
      MESSAGE 'Lütfen cinsiyet alanına K veya E giriniz.' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZGKC_OGR'
      IMPORTING
        number      = p_sno.

    ms_ogrenci_t-ogrno = p_sno.
    ms_ogrenci_t-ograd = p_sname.
    ms_ogrenci_t-ogrsoyad = p_ssname.
    ms_ogrenci_t-cinsiyet = p_sgnd.
    ms_ogrenci_t-dtarih = p_sdt.
    ms_ogrenci_t-sinif = p_sclass.
    ms_ogrenci_t-puan = p_spoint.

    INSERT zgkc_ogrenci_t FROM ms_ogrenci_t.

    MESSAGE 'Öğrenci başarıyla eklendi.' TYPE 'I' DISPLAY LIKE 'S'.
    FREE: p_sno, p_sname, p_ssname, p_sgnd, p_sdt, p_sclass, p_spoint.
  ENDMETHOD.

  METHOD insert_process.
    IF p_psno = ' ' OR p_pbno = ' ' OR p_pat = ' '.
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
    WHERE ogrno = @p_psno
    INTO @lv_checkogr.

    SELECT SINGLE COUNT(*)
    FROM zgkc_kitap_t
    WHERE kitapno = @p_pbno
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
    WHERE kitapno = @p_pbno AND ogrno = @p_psno AND vtarih > @sy-datum
    INTO @lv_checkislem.

    IF lv_checkislem <> 0.
      MESSAGE 'Bu öğrenci bu kitabı henüz kütüphaneye geri vermedi.'  TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    SELECT SINGLE COUNT(*)
    FROM zgkc_islem_t
    WHERE ogrno = @p_psno AND vtarih > @sy-datum
    INTO @lv_bcount.

    SELECT SINGLE puan
    FROM zgkc_ogrenci_t
    WHERE ogrno = @p_psno
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
        number      = p_pno.

    ms_islem_t-islemno = p_pno.
    ms_islem_t-ogrno = p_psno.
    ms_islem_t-kitapno = p_pbno.
    ms_islem_t-atarih = p_pat.
    ms_islem_t-vtarih = p_pvt.

    INSERT zgkc_islem_t FROM ms_islem_t.

    MESSAGE 'İşlem başarıyla eklendi.' TYPE 'I' DISPLAY LIKE 'S'.
    FREE: p_pno, p_psno, p_pbno, p_pat, p_pvt.
  ENDMETHOD.

  METHOD insert_book.
    IF p_bname = ' ' OR p_byno = ' ' OR p_btno = ' '.
      MESSAGE 'Lütfen kitap adı, yazar no, ve tür no alanlarını doldurunuz.' TYPE 'I' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

    DATA: lv_checkyazar TYPE zgkc_yazarno_de,
          lv_checktur   TYPE zgkc_turno_de.

    SELECT SINGLE COUNT(*)
    FROM zgkc_yazar_t
    WHERE yazarno = @p_byno
    INTO @lv_checkyazar.

    SELECT SINGLE COUNT(*)
    FROM zgkc_tur_t
    WHERE turno = @p_btno
    INTO @lv_checktur.

    IF lv_checkyazar = 0.
      MESSAGE 'Lütfen sistemde mevcut bir yazar no giriniz!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ELSEIF lv_checktur = 0.
      MESSAGE 'Lütfen sistemde mevcut bir tür no giriniz!' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZGKC_KITAP'
      IMPORTING
        number      = p_bno.

    ms_kitap_t-kitapno = p_bno.
    ms_kitap_t-kitapad = p_bname.
    ms_kitap_t-yazarno = p_byno.
    ms_kitap_t-turno = p_btno.
    ms_kitap_t-sayfasayisi = p_bpage.

    INSERT zgkc_kitap_t FROM ms_kitap_t.

    MESSAGE 'Kitap başarıyla eklendi.' TYPE 'I' DISPLAY LIKE 'S'.
    FREE: p_bno, p_bname, p_byno, p_btno, p_bpage.
  ENDMETHOD.
ENDCLASS.