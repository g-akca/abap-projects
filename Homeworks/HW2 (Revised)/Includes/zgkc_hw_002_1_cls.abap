*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_002_1_CLS
*&---------------------------------------------------------------------*

CLASS school_application DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: app TYPE REF TO school_application.

    CLASS-METHODS:
      instance_app
        RETURNING
          VALUE(ro_app) TYPE REF TO school_application.

    METHODS:
      initialization,
      at_selection_screen_output,
      start_of_selection,
      calculate_avg
        RETURNING VALUE(avg) TYPE i.

ENDCLASS.

CLASS school_application IMPLEMENTATION.
  METHOD instance_app.
    FREE: ro_app.
    IF school_application=>app IS NOT BOUND.
      CREATE OBJECT school_application=>app.
    ENDIF.
    ro_app = app.
  ENDMETHOD.

  METHOD initialization.
  ENDMETHOD.

  METHOD at_selection_screen_output.
  ENDMETHOD.

  METHOD start_of_selection.
    IF p_mt1 > 100 OR p_mt1 < 0 OR p_mt2 > 100 OR p_mt2 < 0 OR p_final > 100 OR p_final < 0.
      MESSAGE 'Lütfen notları 0-100 aralığında girin.' TYPE 'I' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

    DATA(lv_avg) = calculate_avg( ).

    IF p_final >= 50 AND lv_avg >= 50.
      WRITE: 'Öğrenci dönemi başarıyla geçti.', /.
    ELSE.
      WRITE: 'Öğrenci kaldı. Dönemi tekrar edecek.', /.
    ENDIF.

    WRITE: 'No: ', p_ogrno,
           'Adı: ', p_name,
           'Soyadı: ', p_sname,
           '1. Vize: ', p_mt1,
           '2. Vize: ', p_mt2,
           'Final: ', p_final,
           'Ortalama: ', lv_avg.
  ENDMETHOD.

  METHOD calculate_avg.
    avg = p_mt1 * 30 / 100 + p_mt2 * 30 / 100 + p_final * 40 / 100.
  ENDMETHOD.
ENDCLASS.