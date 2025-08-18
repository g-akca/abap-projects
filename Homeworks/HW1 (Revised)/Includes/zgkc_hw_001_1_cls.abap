*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_001_1_CLS
*&---------------------------------------------------------------------*

CLASS calc_application DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: app TYPE REF TO calc_application.

    CLASS-METHODS:
      instance_app
        RETURNING
          VALUE(ro_app) TYPE REF TO calc_application.

    METHODS:
      initialization,
      at_selection_screen_output,
      start_of_selection,
      calc_sq_area,
      calc_sq_perim,
      calc_rc_area,
      calc_rc_perim,
      calc_tri_area,
      calc_tri_perim.

ENDCLASS.

CLASS calc_application IMPLEMENTATION.
  METHOD instance_app.
    FREE: ro_app.
    IF calc_application=>app IS NOT BOUND.
      CREATE OBJECT calc_application=>app.
    ENDIF.
    ro_app = app.
  ENDMETHOD.

  METHOD initialization.
  ENDMETHOD.

  METHOD at_selection_screen_output.
    LOOP AT SCREEN.
      IF p_square EQ abap_true AND gv_last_checked <> 'square'.
        p_rect = abap_false.
        p_tri = abap_false.
        gv_last_checked = 'square'.
      ELSEIF p_rect EQ abap_true AND gv_last_checked <> 'rect'.
        p_square = abap_false.
        p_tri = abap_false.
        gv_last_checked = 'rect'.
      ELSEIF p_tri EQ abap_true  AND gv_last_checked <> 'tri'.
        p_rect = abap_false.
        p_square = abap_false.
        gv_last_checked = 'tri'.
      ENDIF.

      IF screen-group1 EQ 'GR1' OR screen-group1 EQ 'GR2' OR screen-group1 EQ 'GR3'.
        screen-active = 0.
        IF p_square EQ abap_true AND screen-group1 EQ 'GR1'.
          screen-active = 1.
        ELSEIF p_rect EQ abap_true AND screen-group1 EQ 'GR2'.
          screen-active = 1.
        ELSEIF p_tri EQ abap_true AND screen-group1 EQ 'GR3'.
          screen-active = 1.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD start_of_selection.
    IF p_square EQ abap_true AND p_area EQ abap_true.
      app->calc_sq_area( ).
    ELSEIF p_square EQ abap_true AND p_perim EQ abap_true.
      app->calc_sq_perim( ).
    ELSEIF p_rect EQ abap_true AND p_area EQ abap_true.
      app->calc_rc_area( ).
    ELSEIF p_rect EQ abap_true AND p_perim EQ abap_true.
      app->calc_rc_perim( ).
    ELSEIF p_tri EQ abap_true AND p_area EQ abap_true.
      app->calc_tri_area( ).
    ELSEIF p_tri EQ abap_true AND p_perim EQ abap_true.
      app->calc_tri_perim( ).
    ELSE.
      WRITE 'Lütfen bir şekil seçin.'.
    ENDIF.
  ENDMETHOD.

  METHOD calc_sq_area.
    gv_result = p_sq_sd * p_sq_sd.
    WRITE: 'Kare Alanı: ', gv_result.
  ENDMETHOD.

  METHOD calc_sq_perim.
    gv_result = p_sq_sd * 4.
    WRITE: 'Kare Çevresi: ', gv_result.
  ENDMETHOD.

  METHOD calc_rc_area.
    gv_result = p_rc_sd1 * p_rc_sd2.
    WRITE: 'Dikdörtgen Alanı: ', gv_result.
  ENDMETHOD.

  METHOD calc_rc_perim.
    gv_result = ( p_rc_sd1 + p_rc_sd2 ) * 2.
    WRITE: 'Dikdörtgen Çevresi: ', gv_result.
  ENDMETHOD.

  METHOD calc_tri_area.
    gv_result = p_base * p_height / 2.
    WRITE: 'Üçgen Alanı: ', gv_result.
  ENDMETHOD.

   METHOD calc_tri_perim.
    gv_result = sqrt( p_height ** 2 + p_base ** 2 ) + p_height + p_base.
    WRITE: 'Üçgen Çevresi: ', gv_result.
  ENDMETHOD.
ENDCLASS.