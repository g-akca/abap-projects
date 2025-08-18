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

    TYPE-POOLS: vrm.

    DATA: it_values TYPE vrm_values,
          wa_values TYPE vrm_value,
          g_id      TYPE vrm_id.

    DATA: gv_last_checked TYPE string,
          gv_result       TYPE p DECIMALS 2.

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
    wa_values-key = 'S'.
    wa_values-text = 'KARE'.
    APPEND wa_values TO it_values.
    CLEAR wa_values.
    wa_values-key = 'R'.
    wa_values-text = 'DİKDORTGEN'.
    APPEND wa_values TO it_values.
    CLEAR wa_values.
    wa_values-key = 'T'.
    wa_values-text = 'ÜÇGEN'.
    APPEND wa_values TO it_values.
    CLEAR wa_values.
    g_id = 'p_shape'.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = g_id
        values = it_values.
  ENDMETHOD.

  METHOD at_selection_screen_output.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GR1' OR screen-group1 EQ 'GR2' OR screen-group1 EQ 'GR3'.
        screen-active = 0.
        IF p_shape = 'S' AND screen-group1 EQ 'GR1'.
          screen-active = 1.
        ELSEIF p_shape = 'R' AND screen-group1 EQ 'GR2'.
          screen-active = 1.
        ELSEIF p_shape = 'T' AND screen-group1 EQ 'GR3'.
          screen-active = 1.
        ENDIF.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD start_of_selection.
    IF p_shape = 'S' AND p_area EQ abap_true.
      app->calc_sq_area( ).
    ELSEIF p_shape = 'S' AND p_perim EQ abap_true.
      app->calc_sq_perim( ).
    ELSEIF p_shape = 'R' AND p_area EQ abap_true.
      app->calc_rc_area( ).
    ELSEIF p_shape = 'R' AND p_perim EQ abap_true.
      app->calc_rc_perim( ).
    ELSEIF p_shape = 'T' AND p_area EQ abap_true.
      app->calc_tri_area( ).
    ELSEIF p_shape = 'T' AND p_perim EQ abap_true.
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