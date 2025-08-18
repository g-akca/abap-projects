*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_005_CLS
*&---------------------------------------------------------------------*

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: app TYPE REF TO lcl_application.

    CLASS-METHODS:
      instance_app
        RETURNING
          VALUE(ro_app) TYPE REF TO lcl_application.

    " OO ALV
    DATA: mo_grid     TYPE REF TO cl_gui_alv_grid,
          mt_fieldcat TYPE lvc_t_fcat.

    " SALV
    DATA: mo_salv     TYPE REF TO cl_salv_table,
          mo_columns TYPE REF TO cl_salv_columns_table,
          mo_cont TYPE REF TO cl_gui_custom_container,
          mo_selections type ref to cl_salv_selections.

    DATA: mt_outdat TYPE TABLE OF zgkc_hw_005_s,
          mt_popdat TYPE TABLE OF zgkc_hw_005_s2,
          mt_sfdat TYPE TABLE OF zgkc_hw_005_s3,
          ms_sfdat TYPE zgkc_hw_005_s3.

    METHODS:
      initialization,
      at_selection_screen,
      start_of_selection,
      retrieve_dat
        IMPORTING
          iv_sk TYPE bkpf-bukrs
          iv_my TYPE bkpf-gjahr
        EXCEPTIONS
          contains_error,
      alv_session
        EXCEPTIONS
          contains_error,
      display_alvdat
        EXCEPTIONS
          contains_error,
      create_fieldcat
        IMPORTING
          !im_strname TYPE tabname
        EXCEPTIONS
          contains_error,
      update_fieldcat
        EXCEPTIONS
          contains_error,
      set_layout_dat
        RETURNING
          VALUE(rv_layoutdat) TYPE lvc_s_layo,
      attach_handlers
        IMPORTING
          VALUE(im_grid) TYPE REF TO cl_gui_alv_grid
        EXCEPTIONS
          contains_error,
      refresh_alv
        EXCEPTIONS
          contains_error,
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm,
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_column_id
          e_row_id,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,
      retrieve_popup_data
        IMPORTING
          iv_sk TYPE bkpf-bukrs
          iv_my TYPE bkpf-gjahr
          iv_bn TYPE bkpf-belnr,
      display_popup_alv,
      retrieve_sf_data
        IMPORTING
          iv_sk TYPE bkpf-bukrs
          iv_my TYPE bkpf-gjahr
          iv_bn TYPE bkpf-belnr,
      display_sf.

  PRIVATE SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'ZGKC_HW_005_S',
      mc_strname2 TYPE tabname VALUE 'ZGKC_HW_005_S2'.
ENDCLASS.

CLASS lcl_application IMPLEMENTATION.
  METHOD instance_app.
    FREE: ro_app.
    IF lcl_application=>app IS NOT BOUND.
      CREATE OBJECT lcl_application=>app.
    ENDIF.
    ro_app = app.
  ENDMETHOD.

  METHOD initialization.
    p_view = '@10@ Tabloyu Görüntüle'.
    p_upd = '@0Z@ Tabloyu Değiştir'.
  ENDMETHOD.

  METHOD at_selection_screen.
    IF sscrfields-ucomm = 'BUT'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'S'
          view_name = 'ZGKC_ISKONTO_T'.
      IF sy-subrc <> 0.
        " Implement suitable error handling here
      ENDIF.
    ELSEIF sscrfields-ucomm = 'BUT2'.
      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action    = 'U'
          view_name = 'ZGKC_ISKONTO_T'.
      IF sy-subrc <> 0.
        " Implement suitable error handling here
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD start_of_selection.
    app->retrieve_dat(
      EXPORTING
        iv_sk     = p_sk
        iv_my     = p_yil
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD retrieve_dat.
    FREE: mt_outdat.

    SELECT bkpf~bukrs, bkpf~belnr, bkpf~gjahr, bkpf~monat, bseg~lifnr, lfa1~name1, bseg~wrbtr, isk~iskonto
    FROM bkpf
    JOIN bseg ON bkpf~bukrs = bseg~bukrs AND bkpf~belnr = bseg~belnr AND bkpf~gjahr = bseg~gjahr
    JOIN lfa1 ON bseg~lifnr = lfa1~lifnr
    JOIN zgkc_iskonto_t AS isk ON bseg~lifnr = isk~satici
    WHERE bkpf~bukrs = @iv_sk AND bkpf~gjahr = @iv_my
    INTO CORRESPONDING FIELDS OF TABLE @mt_outdat.
    IF sy-subrc <> 0.
      MESSAGE 'Girilen şirket kodu ve yıl ile sistemde veri bulunamadı.' TYPE 'I' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<ms_outdat>).
      <ms_outdat>-tutar2 = <ms_outdat>-wrbtr * ( 100 - <ms_outdat>-iskonto ) / 100.
    ENDLOOP.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD alv_session.
    IF NOT lines( app->mt_outdat ) IS INITIAL.
      app->display_alvdat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD display_alvdat.
    IF mo_grid IS NOT BOUND.
      CREATE OBJECT mo_grid
        EXPORTING
          i_parent          = cl_gui_custom_container=>screen0
          i_lifetime        = cl_gui_alv_grid=>lifetime_dynpro
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      create_fieldcat(
        EXPORTING
          im_strname     = mc_strname
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      update_fieldcat(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      attach_handlers(
        EXPORTING
          im_grid        = mo_grid
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      mo_grid->set_table_for_first_display(
        EXPORTING
          i_buffer_active               = space
          i_bypassing_buffer            = abap_true
          is_layout                     = set_layout_dat( )
        CHANGING
          it_fieldcatalog               = mt_fieldcat
          it_outtab                     = app->mt_outdat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.
    ELSE.
      refresh_alv(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD create_fieldcat.
    FREE: mt_fieldcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = mc_strname
      CHANGING
        ct_fieldcat      = mt_fieldcat.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.
  ENDMETHOD.

  METHOD update_fieldcat.
    DATA: _text(40) TYPE c.
    LOOP AT mt_fieldcat REFERENCE INTO DATA(r_fieldcat).
      CLEAR: _text.
      CASE r_fieldcat->fieldname .
        WHEN 'SELKZ'.
          r_fieldcat->tech = abap_true.
        WHEN 'ISKONTO'.
          r_fieldcat->edit = abap_true.
        WHEN 'BELNR'.
          r_fieldcat->hotspot = abap_true.
      ENDCASE.
      IF _text <> space.
        MOVE _text TO: r_fieldcat->scrtext_l, r_fieldcat->scrtext_m, r_fieldcat->scrtext_s, r_fieldcat->reptext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_layout_dat.
    rv_layoutdat = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true ).
  ENDMETHOD.

  METHOD attach_handlers.
    CALL METHOD im_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER me->handle_hotspot_click FOR im_grid.
    SET HANDLER me->handle_user_command  FOR im_grid.
    SET HANDLER me->handle_data_changed   FOR im_grid.
  ENDMETHOD.

  METHOD refresh_alv.
    mo_grid->refresh_table_display(
      EXPORTING
        is_stable = VALUE #( row = abap_true col = abap_true )
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.
  ENDMETHOD.

  METHOD handle_data_changed.
    DATA: ls_modi TYPE lvc_s_modi.

    READ TABLE er_data_changed->mt_mod_cells INTO ls_modi INDEX 1.
    IF sy-subrc IS INITIAL.
      READ TABLE mt_outdat ASSIGNING FIELD-SYMBOL(<ms_outdat>) INDEX ls_modi-row_id.
      IF sy-subrc = 0.
        <ms_outdat>-tutar2 = <ms_outdat>-wrbtr * ( 100 - ls_modi-value ) / 100.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_modi-row_id
            i_fieldname = 'TUTAR2'
            i_value     = <ms_outdat>-tutar2.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    READ TABLE mt_outdat ASSIGNING FIELD-SYMBOL(<ms_outdat>) INDEX e_row_id.
    IF sy-subrc = 0.
      SET PARAMETER ID 'BLN' FIELD <ms_outdat>-belnr.
      SET PARAMETER ID 'BUK' FIELD <ms_outdat>-bukrs.
      SET PARAMETER ID 'GJR' FIELD <ms_outdat>-gjahr.
      CALL TRANSACTION 'FB03'.
    ENDIF.
  ENDMETHOD.

  METHOD handle_user_command.
    mo_grid->get_selected_rows(
      IMPORTING
        et_row_no     = DATA(t_seldat)
    ).

    DATA(lv_count) = 0.
    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>).
      <fs_outdat>-selkz = abap_false.
      READ TABLE t_seldat WITH KEY row_id = sy-tabix ASSIGNING FIELD-SYMBOL(<seldat>).
      IF sy-subrc = 0.
        <fs_outdat>-selkz = abap_true.
        lv_count = lv_count + 1.
      ENDIF.
    ENDLOOP.

    IF lv_count > 1.
      MESSAGE 'Lütfen birden fazla satır seçmeyin.' TYPE 'I'.
      EXIT.
    ELSE.
      READ TABLE mt_outdat ASSIGNING FIELD-SYMBOL(<selected>) WITH KEY selkz = abap_true.
      IF sy-subrc <> 0.
        MESSAGE 'Lütfen bir satır seçin.' TYPE 'I'.
        EXIT.
      ENDIF.
    ENDIF.

    CASE e_ucomm.
      WHEN '&DETAY'.
        retrieve_popup_data(
          EXPORTING
            iv_sk = <selected>-bukrs
            iv_my = <selected>-gjahr
            iv_bn = <selected>-belnr
        ).

      WHEN '&PRINT'.
        retrieve_sf_data(
          EXPORTING
            iv_sk = <selected>-bukrs
            iv_my = <selected>-gjahr
            iv_bn = <selected>-belnr
        ).

        display_sf( ).
    ENDCASE.
  ENDMETHOD.

  METHOD retrieve_popup_data.
    FREE: mt_popdat.

    SELECT bkpf~bukrs, bkpf~gjahr, bkpf~belnr, bseg~buzei, bseg~sgtxt, bseg~gsber, bseg~shkzg, bseg~wrbtr
    FROM bkpf
    JOIN bseg ON bkpf~bukrs = bseg~bukrs AND bkpf~belnr = bseg~belnr AND bkpf~gjahr = bseg~gjahr
    WHERE bkpf~bukrs = @iv_sk AND bkpf~gjahr = @iv_my AND bkpf~belnr = @iv_bn
    INTO CORRESPONDING FIELDS OF TABLE @mt_popdat.

    CALL SCREEN 0200 STARTING AT 50 10 ENDING AT 130 30.
  ENDMETHOD.

  METHOD display_popup_alv.
    IF mo_cont IS NOT BOUND.
      CREATE OBJECT mo_cont
        EXPORTING
          container_name  = 'C_POPUP'.

      cl_salv_table=>factory(
        EXPORTING
          r_container  = mo_cont
        IMPORTING
          r_salv_table = mo_salv
        CHANGING
          t_table = mt_popdat
      ).

      mo_selections = mo_salv->get_selections( ).
      mo_selections->set_selection_mode( 3 ).
      mo_columns = mo_salv->get_columns( ).
      mo_columns->set_optimize( abap_true ).
      mo_salv->get_functions( )->set_default( abap_true ).
      mo_salv->display( ).
    ELSE.
      mo_salv->refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD retrieve_sf_data.
    FREE: mt_sfdat.

    SELECT bkpf~gjahr, bkpf~belnr, bseg~buzei, bseg~sgtxt, bseg~lifnr,
      bseg~gsber, bkpf~xblnr, bseg~shkzg, bseg~wrbtr
    FROM bkpf
    JOIN bseg ON bkpf~bukrs = bseg~bukrs AND bkpf~belnr = bseg~belnr AND bkpf~gjahr = bseg~gjahr
    WHERE bkpf~bukrs = @iv_sk AND bkpf~gjahr = @iv_my AND bkpf~belnr = @iv_bn
    INTO CORRESPONDING FIELDS OF TABLE @mt_sfdat.

    READ TABLE mt_sfdat INTO ms_sfdat INDEX 1.
    READ TABLE mt_outdat ASSIGNING FIELD-SYMBOL(<ms_outdat>) WITH KEY lifnr = ms_sfdat-lifnr.
    ms_sfdat-iskonto = <ms_outdat>-iskonto.
    ms_sfdat-tutar2  = ms_sfdat-wrbtr * ( 100 - ms_sfdat-iskonto ) / 100.
  ENDMETHOD.

  METHOD display_sf.
    DATA: lv_fm_name TYPE rs38l_fnam.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'ZGKC_HW_005_SF'
      IMPORTING
        fm_name            = lv_fm_name.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.

    CALL FUNCTION lv_fm_name
      EXPORTING
        ms_sfdat = ms_sfdat
      TABLES
        mt_sfdat = mt_sfdat.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.
  ENDMETHOD.
ENDCLASS.
