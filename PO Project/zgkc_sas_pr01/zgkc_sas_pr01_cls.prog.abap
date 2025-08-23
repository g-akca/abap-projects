*&---------------------------------------------------------------------*
*& Include          ZGKC_SAS_PR01_CLS
*&---------------------------------------------------------------------*

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      app TYPE REF TO lcl_application.

    CLASS-METHODS:
      instance_app
        RETURNING
          VALUE(ro_app) TYPE REF TO lcl_application.

    DATA:
      mo_grid        TYPE REF TO cl_gui_alv_grid,
      mo_splitter    TYPE REF TO cl_gui_splitter_container,
      mo_container   TYPE REF TO cl_gui_custom_container,
      mo_parent_grid TYPE REF TO cl_gui_container,
      mo_parent_top  TYPE REF TO cl_gui_container,
      mt_fieldcat    TYPE lvc_t_fcat,
      mo_grid2       TYPE REF TO cl_gui_alv_grid.

    DATA:
      mt_outdat  TYPE TABLE OF zgkc_po_s,
      mt_outdat2 TYPE TABLE OF zgkc_item_s,
      ms_outdat2 TYPE zgkc_item_s.

    METHODS:
      initialization,
      at_selection_screen,
      start_of_selection,
      retrieve_dat
        IMPORTING
          iv_bukrs TYPE bukrs
        EXCEPTIONS
          contains_error
          no_orders,
      retrieve_dat2
        IMPORTING
          iv_ebeln TYPE ebeln
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
      update_fieldcat2
        EXCEPTIONS
          contains_error,
      set_layout_dat
        RETURNING
          VALUE(rv_layoutdat) TYPE lvc_s_layo,
      set_exclude_dat
        RETURNING
          VALUE(rv_excludedat) TYPE ui_functions,
      attach_handlers
        IMPORTING
          VALUE(im_grid) TYPE REF TO cl_gui_alv_grid
        EXCEPTIONS
          contains_error,
      refresh_alv
        EXCEPTIONS
          contains_error,
      refresh_alv2
        EXCEPTIONS
          contains_error,
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING
          e_column_id
          e_row_id,
      handle_toolbar_set FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,
      approve_po,
      reject_po.

  PRIVATE SECTION.
    CONSTANTS:
      mc_strname  TYPE tabname VALUE 'ZGKC_PO_S',
      mc_strname2 TYPE tabname VALUE 'ZGKC_ITEM_S'.

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
    CONCATENATE ICON_VARIANTS '' INTO SSCRFIELDS-FUNCTXT_01.
  ENDMETHOD.

  METHOD at_selection_screen.
  ENDMETHOD.

  METHOD start_of_selection.
    CASE sscrfields-ucomm.
      WHEN 'FC01'.
        " whatever this button does
      WHEN OTHERS.
        app->retrieve_dat(
          EXPORTING
            iv_bukrs = p_bukrs
          EXCEPTIONS
            contains_error = 1
            no_orders      = 2
            OTHERS         = 3 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. RETURN.
        ENDIF.

        CALL SCREEN 0100.
      ENDCASE.
  ENDMETHOD.

  METHOD retrieve_dat.
    FREE: mt_outdat.
    SELECT *
      FROM zgkc_po_t
      WHERE bukrs = @p_bukrs AND ebeln IN @s_ebeln AND erdat IN @s_erdat AND ernam IN @s_ernam
      INTO CORRESPONDING FIELDS OF TABLE @mt_outdat.
    IF sy-subrc <> 0.
      MESSAGE 'Couldn''''t find any purchase orders with this criteria.' TYPE 'I' RAISING no_orders.
    ENDIF.

    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>).
      <fs_outdat>-msgshw = icon_message_faulty_orphan.
      IF <fs_outdat>-status = '01'.
        <fs_outdat>-light = '@09@'.
      ELSEIF <fs_outdat>-status = '02'.
        <fs_outdat>-light = '@08@'.
      ELSEIF <fs_outdat>-status = '03'.
        <fs_outdat>-light = '@0A@'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD retrieve_dat2.
    FREE: mt_outdat2.
    SELECT *
      FROM zgkc_item_t
      WHERE ebeln = @iv_ebeln
      INTO CORRESPONDING FIELDS OF TABLE @mt_outdat2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.
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
    IF mo_grid IS NOT BOUND OR mo_grid2 IS NOT BOUND.
      CREATE OBJECT mo_container
        EXPORTING
          container_name = 'CONT1'.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_container
          rows    = 2
          columns = 1.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_parent_top.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 2
          column    = 1
        RECEIVING
          container = mo_parent_grid.

      CALL METHOD mo_splitter->set_row_height
        EXPORTING
          id     = 1
          height = 50.

      CREATE OBJECT mo_grid
        EXPORTING
          i_parent          = mo_parent_top
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
          it_toolbar_excluding          = CONV #( set_exclude_dat( ) )
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

      CREATE OBJECT mo_grid2
        EXPORTING
          i_parent          = mo_parent_grid
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
          im_strname     = mc_strname2
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      update_fieldcat2(
        EXCEPTIONS
          contains_error = 1
          OTHERS         = 2 ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
      ENDIF.

      mo_grid2->set_table_for_first_display(
        EXPORTING
          i_buffer_active               = space
          i_bypassing_buffer            = abap_true
          is_layout                     = set_layout_dat( )
          it_toolbar_excluding          = CONV #( set_exclude_dat( ) )
        CHANGING
          it_fieldcatalog               = mt_fieldcat
          it_outtab                     = app->mt_outdat2
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

      refresh_alv2(
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
        i_structure_name       = im_strname
      CHANGING
        ct_fieldcat            = mt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.
  ENDMETHOD.

  METHOD update_fieldcat.
    DATA: _text(40) TYPE c.
    LOOP AT mt_fieldcat REFERENCE INTO DATA(r_fieldcat).
      CLEAR: _text.
      CASE r_fieldcat->fieldname.
        WHEN 'SELKZ' OR 'MSGDAT' OR 'STATUS'.
          r_fieldcat->tech = abap_true.
        WHEN 'LIGHT'.
          _text = 'Status'.
        WHEN 'EBELN'.
          r_fieldcat->hotspot = abap_true.
        WHEN 'MSGSHW'.
          _text = 'Msg'.
          r_fieldcat->hotspot = abap_true.
      ENDCASE.
      IF _text <> space.
        MOVE _text TO: r_fieldcat->scrtext_l, r_fieldcat->scrtext_m, r_fieldcat->scrtext_s, r_fieldcat->reptext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD update_fieldcat2.
    LOOP AT mt_fieldcat REFERENCE INTO DATA(r_fieldcat).
      CASE r_fieldcat->fieldname.
        WHEN 'SELKZ'.
          r_fieldcat->tech = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_layout_dat.
    rv_layoutdat = VALUE lvc_s_layo( col_opt = abap_true
                                     cwidth_opt = abap_true
                                     zebra = abap_true
                                     sel_mode = 'D' ).
  ENDMETHOD.

  METHOD set_exclude_dat.
    rv_excludedat = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_copy )
                             ( cl_gui_alv_grid=>mc_fc_loc_cut )
                             ( cl_gui_alv_grid=>mc_fc_loc_paste )
                             ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_undo )
                             ( cl_gui_alv_grid=>mc_fc_graph )
                             ( cl_gui_alv_grid=>mc_fc_info )
                             ( cl_gui_alv_grid=>mc_fc_refresh )
                             ( cl_gui_alv_grid=>mc_fc_print )
                             ( cl_gui_alv_grid=>mc_fc_detail )
                             ( cl_gui_alv_grid=>mc_fc_check )
                             ( cl_gui_alv_grid=>mc_fc_views )
                             ( cl_gui_alv_grid=>mc_fc_find_more )
                             ( cl_gui_alv_grid=>mc_fc_find ) ).
  ENDMETHOD.

  METHOD attach_handlers.
    CALL METHOD im_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER me->handle_hotspot_click FOR im_grid.
    SET HANDLER me->handle_toolbar_set   FOR im_grid.
    SET HANDLER me->handle_user_command  FOR im_grid.
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

  METHOD refresh_alv2.
    mo_grid2->refresh_table_display(
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

  METHOD handle_hotspot_click.
    CASE e_column_id.
      WHEN 'EBELN'.
        READ TABLE mt_outdat ASSIGNING FIELD-SYMBOL(<ms_outdat>) INDEX e_row_id.
        IF sy-subrc = 0.
          app->retrieve_dat2(
            EXPORTING
              iv_ebeln = <ms_outdat>-ebeln
            EXCEPTIONS
              contains_error = 1
              OTHERS         = 2 ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. RETURN.
          ENDIF.

          app->refresh_alv2( ).
        ENDIF.
      WHEN 'MSGSHW'.
        "show error messages
    ENDCASE.
  ENDMETHOD.

  METHOD handle_toolbar_set.
    DELETE e_object->mt_toolbar WHERE function = '&LOCAL&COPY_ROW'
                                   OR function = '&LOCAL&APPEND'
                                   OR function = '&LOCAL&INSERT_ROW'
                                   OR function = '&LOCAL&DELETE_ROW'
                                   OR function = '&LOCAL&CUT'
                                   OR function = '&LOCAL&COPY'
                                   OR function = '&LOCAL&PASTE'.

    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
                                  ( butn_type = 0
                                    function  = '&APPROVE'
                                    icon      = '@01@'
                                    text      = 'Approve'
                                    quickinfo = 'Approve' ) ).

    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
                                  ( butn_type = 0
                                    function  = '&REJECT'
                                    icon      = '@02@'
                                    text      = 'Reject'
                                    quickinfo = 'Reject' ) ).

  ENDMETHOD.

  METHOD handle_user_command.
    mo_grid->get_selected_rows(
      IMPORTING
        et_row_no     = DATA(t_seldat)
    ).

    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>).
      <fs_outdat>-selkz = abap_false.
      READ TABLE t_seldat WITH KEY row_id = sy-tabix ASSIGNING FIELD-SYMBOL(<seldat>).
      IF sy-subrc = 0.
        <fs_outdat>-selkz = abap_true.
      ENDIF.
    ENDLOOP.

    READ TABLE mt_outdat ASSIGNING FIELD-SYMBOL(<selected>) WITH KEY selkz = abap_true.
    IF sy-subrc <> 0.
      MESSAGE 'Please select an item.' TYPE 'I'.
      EXIT.
    ENDIF.

    CASE e_ucomm.
      WHEN '&APPROVE'.
        app->approve_po( ).
      WHEN '&REJECT'.
        app->reject_po( ).
    ENDCASE.
  ENDMETHOD.

  METHOD approve_po.
    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>) WHERE selkz = abap_true.
      UPDATE zgkc_po_t
      SET status = '02'
      WHERE ebeln = <fs_outdat>-ebeln.

      <fs_outdat>-light = '@08@'.
      <fs_outdat>-status = '02'.
    ENDLOOP.

    MESSAGE 'Approved the selected purchase order(s) successfully.' TYPE 'I'.
    app->refresh_alv( ).
  ENDMETHOD.

  METHOD reject_po.
    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>) WHERE selkz = abap_true.
      UPDATE zgkc_po_t
      SET status = '03'
      WHERE ebeln = <fs_outdat>-ebeln.

      <fs_outdat>-light = '@0A@'.
      <fs_outdat>-status = '03'.
    ENDLOOP.

    MESSAGE 'Rejected the selected purchase order(s) successfully.' TYPE 'I'.
    app->refresh_alv( ).
  ENDMETHOD.
ENDCLASS.