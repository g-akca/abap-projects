*&---------------------------------------------------------------------*
*& Include          ZGKC_SAS_PR02_CLS
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
      mo_dyndoc_id   TYPE REF TO cl_dd_document,
      mo_splitter    TYPE REF TO cl_gui_splitter_container,
      mo_parent_grid TYPE REF TO cl_gui_container,
      mo_parent_top  TYPE REF TO cl_gui_container,
      mo_html_cntrl  TYPE REF TO cl_gui_html_viewer,
      mt_fieldcat    TYPE lvc_t_fcat.

    DATA:
      mt_outdat TYPE TABLE OF zgkc_po_s2.

    METHODS:
      initialization,
      start_of_selection,
      retrieve_dat
        IMPORTING
          iv_bukrs TYPE bukrs
        EXCEPTIONS
          contains_error
          no_orders,
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
      get_long_date
        IMPORTING
          !im_dat         TYPE datum
        RETURNING
          VALUE(r_dattxt) TYPE text40,
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
      handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id,
      event_top_of_page
        CHANGING
          dg_dyndoc_id TYPE REF TO cl_dd_document,
      display_top_of_page
        IMPORTING
          dg_dyndoc_id TYPE REF TO cl_dd_document,
      send_to_bank,
      create_txt
        IMPORTING
          ms_outdat TYPE zgkc_po_s2.

  PRIVATE SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'zgkc_po_s2'.

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
  ENDMETHOD.

  METHOD start_of_selection.
    app->retrieve_dat(
      EXPORTING
        iv_bukrs     = p_bukrs
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4. RETURN.
    ENDIF.

    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD retrieve_dat.
    FREE: mt_outdat.
    SELECT po~ebeln, po~status, po~waers, SUM( item~wrbtr_sum ) AS wrbtr, po~erdat, po~ernam, po~erzet
      FROM zgkc_po_t AS po
      JOIN zgkc_item_t AS item ON item~ebeln = po~ebeln
      WHERE po~bukrs = @p_bukrs AND item~ebeln IN @s_ebeln AND po~erdat IN @s_erdat AND po~ernam IN @s_ernam AND ( po~status = '03' OR po~status = '04' )
      GROUP BY po~ebeln, po~status, po~waers, po~erdat, po~ernam, po~erzet
      ORDER BY po~ebeln
      INTO CORRESPONDING FIELDS OF TABLE @mt_outdat.

    IF p_green <> 'X'.
      DELETE mt_outdat WHERE status = '04'.
    ENDIF.
    IF p_yellow <> 'X'.
      DELETE mt_outdat WHERE status = '03'.
    ENDIF.
    IF mt_outdat IS INITIAL.
      MESSAGE 'Couldn''''t find any approved purchase orders with this criteria.' TYPE 'I' RAISING no_orders.
    ENDIF.

    SELECT SINGLE iban
      FROM tiban
      WHERE banks = 'TR'
      INTO @DATA(lv_iban).

    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>).
      <fs_outdat>-msgshw = icon_message_faulty_orphan.
      <fs_outdat>-iban = lv_iban.
      IF <fs_outdat>-status = '03'.
        <fs_outdat>-light = '@09@'.
      ELSEIF <fs_outdat>-status = '04'.
        <fs_outdat>-light = '@08@'.
      ENDIF.
    ENDLOOP.
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
*-&Create TOP-Document
      CREATE OBJECT mo_dyndoc_id
        EXPORTING
          style = 'ALV_GRID'.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = cl_gui_custom_container=>screen0
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
          height = 20.

      CREATE OBJECT mo_grid
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

      CALL METHOD mo_dyndoc_id->initialize_document
        EXPORTING
          background_color = cl_dd_area=>col_textarea.

      CALL METHOD mo_grid->list_processing_events
        EXPORTING
          i_event_name = 'TOP_OF_PAGE'
          i_dyndoc_id  = mo_dyndoc_id.
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
        WHEN 'MSGSHW'.
          _text = 'Msg'.
          r_fieldcat->hotspot = abap_true.
      ENDCASE.
      IF _text <> space.
        MOVE _text TO: r_fieldcat->scrtext_l, r_fieldcat->scrtext_m, r_fieldcat->scrtext_s, r_fieldcat->reptext.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_layout_dat.
    rv_layoutdat = VALUE lvc_s_layo( col_opt = abap_true cwidth_opt = abap_true zebra = abap_true sel_mode = 'D' ).
  ENDMETHOD.

  METHOD set_exclude_dat.
    rv_excludedat = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_copy_row ) ( cl_gui_alv_grid=>mc_fc_loc_delete_row ) ( cl_gui_alv_grid=>mc_fc_loc_append_row ) ( cl_gui_alv_grid=>mc_fc_loc_insert_row ) ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                             ( cl_gui_alv_grid=>mc_fc_loc_copy ) ( cl_gui_alv_grid=>mc_fc_loc_cut ) ( cl_gui_alv_grid=>mc_fc_loc_paste ) ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row ) ( cl_gui_alv_grid=>mc_fc_loc_undo )
                             ( cl_gui_alv_grid=>mc_fc_graph ) ( cl_gui_alv_grid=>mc_fc_info ) ( cl_gui_alv_grid=>mc_fc_refresh ) ( cl_gui_alv_grid=>mc_fc_print ) ( cl_gui_alv_grid=>mc_fc_detail ) ).
  ENDMETHOD.

  METHOD attach_handlers.
    CALL METHOD im_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    SET HANDLER me->handle_hotspot_click FOR im_grid.
    SET HANDLER me->handle_toolbar_set   FOR im_grid.
    SET HANDLER me->handle_user_command  FOR im_grid.
    SET HANDLER me->handle_top_of_page   FOR im_grid.
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

  METHOD get_long_date.
    DATA: t_month_names TYPE TABLE OF t247,
          r_month_names TYPE REF TO t247,
          l_daytxt      TYPE hrvsched-daytxt,
          l_year(4)     TYPE c,
          l_month(2)    TYPE c,
          l_day(2)      TYPE c.

    FREE: t_month_names.
    CALL FUNCTION 'MONTH_NAMES_GET'
      EXPORTING
        language    = sy-langu
      TABLES
        month_names = t_month_names.

    l_year = im_dat+0(4).
    l_month = im_dat+4(2).
    l_day = im_dat+6(2).

    CLEAR: l_daytxt.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
      EXPORTING
        langu               = sy-langu
        date                = im_dat
      IMPORTING
        daytxt              = l_daytxt
      EXCEPTIONS
        no_langu            = 1
        no_date             = 2
        no_daytxt_for_langu = 3
        invalid_date        = 4
        OTHERS              = 5.

    READ TABLE t_month_names REFERENCE INTO r_month_names INDEX l_month.
    IF sy-subrc IS INITIAL.
      CONCATENATE l_day r_month_names->ltx l_year l_daytxt INTO r_dattxt SEPARATED BY space.
    ENDIF.
  ENDMETHOD.

  METHOD handle_hotspot_click.
    CASE e_column_id.
      WHEN 'MSGHSW'.
        READ TABLE mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>) INDEX e_row_id.
        IF sy-subrc = 0 AND <fs_outdat>-msgdat[] IS NOT INITIAL.
          cl_rmsl_message=>display( <fs_outdat>-msgdat ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_toolbar_set.
    DELETE e_object->mt_toolbar WHERE function = '&LOCAL&COPY_ROW'
                                   OR function = '&LOCAL&APPEND'
                                   OR function = '&LOCAL&INSERT_ROW'
                                   OR function = '&LOCAL&DELETE_ROW'
                                   OR function = '&LOCAL&CUT'
                                   OR function = '&LOCAL&COPY'
                                   OR function = '&LOCAL&PASTE' .

    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
                                  ( butn_type = 0
                                    function  = '&BANK'
                                    icon      = icon_execute_object
                                    text      = 'Send to Bank'
                                    quickinfo = 'Send to Bank' ) ).
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
      WHEN '&BANK'.
        app->send_to_bank( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_top_of_page.
    event_top_of_page(
      CHANGING
        dg_dyndoc_id = e_dyndoc_id ).
  ENDMETHOD.

  METHOD event_top_of_page.
    DATA : dl_text(255) TYPE c,
           v_name_last  TYPE adrp-name_last,
           v_name_first TYPE adrp-name_first.

    CONSTANTS: c_date_from TYPE adrp-date_from VALUE '00010101'.

*-&Free class and create top of page document;
    FREE dg_dyndoc_id.
    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

*--------------------------------------------------------------------*
*-&HEADER ->
*--------------------------------------------------------------------*
    CLEAR: dl_text.
    CONCATENATE 'Header:' TEXT-h01 INTO dl_text.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>standard
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR: dl_text.
    dl_text = TEXT-h02.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_style    = cl_dd_area=>heading
        sap_fontsize = cl_dd_area=>large
        sap_emphasis = cl_dd_area=>strong.

    CLEAR: dl_text.
    dl_text = 'Username :'.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>strong
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR: dl_text, v_name_last, v_name_first.
    SELECT SINGLE name_last name_first
      FROM usr21 JOIN adrp ON adrp~persnumber = usr21~persnumber AND
                              adrp~date_from  = c_date_from AND
                              adrp~nation     = space
        INTO (v_name_last, v_name_first)
        WHERE usr21~bname = sy-uname.
    CONCATENATE v_name_first v_name_last INTO dl_text SEPARATED BY space.

    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading
        sap_color    = cl_dd_area=>list_negative_inv.

    CLEAR: dl_text.
    dl_text = 'Date :'.
    CALL METHOD dg_dyndoc_id->new_line.
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>strong
        sap_color    = cl_dd_area=>list_heading_int.

    CLEAR dl_text.
    dl_text = get_long_date( im_dat = sy-datum ).
    CALL METHOD dg_dyndoc_id->add_text
      EXPORTING
        text         = dl_text
        sap_emphasis = cl_dd_area=>heading
        sap_color    = cl_dd_area=>list_negative_inv.

    display_top_of_page(
      EXPORTING
        dg_dyndoc_id = dg_dyndoc_id ).

  ENDMETHOD.

  METHOD display_top_of_page.
    IF app->mo_html_cntrl IS INITIAL.
      CREATE OBJECT app->mo_html_cntrl
        EXPORTING
          parent = app->mo_parent_top.
    ENDIF.
    CALL METHOD dg_dyndoc_id->merge_document.
    dg_dyndoc_id->html_control = app->mo_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = abap_true
        parent             = app->mo_parent_top
      EXCEPTIONS
        html_display_error = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD send_to_bank.
    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>) WHERE selkz = abap_true.
      IF <fs_outdat>-status <> '03'.
        MESSAGE 'Can''''t send orders already sent to bank.' TYPE 'I'.
        RETURN.
      ENDIF.

      app->create_txt(
        EXPORTING
          ms_outdat = <fs_outdat>
      ).

      UPDATE zgkc_po_t
      SET status = '04'
      WHERE ebeln = <fs_outdat>-ebeln.

      <fs_outdat>-light = '@08@'.
      <fs_outdat>-status = '04'.
    ENDLOOP.

    MESSAGE 'Sent the selected item(s) to bank successfully.' TYPE 'I'.
    app->refresh_alv( ).
  ENDMETHOD.

  METHOD create_txt.
    DATA: lv_filename TYPE string,
          lv_fullpath TYPE string.

    DATA: lt_data TYPE TABLE OF string,
          ls_data TYPE string.

    APPEND |Documenting Number: { ms_outdat-ebeln }, Total Price: { ms_outdat-wrbtr }, Currency: { ms_outdat-waers }, IBAN: { ms_outdat-iban }, Creation Date: { ms_outdat-erdat }, Time: { ms_outdat-erzet }, Creator: { ms_outdat-ernam }| TO lt_data.
    DATA: mt_itemdat TYPE TABLE OF zgkc_item_t.

    SELECT *
      FROM zgkc_item_t
      WHERE ebeln = @ms_outdat-ebeln
      INTO CORRESPONDING FIELDS OF TABLE @mt_itemdat.

    LOOP AT mt_itemdat ASSIGNING FIELD-SYMBOL(<ms_itemdat>).
      APPEND |Item: { <ms_itemdat>-ebelp }, Material ID: { <ms_itemdat>-matnr }, Material: { <ms_itemdat>-maktx }, Quantity: { <ms_itemdat>-menge }, UoM: { <ms_itemdat>-meins }, Material Type: { <ms_itemdat>-mtart }, | && |Material Group: {
<ms_itemdat>-matkl }, Price: { <ms_itemdat>-wrbtr }, Total Price: { <ms_itemdat>-wrbtr_sum }| TO lt_data.
    ENDLOOP.

    lv_filename = |{ sy-datum }_{ sy-uzeit }_{ ms_outdat-ebeln }.txt|.
    lv_fullpath = '/usr/sap/DIR_BANKA/' && lv_filename.

    OPEN DATASET lv_fullpath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc <> 0.
      MESSAGE 'Error opening file' TYPE 'E'.
      EXIT.
    ENDIF.

    LOOP AT lt_data INTO ls_data.
      TRANSFER ls_data TO lv_fullpath.
    ENDLOOP.

    CLOSE DATASET lv_fullpath.
  ENDMETHOD.
ENDCLASS.