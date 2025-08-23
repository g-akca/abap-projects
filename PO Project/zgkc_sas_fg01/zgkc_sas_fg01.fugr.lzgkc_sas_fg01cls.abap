*&---------------------------------------------------------------------*
*& Include          LZGKC_SAS_FG01CLS
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
      mv_matnr TYPE mara-matnr.

    DATA:
      mo_grid        TYPE REF TO cl_gui_alv_grid,
      mo_container   TYPE REF TO cl_gui_custom_container,
      mo_splitter    TYPE REF TO cl_gui_splitter_container,
      mo_parent_grid TYPE REF TO cl_gui_container,
      mt_fieldcat    TYPE lvc_t_fcat.

    DATA:
      mt_outdat TYPE zgkc_po_item_tt,
      ms_item   TYPE zgkc_item_t,
      ms_po     TYPE zgkc_po_t.

    METHODS:
      initialization,
      validate_material,
      validate_inputs,
      update_key,
      alv_session
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
      refresh_item_num,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm,
      handle_toolbar_set FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,
      add_new_item,
      clear_screen,
      send_to_approval,
      send_email
        IMPORTING
          iv_ebeln TYPE ebeln.

  PRIVATE SECTION.
    CONSTANTS:
      mc_strname TYPE tabname VALUE 'ZGKC_PO_ITEM'.

ENDCLASS.

CLASS lcl_application IMPLEMENTATION.
  METHOD instance_app.
    FREE ro_app.
    IF lcl_application=>app IS NOT BOUND.
      CREATE OBJECT lcl_application=>app.
    ENDIF.
    ro_app = app.
  ENDMETHOD.

  METHOD initialization.
    IF zgkc_po_item-ebeln IS INITIAL.
      zgkc_po_item-ebeln = '$000000000'.
    ENDIF.
    IF zgkc_po_item-datum IS INITIAL.
      zgkc_po_item-datum = sy-datum.
    ENDIF.
    IF zgkc_po_item-uzeit IS INITIAL.
      zgkc_po_item-uzeit = sy-uzeit.
    ENDIF.
    IF zgkc_po_item-uname IS INITIAL.
      zgkc_po_item-uname = sy-uname.
    ENDIF.

    IF zgkc_po_key-bukrs IS INITIAL.
      zgkc_po_key-bukrs = '1000'.
    ENDIF.
    IF zgkc_po_key-waers IS INITIAL.
      zgkc_po_key-waers = 'TRY'.
    ENDIF.

    app->update_key( ).

    app->alv_session(
      EXCEPTIONS
        contains_error = 1
        OTHERS         = 2  ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD validate_material.
    IF sy-ucomm <> '&BACK' AND sy-ucomm <> '&CANCEL' AND sy-ucomm <> '&EXIT'
    AND sy-ucomm <> '&KEY' AND sy-ucomm <> '&CLEAR' AND sy-ucomm <> '&SEND'.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = zgkc_po_item-matnr
        IMPORTING
          output = mv_matnr.
      "gs_item-matnr = |{ gv_material ALPHA = IN }|.

      SELECT SINGLE makt~maktx, mara~meins, mara~mtart, mara~matkl, t134t~mtbez, t023t~wgbez
        FROM mara
          JOIN makt ON makt~matnr = mara~matnr
          JOIN t134t ON t134t~mtart = mara~mtart
          JOIN t023t ON t023t~matkl = mara~matkl
        WHERE mara~matnr = @mv_matnr
          INTO CORRESPONDING FIELDS OF @zgkc_po_item.
      IF sy-subrc <> 0.
        MESSAGE 'Please enter a valid material number.' TYPE 'E'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD validate_inputs.
    IF sy-ucomm = '&INSERT'.
      IF zgkc_po_item-ebeln IS INITIAL OR zgkc_po_item-menge IS INITIAL OR zgkc_po_item-meins IS INITIAL OR
        zgkc_po_item-mtart IS INITIAL OR zgkc_po_item-matkl IS INITIAL OR zgkc_po_item-wrbtr IS INITIAL OR
        zgkc_po_item-waers IS INITIAL OR zgkc_po_item-matnr IS INITIAL.
        MESSAGE 'Please fill all fields.' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD update_key.
    zgkc_po_item-bukrs = zgkc_po_key-bukrs.
    zgkc_po_item-waers = zgkc_po_key-waers.
    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>).
      <fs_outdat>-bukrs = zgkc_po_key-bukrs.
      <fs_outdat>-waers = zgkc_po_key-waers.
    ENDLOOP.
  ENDMETHOD.

  METHOD alv_session.
    IF mo_grid IS NOT BOUND.
*-&Create TOP-Document
      CREATE OBJECT mo_container
        EXPORTING
          container_name = 'CONT1'.

      CREATE OBJECT mo_splitter
        EXPORTING
          parent  = mo_container
          rows    = 1
          columns = 1.

      CALL METHOD mo_splitter->get_container
        EXPORTING
          row       = 1
          column    = 1
        RECEIVING
          container = mo_parent_grid.

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
      CASE r_fieldcat->fieldname .
        WHEN 'SELKZ' OR 'MTBEZ' OR 'WGBEZ' OR 'DATUM' OR 'UZEIT' OR 'UNAME' OR 'TOTAL_PRICE'.
          r_fieldcat->tech = abap_true.
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
                             ( cl_gui_alv_grid=>mc_fc_graph ) ( cl_gui_alv_grid=>mc_fc_info ) ( cl_gui_alv_grid=>mc_fc_refresh ) ( cl_gui_alv_grid=>mc_fc_print ) ( cl_gui_alv_grid=>mc_fc_detail ) ) .
  ENDMETHOD.

  METHOD attach_handlers.
    CALL METHOD im_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING contains_error.
    ENDIF.

    SET HANDLER: me->handle_user_command FOR im_grid,
                 me->handle_toolbar_set FOR im_grid.
  ENDMETHOD.

  METHOD refresh_alv.
    app->refresh_item_num( ).

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

  METHOD refresh_item_num.
    DATA(lv_ebelp) = 1.
    LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>).
      <fs_outdat>-ebelp = lv_ebelp.
      lv_ebelp = lv_ebelp + 1.
    ENDLOOP.
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
      WHEN '&DEL'.
        LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<sel>) WHERE selkz = abap_true.
          DELETE mt_outdat WHERE ebelp = <sel>-ebelp.
        ENDLOOP.

        app->refresh_alv( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_toolbar_set.
    e_object->mt_toolbar = VALUE #( BASE e_object->mt_toolbar
                                  ( butn_type = 0
                                    function  = '&DEL'
                                    text      = 'Delete PO Item'
                                    quickinfo = 'Delete Row' ) ).
  ENDMETHOD.

  METHOD add_new_item.
    zgkc_po_item-wrbtr_sum = zgkc_po_item-menge * zgkc_po_item-wrbtr.
    APPEND zgkc_po_item TO mt_outdat.
    zgkc_po_item-total_price = zgkc_po_item-total_price + zgkc_po_item-wrbtr_sum.
  ENDMETHOD.

  METHOD clear_screen.
    FREE: mt_outdat, zgkc_po_item.
  ENDMETHOD.

  METHOD send_to_approval.
    " optimize this part
    IF mt_outdat IS NOT INITIAL.
      FREE: ms_po.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '1'
          object      = 'ZGKC_PONR'
        IMPORTING
          number      = ms_po-ebeln.
      ms_po-ebeln = |P{ ms_po-ebeln }|.
      ms_po-status = '01'.
      ms_po-bukrs = zgkc_po_key-bukrs.
      ms_po-waers = zgkc_po_key-waers.
      ms_po-erdat = zgkc_po_item-datum.
      ms_po-erzet = zgkc_po_item-uzeit.
      ms_po-ernam = zgkc_po_item-uname.
      INSERT INTO zgkc_po_t VALUES ms_po.

      LOOP AT mt_outdat ASSIGNING FIELD-SYMBOL(<fs_outdat>).
        ms_item-ebeln = ms_po-ebeln.
        ms_item-ebelp = <fs_outdat>-ebelp.
        ms_item-matnr = <fs_outdat>-matnr.
        ms_item-maktx = <fs_outdat>-maktx.
        ms_item-menge = <fs_outdat>-menge.
        ms_item-meins = <fs_outdat>-meins.
        ms_item-mtart = <fs_outdat>-mtart.
        ms_item-matkl = <fs_outdat>-matkl.
        ms_item-wrbtr = <fs_outdat>-wrbtr.
        ms_item-wrbtr_sum = <fs_outdat>-wrbtr_sum.
        INSERT INTO zgkc_item_t VALUES ms_item.
      ENDLOOP.

      app->send_email(
        EXPORTING
          iv_ebeln = ms_po-ebeln
      ).

      app->clear_screen( ).
      MESSAGE 'Purchase Order has been sent to approval.' TYPE 'I' DISPLAY LIKE 'S'.
    ELSE.
      MESSAGE 'Please add items to the Purchase Order.' TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD send_email.

  ENDMETHOD.
ENDCLASS.