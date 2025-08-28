*&---------------------------------------------------------------------*
*& Include          LZGKC_SAS_FG02CLS
*&---------------------------------------------------------------------*

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA:
      app TYPE REF TO lcl_application.

    CLASS-METHODS:
      instance_app
        RETURNING
          VALUE(ro_app) TYPE REF TO lcl_application.

    DATA: init,
          url   TYPE cndp_url.

    TYPES: pict_line(1022) TYPE x,
           pict_tab        TYPE STANDARD TABLE OF pict_line WITH EMPTY KEY.

    METHODS:
      initialization.

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
    DATA: pict_tab TYPE pict_tab.
    FREE: pict_tab.

    cl_mime_repository_api=>get_api( )->get(
      EXPORTING
        i_url = '/SAP/PUBLIC/ZGKC_SAS/finpro_solution_consulting_logo.jpg'
      IMPORTING
        e_content = DATA(pict_wa)
      EXCEPTIONS
        OTHERS = 4
    ).
    IF sy-subrc = 4.
      RETURN.
    ENDIF.
    pict_tab = VALUE #( LET l1 = xstrlen( pict_wa ) l2 = l1 - 1022 IN
                FOR j = 0 THEN j + 1022 UNTIL j >= l1
                  ( COND #( WHEN j <= l2 THEN pict_wa+j(1022)
                            ELSE pict_wa+j ) ) ).

    IF init IS INITIAL.
      DATA(custom_container) = NEW cl_gui_custom_container( container_name = 'CONT1' ).

      DATA: go_pic TYPE REF TO cl_gui_picture.
      go_pic = NEW cl_gui_picture( parent = custom_container ).

      DATA(ext_data) = VALUE cl_abap_browser=>load_tab( ( name = 'PICT.PNG'
                                                          type = 'image'
                                                          dref = REF #( pict_tab ) ) ).
      DATA: lv_url TYPE c LENGTH 100.
      CALL FUNCTION 'DP_CREATE_URL'
        EXPORTING
          type                 = 'image/png'
          subtype              = 'png'
        TABLES
          data                 = pict_tab
        CHANGING
          url                  = lv_url
        EXCEPTIONS
          dp_invalid_parameter = 1
          dp_error_put_table   = 2
          dp_error_general     = 3
          OTHERS               = 4.

      go_pic->load_picture_from_url_async( lv_url ).
      go_pic->set_display_mode( cl_gui_picture=>display_mode_fit_center ).

      init = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.