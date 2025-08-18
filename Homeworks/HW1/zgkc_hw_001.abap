*&---------------------------------------------------------------------*
*& Report ZGKC_HW_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgkc_hw_001.

SELECTION-SCREEN BEGIN OF BLOCK shape WITH FRAME TITLE TEXT-001.
PARAMETERS: p_square AS CHECKBOX USER-COMMAND check,
            p_rect   AS CHECKBOX USER-COMMAND check,
            p_tri    AS CHECKBOX USER-COMMAND check.
SELECTION-SCREEN END OF BLOCK shape.

SELECTION-SCREEN BEGIN OF BLOCK calc WITH FRAME TITLE TEXT-002.
PARAMETERS: p_area  RADIOBUTTON GROUP r1,
            p_perim RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK calc.

SELECTION-SCREEN BEGIN OF BLOCK var WITH FRAME TITLE TEXT-003.
PARAMETERS: p_sq_sd  TYPE p DECIMALS 2 MODIF ID gr1,
            p_rc_sd1 TYPE p DECIMALS 2 MODIF ID gr2,
            p_rc_sd2 TYPE p DECIMALS 2 MODIF ID gr2,
            p_base   TYPE p DECIMALS 2 MODIF ID gr3,
            p_height TYPE p DECIMALS 2 MODIF ID gr3.
SELECTION-SCREEN END OF BLOCK var.

DATA: gv_last_checked TYPE string,
      gv_result       TYPE p DECIMALS 2.


INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
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

START-OF-SELECTION.
  IF p_square EQ abap_true AND p_area EQ abap_true.
    PERFORM calc_sq_area.
  ELSEIF p_square EQ abap_true AND p_perim EQ abap_true.
    PERFORM calc_sq_perim.
  ELSEIF p_rect EQ abap_true AND p_area EQ abap_true.
    PERFORM calc_rc_area.
  ELSEIF p_rect EQ abap_true AND p_perim EQ abap_true.
    PERFORM calc_rc_perim.
  ELSEIF p_tri EQ abap_true AND p_area EQ abap_true.
    PERFORM calc_tri_area.
  ELSEIF p_tri EQ abap_true AND p_perim EQ abap_true.
    PERFORM calc_tri_perim.
  ELSE.
    WRITE 'Lütfen bir şekil seçin.'.
  ENDIF.

END-OF-SELECTION.

FORM calc_sq_area.
  gv_result = p_sq_sd * p_sq_sd.
  WRITE: 'Kare Alanı: ', gv_result.
ENDFORM.
FORM calc_sq_perim.
  gv_result = p_sq_sd * 4.
  WRITE: 'Kare Çevresi: ', gv_result.
ENDFORM.
FORM calc_rc_area.
  gv_result = p_rc_sd1 * p_rc_sd2.
  WRITE: 'Dikdörtgen Alanı: ', gv_result.
ENDFORM.
FORM calc_rc_perim.
  gv_result = ( p_rc_sd1 + p_rc_sd2 ) * 2.
  WRITE: 'Dikdörtgen Çevresi: ', gv_result.
ENDFORM.
FORM calc_tri_area.
  gv_result = p_base * p_height / 2.
  WRITE: 'Üçgen Alanı: ', gv_result.
ENDFORM.
FORM calc_tri_perim.
  gv_result = sqrt( p_height ** 2 + p_base ** 2 ) + p_height + p_base.
  WRITE: 'Üçgen Çevresi: ', gv_result.
ENDFORM.
