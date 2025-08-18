*&---------------------------------------------------------------------*
*& Report ZGKC_HW_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgkc_hw_002.

PARAMETERS: p_no    TYPE char10 OBLIGATORY,
            p_name  TYPE string OBLIGATORY,
            p_surnm TYPE string OBLIGATORY,
            p_mt_1  TYPE p DECIMALS 2 OBLIGATORY,
            p_mt_2  TYPE p DECIMALS 2 OBLIGATORY,
            p_final TYPE p DECIMALS 2 OBLIGATORY.

DATA gv_avg TYPE p DECIMALS 2.

INITIALIZATION.

START-OF-SELECTION.
  IF p_mt_1 > 100 OR p_mt_1 < 0 OR p_mt_2 > 100 OR p_mt_2 < 0 OR p_final > 100 OR p_final < 0.
    WRITE 'Lütfen notları 0-100 aralığında girin.'.
  ELSE.
    PERFORM calculate_avg USING p_mt_1 p_mt_2 p_final.
    WRITE: 'No: ', p_no,
           'Adı: ', p_name,
           'Soyadı: ', p_surnm,
           '1. Vize: ', p_mt_1,
           '2. Vize: ', p_mt_2,
           'Final: ', p_final,
           'Ortalama: ', gv_avg.
  ENDIF.

END-OF-SELECTION.

FORM calculate_avg USING mt1 mt2 fn.
  gv_avg = ( mt1 * 30 / 100 ) + ( mt2 * 30 / 100 ) + ( fn * 40 / 100 ).
  IF gv_avg >= 50 AND fn >= 50.
    WRITE: 'Öğrenci dönemi başarıyla geçti.', /.
  ELSE.
    WRITE: 'Öğrenci kaldı. Dönemi tekrar edecek.', /.
  ENDIF.
ENDFORM.
