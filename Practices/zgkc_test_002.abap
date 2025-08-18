*&---------------------------------------------------------------------*
*& Report ZGKC_TEST_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGKC_TEST_002.

*Declaration
DATA: gv_test1 TYPE p DECIMALS 3,
      gv_test2 TYPE int2,
      gv_result TYPE p DECIMALS 1.

*Assignment
gv_test1 = '1.5'.
gv_test2 = 2.
gv_result = gv_test1 + gv_test2.

*Output
WRITE: gv_test1,
       / gv_test2,
       / 'Sayıların toplamı:',
       gv_result.

*If-Else condition
IF gv_result > 10.
  WRITE: '(Yani toplam 10dan yüksek.)', /.
ELSEIF gv_result < 10.
  WRITE: '(Yani toplam 10dan düşük.)', /.
ELSE.
  WRITE: '(Yani toplam 10a eşit.)', /.
ENDIF.

*Case condition
CASE gv_test2.
  WHEN 1.
    WRITE '2. sayının değeri: 1.'.
  WHEN 2.
    WRITE '2. sayının değeri: 2.'.
  WHEN 3.
    WRITE '2. sayının değeri: 3.'.
  WHEN 4.
    WRITE '2. sayının değeri: 4.'.
  WHEN 5.
    WRITE '2. sayının değeri: 5.'.
  WHEN OTHERS.
    WRITE '2. sayının değeri: 5ten büyük veya 1den küçük.'.
ENDCASE.

*Do loop
DO 5 TIMES.
  gv_test2 = gv_test2 + 1.
ENDDO.
WRITE: /, '2. sayı şimdi 5 arttı ve yeni değeri:', gv_test2.

*While loop
WHILE gv_test2 < 10.
  gv_test2 = gv_test2 + 1.
  WRITE: /, '2. sayı 10 olana kadar arttırılıyor... Şuanki değeri: ', gv_test2.
ENDWHILE.
