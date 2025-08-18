*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_003_TOP
*&---------------------------------------------------------------------*
*TABLES
TABLES: sscrfields.

*SELECTION_SCREEN
SELECTION-SCREEN BEGIN OF BLOCK options WITH FRAME TITLE TEXT-001.
PARAMETERS: p_std  RADIOBUTTON GROUP gr1 USER-COMMAND usr DEFAULT 'X',
            p_prc  RADIOBUTTON GROUP gr1,
            p_book RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK options.

SELECTION-SCREEN BEGIN OF BLOCK new_student WITH FRAME TITLE TEXT-002.
PARAMETERS: p_sno    TYPE zgkc_ogrno_de MODIF ID sno,
            p_sname  TYPE zgkc_ograd_de MODIF ID std,
            p_ssname TYPE zgkc_ogrsoyad_de MODIF ID std,
            p_sgnd   TYPE zgkc_cinsiyet_de MODIF ID std,
            p_sdt    TYPE zgkc_dtarih_de MODIF ID std,
            p_sclass TYPE zgkc_sinif_de MODIF ID std,
            p_spoint TYPE zgkc_puan_de MODIF ID std.
SELECTION-SCREEN END OF BLOCK new_student.

SELECTION-SCREEN BEGIN OF BLOCK new_process WITH FRAME TITLE TEXT-003.
PARAMETERS: p_pno  TYPE zgkc_islemno_de MODIF ID pno,
            p_psno TYPE zgkc_ogrno_de MATCHCODE OBJECT zgkc_hw_003_sh MODIF ID prc,
            p_pbno TYPE zgkc_kitapno_de MATCHCODE OBJECT zgkc_hw_003_sh5 MODIF ID prc,
            p_pat  TYPE zgkc_atarih_de MODIF ID prc,
            p_pvt  TYPE zgkc_vtarih_de MODIF ID prc.
SELECTION-SCREEN END OF BLOCK new_process.

SELECTION-SCREEN BEGIN OF BLOCK new_book WITH FRAME TITLE TEXT-004.
PARAMETERS: p_bno   TYPE zgkc_kitapno_de MODIF ID bno,
            p_bname TYPE zgkc_kitapad_de MODIF ID bo,
            p_byno  TYPE zgkc_yazarno_de MATCHCODE OBJECT zgkc_hw_003_sh3 MODIF ID bo,
            p_btno  TYPE zgkc_turno_de MATCHCODE OBJECT zgkc_hw_003_sh4 MODIF ID bo,
            p_bpage TYPE zgkc_sayfasayisi_de MODIF ID bo.
SELECTION-SCREEN END OF BLOCK new_book.

SELECTION-SCREEN BEGIN OF BLOCK create WITH FRAME.
SELECTION-SCREEN PUSHBUTTON 47(20) p_but USER-COMMAND but.
SELECTION-SCREEN END OF BLOCK create.


DATA: gs_ogrenci_t TYPE zgkc_ogrenci_t,
      gs_islem_t   TYPE zgkc_islem_t,
      gs_kitap_t   TYPE zgkc_kitap_t,
      gv_ogrno     TYPE nriv-nrlevel,
      gv_islemno   TYPE nriv-nrlevel,
      gv_kitapno   TYPE nriv-nrlevel.
