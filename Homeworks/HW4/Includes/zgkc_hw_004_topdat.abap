*&---------------------------------------------------------------------*
*& Include          ZGKC_HW_004_TOPDAT
*&---------------------------------------------------------------------*

" Main ALV
TYPES: BEGIN OF gty_list,
         secim        TYPE c,
         istisna      TYPE char4,
         color        TYPE char4,
         islemno      TYPE zgkc_islemno_de,
         ogrno        TYPE zgkc_ogrno_de,
         ograd        TYPE zgkc_ograd_de,
         ogrsoyad     TYPE zgkc_ogrsoyad_de,
         ogradsoyad   TYPE zgkc_ograd_de,
         kitapno      TYPE zgkc_kitapno_de,
         kitapad      TYPE zgkc_kitapad_de,
         yazarno      TYPE zgkc_yazarno_de,
         yazarad      TYPE zgkc_yazarad_de,
         yazarsoyad   TYPE zgkc_yazarsoyad_de,
         yazaradsoyad TYPE zgkc_yazarad_de,
         turno        TYPE zgkc_turno_de,
         turad        TYPE zgkc_turad_de,
         atarih       TYPE zgkc_atarih_de,
         vtarih       TYPE zgkc_vtarih_de,
       END OF gty_list.

DATA: gt_list         TYPE TABLE OF gty_list,
      gs_list         TYPE gty_list,
      gt_fieldcatalog TYPE slis_t_fieldcat_alv,
      gs_fieldcatalog TYPE slis_fieldcat_alv,
      gs_layout       TYPE slis_layout_alv,
      gt_events       TYPE slis_t_event,
      gs_event        TYPE slis_alv_event.

" Report ALV
TYPES: BEGIN OF gty_rapor,
         ogrno      TYPE zgkc_ogrno_de,
         ogradsoyad TYPE zgkc_ograd_de,
         toplam     TYPE i,
         verilmeyen TYPE i,
         geciken    TYPE i,
         teslim     TYPE i,
       END OF gty_rapor.

DATA: gt_rapor    TYPE TABLE OF gty_rapor,
      gs_rapor    TYPE gty_rapor,
      gt_rapor_fc TYPE slis_t_fieldcat_alv,
      gs_rapor_fc TYPE slis_fieldcat_alv,
      gs_rapor_lo TYPE slis_layout_alv,
      gt_rapor_ev TYPE slis_t_event,
      gs_rapor_ev TYPE slis_alv_event.

" Select Options
DATA: gv_ktpno TYPE zgkc_kitapno_de,
      gv_ogrno TYPE zgkc_ogrno_de.
SELECT-OPTIONS: s_ktpno FOR gv_ktpno,
                s_ogrno FOR gv_ogrno.

" Teslim Ver Modal
DATA: gv_islemno1  TYPE nriv-nrlevel,
      gv_ogrno1    TYPE zgkc_ogrno_de,
      gv_kitapno1  TYPE zgkc_kitapno_de,
      gv_atarih1   TYPE zgkc_atarih_de,
      gv_puan1     TYPE zgkc_puan_de,
      gv_puan_text TYPE string,
      gs_islem_t   TYPE zgkc_islem_t.

" Teslim Al Modal
DATA: gv_islemno2 TYPE zgkc_islemno_de,
      gv_ogrno2   TYPE zgkc_ogrno_de,
      gv_kitapno2 TYPE zgkc_kitapno_de,
      gv_vtarih2  TYPE zgkc_vtarih_de.

" Güncelle Modal
DATA: gv_islemno3      TYPE zgkc_islemno_de,
      gv_ogrno3        TYPE zgkc_ogrno_de,
      gv_kitapno3      TYPE zgkc_kitapno_de,
      gv_atarih3       TYPE zgkc_atarih_de,
      gv_vtarih3       TYPE zgkc_vtarih_de,
      gv_rapor_created TYPE bool.
