FUNCTION-POOL ZGKC_SAS_FG01.                "MESSAGE-ID ..

* INCLUDE LZGKC_SAS_FG01D...                 " Local class definition

CLASS lcl_application DEFINITION DEFERRED.
DATA mo_application TYPE REF TO lcl_application.

TABLES: zgkc_po_item,
        zgkc_po_key.

DATA: mt_exclude TYPE TABLE OF sy-ucomm.