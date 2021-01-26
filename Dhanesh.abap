FUNCTION z_mrs_book_default.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ORDER_HEADER) TYPE  ORDER_HEADER_EXTERN
*"     VALUE(CAUFVD) LIKE  CAUFVD STRUCTURE  CAUFVD OPTIONAL
*"     VALUE(TRTYP) LIKE  TC10-TRTYP OPTIONAL
*"  TABLES
*"      OPERATIONS STRUCTURE  OPERATION_EXTERN OPTIONAL
*"      REQUIREMENTS STRUCTURE  OPERATION_JOBREQUIRE OPTIONAL
*"      AFVGDGET STRUCTURE  AFVGDGET OPTIONAL
*"  EXCEPTIONS
*"      CSI
*"----------------------------------------------------------------------

*Based on order type we have to call either MRS or click functions
  DATA:
        ls_rsg_oitg           TYPE /mrss/c_rsg_oitg,
        lv_failed             TYPE /mrss/t_rsg_boolean.

*Based on order type we have to call either MRS or click functions

* -> check customizing PM/CS order integration: Relevance
  CLEAR ls_rsg_oitg.
  ls_rsg_oitg-order_type = order_header-order_type.
  CALL FUNCTION '/MRSS/RSG_OITG_CB'
    EXPORTING
      is_rsg_oitg = ls_rsg_oitg
    IMPORTING
      es_rsg_oitg = ls_rsg_oitg
      ev_failed   = lv_failed.

  IF lv_failed = space.
*   Call MRS specific Functions

    CALL FUNCTION '/MRSS/RSG_PM_ORDER_EXTERN_BOOK'
      EXPORTING
        order_header = order_header
      TABLES
        operations   = operations
        requirements = requirements
      EXCEPTIONS
        csi          = 1
        OTHERS       = 2.

    IF sy-subrc <> 0.
*       Implement suitable error handling here
    ENDIF.

  ELSE.

    CALL FUNCTION '/WSCGMBH/C_BOOK_DEFAULT'
      EXPORTING
        order_header = order_header
        caufvd       = caufvd
        trtyp        = trtyp
      TABLES
        operations   = operations
        requirements = requirements
        afvgdget     = afvgdget
      EXCEPTIONS
        csi          = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.



  ENDIF.

ENDFUNCTION.
