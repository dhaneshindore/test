


*----------------------------------------------------------------------*
*&       Create by - Tech Orbit Team
*&       This utility is developed data decomissioning from MARD/MARC
*&       LFC1/LFC3. Also this can be used for setting the deletion flag
*&       in MARD/MARC
*&---------------------------------------------------------------------*

REPORT ZIKANALM.

DATA: lv_plant TYPE werks_d.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETER: rb_mard RADIOBUTTON GROUP rb1 USER-COMMAND us1 DEFAULT 'X',
           rb_marc RADIOBUTTON GROUP rb1,
           rb_lfc1 RADIOBUTTON GROUP rb1,
           rb_lfc3 RADIOBUTTON GROUP rb1,
           rb_bsik RADIOBUTTON GROUP rb1,
           rb_bsid RADIOBUTTON GROUP rb1,
           rb_bsis RADIOBUTTON GROUP rb1,
           rb_bsak RADIOBUTTON GROUP rb1,
           rb_bsad RADIOBUTTON GROUP rb1,
           rb_bsas RADIOBUTTON GROUP rb1,
           rb_coep RADIOBUTTON GROUP rb1,
           rb_skb1 RADIOBUTTON GROUP rb1,
           rb_lfb1 RADIOBUTTON GROUP rb1,
           rb_flexa RADIOBUTTON GROUP rb1,
           rb_flext RADIOBUTTON GROUP rb1,
           rb_knb1 RADIOBUTTON GROUP rb1,
           rb_mseg RADIOBUTTON GROUP rb1,
           rb_anla RADIOBUTTON GROUP rb1,
           rb_anlb RADIOBUTTON GROUP rb1,
           rb_anlc RADIOBUTTON GROUP rb1,
           rb_anek RADIOBUTTON GROUP rb1,
           rb_anep RADIOBUTTON GROUP rb1,
           rb_ailg RADIOBUTTON GROUP rb1,
           rb_assl RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER: p_bukrs TYPE bukrs MODIF ID buk.
SELECT-OPTIONS : s_plant FOR lv_plant MODIF ID pln.
SELECTION-SCREEN SKIP.
PARAMETER p_status TYPE char1 DEFAULT 'X'.
PARAMETER p_del AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
** Validate the screen data
  PERFORM f_validation.

  IF rb_mard IS NOT INITIAL.
** Process MARD
    PERFORM f_mard_process.
  ELSEIF rb_marc IS NOT INITIAL.
** Process MARC
    PERFORM f_marc_process.
  ELSEIF rb_lfc1 IS NOT INITIAL.
** Process LFC1
    PERFORM f_lfc1_process.
  ELSEIF rb_lfc3 IS NOT INITIAL.
** Process LFC3
    PERFORM f_lfc3_process.
  ELSEIF rb_bsik IS NOT INITIAL.
** Process BSIK
    PERFORM f_bsik_process.
  ELSEIF rb_bsid IS NOT INITIAL.
** Process BSID
    PERFORM f_bsid_process.
  ELSEIF rb_bsis IS NOT INITIAL.
** Process BSIS
    PERFORM f_bsis_process.
  ELSEIF rb_bsak IS NOT INITIAL.
** Process BSAK
    PERFORM f_bsak_process.
  ELSEIF rb_bsad IS NOT INITIAL.
** Process BSAD
    PERFORM f_bsad_process.
  ELSEIF rb_bsas IS NOT INITIAL.
** Process BSAS
    PERFORM f_bsas_process.
  ELSEIF rb_coep IS NOT INITIAL.
** Process COEP
    PERFORM f_coep_process.
  ELSEIF rb_skb1 IS NOT INITIAL.
** Process SKB1
    PERFORM f_skb1_process.
   ELSEIF rb_lfb1 IS NOT INITIAL.
** Process LFB1
    PERFORM f_lfb1_process.
  ELSEIF rb_flext IS NOT INITIAL.
** Process FAGLFLEXT
    PERFORM f_faglflext_process.
  ELSEIF rb_flexa IS NOT INITIAL.
** Process FAGLFLEXA
    PERFORM f_faglflexa_process.
  ELSEIF rb_knb1 IS NOT INITIAL.
** Process KNB1
    PERFORM f_knb1_process.
  ELSEIF rb_mseg IS NOT INITIAL.
** Process MESG
    PERFORM f_mseg_process.
  ELSEIF rb_anla IS NOT INITIAL.
** Process ANLA
    PERFORM f_anla_process.
  ELSEIF rb_anlb IS NOT INITIAL.
** Process ANLB
    PERFORM f_anlb_process.
  ELSEIF rb_anlc IS NOT INITIAL.
** Process ANLC
    PERFORM f_anlc_process.
  ELSEIF rb_anek IS NOT INITIAL.
** Process ANEK
    PERFORM f_anek_process.
  ELSEIF rb_anep IS NOT INITIAL.
** Process ANEP
    PERFORM f_anep_process.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  f_validation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_validation.
  IF rb_mard IS NOT INITIAL AND p_bukrs IS NOT INITIAL.
    MESSAGE 'Company code not allowed for MARD, Enter plant only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_marc IS NOT INITIAL AND p_bukrs IS NOT INITIAL.
    MESSAGE 'Company code not allowed for MARC, Enter plant only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_lfc1 IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for LFC1, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_lfc3 IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for LFC3, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_bsas IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for BSAS, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_bsik IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for BSIK, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_bsid IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for BSID, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_bsis IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for BSIS, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_bsak IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for BSAK, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_bsad IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for BSAD, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_coep IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for COEP, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_skb1 IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for SKB1, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_lfb1 IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for LFB1, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_flext IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for FAGLFLEXT, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_flexa IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for FAGLFLEXA, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_knb1 IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for KNB1, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_mseg IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for mseg, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_anla IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for ANLA, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_anlb IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for ANLB, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_anlc IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for ANLC, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_anek IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for ANEK, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_anep IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for ANEP, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_ailg IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for CE1AILG, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ELSEIF rb_assl IS NOT INITIAL AND s_plant IS NOT INITIAL.
    MESSAGE 'Plant not allowed for CE1ASSL, Enter company code only' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    "f_validation

*&---------------------------------------------------------------------*
*&      Form  f_mard_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_mard_process.
  DATA: lt_mard TYPE TABLE OF mard,
        lt_mard_tmp TYPE TABLE OF mard,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  FIELD-SYMBOLS: <fs_mard> TYPE mard.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  IF p_status IS NOT INITIAL AND p_del IS INITIAL.
    SELECT * FROM mard INTO TABLE lt_mard
             WHERE werks IN s_plant AND lvorm <> 'X'.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_mard LINES lv_lines.
*      DELETE lt_mard FROM 5 TO lv_lines.
      IF lv_lines > 10000.
        lv_do_cnt =  lv_lines / 10000.
        lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
      ELSE.
        lv_do_cnt = 1.
      ENDIF.

      DO lv_do_cnt TIMES.
        CLEAR lt_mard_tmp.
        APPEND LINES OF lt_mard FROM 1 TO 10000 TO lt_mard_tmp.
        DELETE lt_mard FROM 1 TO 10000.
        LOOP AT lt_mard_tmp ASSIGNING <fs_mard>.
          UPDATE mard SET lvorm = 'X'
                      WHERE matnr = <fs_mard>-matnr
                      AND werks = <fs_mard>-werks
                      AND lgort = <fs_mard>-lgort.
          IF sy-subrc <> 0.
            WRITE:/ 'Update failed for', <fs_mard>-matnr, <fs_mard>-werks, <fs_mard>-lgort.
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDIF.
  ELSEIF p_status IS INITIAL AND p_del IS INITIAL.
    SELECT * FROM mard INTO TABLE lt_mard
             WHERE werks IN s_plant AND lvorm = 'X'.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_mard LINES lv_lines.
*      DELETE lt_mard FROM 5 TO lv_lines.
      IF lv_lines > 10000.
        lv_do_cnt =  lv_lines / 10000.
        lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
      ELSE.
        lv_do_cnt = 1.
      ENDIF.

      DO lv_do_cnt TIMES.
        CLEAR lt_mard_tmp.
        APPEND LINES OF lt_mard FROM 1 TO 10000 TO lt_mard_tmp.
        DELETE lt_mard FROM 1 TO 10000.
        LOOP AT lt_mard_tmp ASSIGNING <fs_mard>.
          UPDATE mard SET lvorm = space
                      WHERE matnr = <fs_mard>-matnr
                      AND werks = <fs_mard>-werks
                      AND lgort = <fs_mard>-lgort.
          IF sy-subrc <> 0.
            WRITE:/ 'Update failed for', <fs_mard>-matnr, <fs_mard>-werks, <fs_mard>-lgort.
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDIF.
  ELSEIF p_del IS NOT INITIAL.
    SELECT * FROM mard INTO TABLE lt_mard
             WHERE werks IN s_plant AND lvorm = 'X'.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_mard LINES lv_lines.
*      DELETE lt_mard FROM 5 TO lv_lines.
      IF lv_lines > 10000.
        lv_do_cnt =  lv_lines / 10000.
        lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
      ELSE.
        lv_do_cnt = 1.
      ENDIF.

      DO lv_do_cnt TIMES.
        CLEAR lt_mard_tmp.
        APPEND LINES OF lt_mard FROM 1 TO 10000 TO lt_mard_tmp.
        DELETE lt_mard FROM 1 TO 10000.
        IF lt_mard_tmp IS NOT INITIAL.
          DELETE mard FROM TABLE lt_mard_tmp.
          IF sy-subrc <> 0.
            WRITE:/ 'Delete failed'.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDIF.
ENDFORM.                    "f_mard_process
*&---------------------------------------------------------------------*
*&      Form  f_marc_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_marc_process.
  DATA: lt_marc TYPE TABLE OF marc,
        lt_marc_tmp TYPE TABLE OF marc,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  FIELD-SYMBOLS: <fs_marc> TYPE marc.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  IF p_status IS NOT INITIAL AND p_del IS INITIAL.
    SELECT * FROM marc INTO TABLE lt_marc
             WHERE werks IN s_plant AND lvorm <> 'X'.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_marc LINES lv_lines.
*      DELETE lt_marc FROM 5 TO lv_lines.
      IF lv_lines > 10000.
        lv_do_cnt =  lv_lines / 10000.
        lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
      ELSE.
        lv_do_cnt = 1.
      ENDIF.

      DO lv_do_cnt TIMES.
        CLEAR lt_marc_tmp.
        APPEND LINES OF lt_marc FROM 1 TO 10000 TO lt_marc_tmp.
        DELETE lt_marc FROM 1 TO 10000.
        LOOP AT lt_marc_tmp ASSIGNING <fs_marc>.
          UPDATE marc SET lvorm = 'X'
                      WHERE matnr = <fs_marc>-matnr
                      AND werks = <fs_marc>-werks
                      AND lvorm = <fs_marc>-lvorm.
          IF sy-subrc <> 0.
            WRITE:/ 'Update failed for', <fs_marc>-matnr, <fs_marc>-werks, <fs_marc>-lvorm.
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDIF.
  ELSEIF p_status IS INITIAL AND p_del IS INITIAL.
    SELECT * FROM marc INTO TABLE lt_marc
             WHERE werks IN s_plant AND lvorm = 'X'.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_marc LINES lv_lines.
*      DELETE lt_marc FROM 5 TO lv_lines.
      IF lv_lines > 10000.
        lv_do_cnt =  lv_lines / 10000.
        lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
      ELSE.
        lv_do_cnt = 1.
      ENDIF.

      DO lv_do_cnt TIMES.
        CLEAR lt_marc_tmp.
        APPEND LINES OF lt_marc FROM 1 TO 10000 TO lt_marc_tmp.
        DELETE lt_marc FROM 1 TO 10000.
        LOOP AT lt_marc_tmp ASSIGNING <fs_marc>.
          UPDATE marc SET lvorm = space
                      WHERE matnr = <fs_marc>-matnr
                      AND werks = <fs_marc>-werks
                      AND lvorm = <fs_marc>-lvorm.
          IF sy-subrc <> 0.
            WRITE:/ 'Update failed for', <fs_marc>-matnr, <fs_marc>-werks, <fs_marc>-lvorm.
          ENDIF.
        ENDLOOP.
      ENDDO.
    ENDIF.
  ELSEIF p_del IS NOT INITIAL.
    SELECT * FROM marc INTO TABLE lt_marc
             WHERE werks IN s_plant AND lvorm = 'X'.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_marc LINES lv_lines.
*      DELETE lt_marc FROM 5 TO lv_lines.
      IF lv_lines > 10000.
        lv_do_cnt =  lv_lines / 10000.
        lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
      ELSE.
        lv_do_cnt = 1.
      ENDIF.

      DO lv_do_cnt TIMES.
        CLEAR lt_marc_tmp.
        APPEND LINES OF lt_marc FROM 1 TO 10000 TO lt_marc_tmp.
        DELETE lt_marc FROM 1 TO 10000.
        IF lt_marc_tmp IS NOT INITIAL.
          DELETE marc FROM TABLE lt_marc_tmp.
          IF sy-subrc <> 0.
            WRITE:/ 'Delete failed'.
          ELSE.
            COMMIT WORK AND WAIT.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDIF.
ENDFORM.                    "f_marc_process
*&---------------------------------------------------------------------*
*&      Form  f_lfc1_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_lfc1_process.
  DATA: lt_lfc1 TYPE TABLE OF lfc1,
        lt_lfc1_tmp TYPE TABLE OF lfc1,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM lfc1 INTO TABLE lt_lfc1
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_lfc1 LINES lv_lines.
*    DELETE lt_lfc1 FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_lfc1_tmp.
      APPEND LINES OF lt_lfc1 FROM 1 TO 10000 TO lt_lfc1_tmp.
      DELETE lt_lfc1 FROM 1 TO 10000.
      IF lt_lfc1_tmp IS NOT INITIAL.
        DELETE lfc1 FROM TABLE lt_lfc1_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_lfc1_process
*&---------------------------------------------------------------------*
*&      Form  f_lfc3_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_lfc3_process.
  DATA: lt_lfc3 TYPE TABLE OF lfc3,
        lt_lfc3_tmp TYPE TABLE OF lfc3,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM lfc3 INTO TABLE lt_lfc3
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_lfc3 LINES lv_lines.
*    DELETE lt_lfc3 FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_lfc3_tmp.
      APPEND LINES OF lt_lfc3 FROM 1 TO 10000 TO lt_lfc3_tmp.
      DELETE lt_lfc3 FROM 1 TO 10000.
      IF lt_lfc3_tmp IS NOT INITIAL.
        DELETE lfc3 FROM TABLE lt_lfc3_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_lfc3_process
*&---------------------------------------------------------------------*
*&      Form  f_lfc1_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bsik_process.
  DATA: lt_bsik TYPE TABLE OF bsik,
        lt_bsik_tmp TYPE TABLE OF bsik,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM bsik INTO TABLE lt_bsik
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_bsik LINES lv_lines.
*    DELETE lt_bsik FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_bsik_tmp.
      APPEND LINES OF lt_bsik FROM 1 TO 10000 TO lt_bsik_tmp.
      DELETE lt_bsik FROM 1 TO 10000.
      IF lt_bsik_tmp IS NOT INITIAL.
        DELETE bsik FROM TABLE lt_bsik_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_bsik_process
*&---------------------------------------------------------------------*
*&      Form  f_bsid_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bsid_process.
  DATA: lt_bsid TYPE TABLE OF bsid,
        lt_bsid_tmp TYPE TABLE OF bsid,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM bsid INTO TABLE lt_bsid
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_bsid LINES lv_lines.
*    DELETE lt_bsid FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_bsid_tmp.
      APPEND LINES OF lt_bsid FROM 1 TO 10000 TO lt_bsid_tmp.
      DELETE lt_bsid FROM 1 TO 10000.
      IF lt_bsid_tmp IS NOT INITIAL.
        DELETE bsid FROM TABLE lt_bsid_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_bsid_process
*&---------------------------------------------------------------------*
*&      Form  f_bsis_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bsis_process.
  DATA: lt_bsis TYPE TABLE OF bsis,
        lt_bsis_tmp TYPE TABLE OF bsis,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM bsis INTO TABLE lt_bsis
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_bsis LINES lv_lines.
*    DELETE lt_bsis FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_bsis_tmp.
      APPEND LINES OF lt_bsis FROM 1 TO 10000 TO lt_bsis_tmp.
      DELETE lt_bsis FROM 1 TO 10000.
      IF lt_bsis_tmp IS NOT INITIAL.
        DELETE bsis FROM TABLE lt_bsis_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_bsis_process
*&---------------------------------------------------------------------*
*&      Form  f_bsak_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bsak_process.
  DATA: lt_bsak TYPE TABLE OF bsak,
        lt_bsak_tmp TYPE TABLE OF bsak,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM bsak INTO TABLE lt_bsak
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_bsak LINES lv_lines.
*    DELETE lt_bsak FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_bsak_tmp.
      APPEND LINES OF lt_bsak FROM 1 TO 10000 TO lt_bsak_tmp.
      DELETE lt_bsak FROM 1 TO 10000.
      IF lt_bsak_tmp IS NOT INITIAL.
        DELETE bsak FROM TABLE lt_bsak_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_bsak_process
*&---------------------------------------------------------------------*
*&      Form  f_bsad_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bsad_process.
  DATA: lt_bsad TYPE TABLE OF bsad,
        lt_bsad_tmp TYPE TABLE OF bsad,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM bsad INTO TABLE lt_bsad
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_bsad LINES lv_lines.
*    DELETE lt_bsad FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_bsad_tmp.
      APPEND LINES OF lt_bsad FROM 1 TO 10000 TO lt_bsad_tmp.
      DELETE lt_bsad FROM 1 TO 10000.
      IF lt_bsad_tmp IS NOT INITIAL.
        DELETE bsad FROM TABLE lt_bsad_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_bsad_process
*&---------------------------------------------------------------------*
*&      Form  f_bsas_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bsas_process.
  DATA: lt_bsas TYPE TABLE OF bsas,
        lt_bsas_tmp TYPE TABLE OF bsas,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM bsas INTO TABLE lt_bsas
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_bsas LINES lv_lines.
*    DELETE lt_bsas FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_bsas_tmp.
      APPEND LINES OF lt_bsas FROM 1 TO 10000 TO lt_bsas_tmp.
      DELETE lt_bsas FROM 1 TO 10000.
      IF lt_bsas_tmp IS NOT INITIAL.
        DELETE bsas FROM TABLE lt_bsas_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_bsas_process
*&---------------------------------------------------------------------*
*&      Form  f_coep_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_coep_process.
  DATA: lt_coep TYPE TABLE OF coep,
        lt_coep_tmp TYPE TABLE OF coep,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM coep INTO TABLE lt_coep
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_coep LINES lv_lines.
*    DELETE lt_coep FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_coep_tmp.
      APPEND LINES OF lt_coep FROM 1 TO 10000 TO lt_coep_tmp.
      DELETE lt_coep FROM 1 TO 10000.
      IF lt_coep_tmp IS NOT INITIAL.
        DELETE coep FROM TABLE lt_coep_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_coep_process
*&---------------------------------------------------------------------*
*&      Form  f_skb1_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_skb1_process.
  DATA: lt_skb1 TYPE TABLE OF skb1,
        lt_skb1_tmp TYPE TABLE OF skb1,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM skb1 INTO TABLE lt_skb1
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_skb1 LINES lv_lines.
*    DELETE lt_skb1 FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_skb1_tmp.
      APPEND LINES OF lt_skb1 FROM 1 TO 10000 TO lt_skb1_tmp.
      DELETE lt_skb1 FROM 1 TO 10000.
      IF lt_skb1_tmp IS NOT INITIAL.
        DELETE skb1 FROM TABLE lt_skb1_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_skb1_process
*&---------------------------------------------------------------------*
*&      Form  f_lfb1_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_lfb1_process.
  DATA: lt_lfb1 TYPE TABLE OF lfb1,
        lt_lfb1_tmp TYPE TABLE OF lfb1,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM lfb1 INTO TABLE lt_lfb1
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_lfb1 LINES lv_lines.
*    DELETE lt_lfb1 FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_lfb1_tmp.
      APPEND LINES OF lt_lfb1 FROM 1 TO 10000 TO lt_lfb1_tmp.
      DELETE lt_lfb1 FROM 1 TO 10000.
      IF lt_lfb1_tmp IS NOT INITIAL.
        DELETE lfb1 FROM TABLE lt_lfb1_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_lfb1_process
*&---------------------------------------------------------------------*
*&      Form  f_FAGLFLEXA_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_faglflexa_process.
  DATA: lt_faglflexa TYPE TABLE OF faglflexa,
        lt_faglflexa_tmp TYPE TABLE OF faglflexa,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM faglflexa INTO TABLE lt_faglflexa
                WHERE rbukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_faglflexa LINES lv_lines.
*    DELETE lt_FAGLFLEXA FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_faglflexa_tmp.
      APPEND LINES OF lt_faglflexa FROM 1 TO 10000 TO lt_faglflexa_tmp.
      DELETE lt_faglflexa FROM 1 TO 10000.
      IF lt_faglflexa_tmp IS NOT INITIAL.
        DELETE faglflexa FROM TABLE lt_faglflexa_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_FAGLFLEXA_process
*&---------------------------------------------------------------------*
*&      Form  f_FAGLFLEXT_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_faglflext_process.
  DATA: lt_faglflext TYPE TABLE OF faglflext,
        lt_faglflext_tmp TYPE TABLE OF faglflext,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM faglflext INTO TABLE lt_faglflext
                WHERE rbukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_faglflext LINES lv_lines.
*    DELETE lt_FAGLFLEXT FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_faglflext_tmp.
      APPEND LINES OF lt_faglflext FROM 1 TO 10000 TO lt_faglflext_tmp.
      DELETE lt_faglflext FROM 1 TO 10000.
      IF lt_faglflext_tmp IS NOT INITIAL.
        DELETE faglflext FROM TABLE lt_faglflext_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_FAGLFLEXT_process
*&---------------------------------------------------------------------*
*&      Form  f_KNB1_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_knb1_process.
  DATA: lt_knb1 TYPE TABLE OF knb1,
        lt_knb1_tmp TYPE TABLE OF knb1,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM knb1 INTO TABLE lt_knb1
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_knb1 LINES lv_lines.
*    DELETE lt_knb1 FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_knb1_tmp.
      APPEND LINES OF lt_knb1 FROM 1 TO 10000 TO lt_knb1_tmp.
      DELETE lt_knb1 FROM 1 TO 10000.
      IF lt_knb1_tmp IS NOT INITIAL.
        DELETE knb1 FROM TABLE lt_knb1_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_KNB1_process
*&---------------------------------------------------------------------*
*&      Form  f_MSEG_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_mseg_process.
  DATA: lt_mseg TYPE TABLE OF mseg,
        lt_mseg_tmp TYPE TABLE OF mseg,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM mseg INTO TABLE lt_mseg
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_mseg LINES lv_lines.
*    DELETE lt_mseg FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_mseg_tmp.
      APPEND LINES OF lt_mseg FROM 1 TO 10000 TO lt_mseg_tmp.
      DELETE lt_mseg FROM 1 TO 10000.
      IF lt_mseg_tmp IS NOT INITIAL.
        DELETE mseg FROM TABLE lt_mseg_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_MSEG_process
*&---------------------------------------------------------------------*
*&      Form  f_ANLA_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_anla_process.
  DATA: lt_anla TYPE TABLE OF anla,
        lt_anla_tmp TYPE TABLE OF anla,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM anla INTO TABLE lt_anla
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_anla LINES lv_lines.
*    DELETE lt_anla FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_anla_tmp.
      APPEND LINES OF lt_anla FROM 1 TO 10000 TO lt_anla_tmp.
      DELETE lt_anla FROM 1 TO 10000.
      IF lt_anla_tmp IS NOT INITIAL.
        DELETE anla FROM TABLE lt_anla_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_anla_process
*&---------------------------------------------------------------------*
*&      Form  f_anlb_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_anlb_process.
  DATA: lt_anlb TYPE TABLE OF anlb,
        lt_anlb_tmp TYPE TABLE OF anlb,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM anlb INTO TABLE lt_anlb
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_anlb LINES lv_lines.
*    DELETE lt_anlb FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_anlb_tmp.
      APPEND LINES OF lt_anlb FROM 1 TO 10000 TO lt_anlb_tmp.
      DELETE lt_anlb FROM 1 TO 10000.
      IF lt_anlb_tmp IS NOT INITIAL.
        DELETE anlb FROM TABLE lt_anlb_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_anlb_process
*&---------------------------------------------------------------------*
*&      Form  f_anlc_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_anlc_process.
  DATA: lt_anlc TYPE TABLE OF anlc,
        lt_anlc_tmp TYPE TABLE OF anlc,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM anlc INTO TABLE lt_anlc
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_anlc LINES lv_lines.
*    DELETE lt_anlc FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_anlc_tmp.
      APPEND LINES OF lt_anlc FROM 1 TO 10000 TO lt_anlc_tmp.
      DELETE lt_anlc FROM 1 TO 10000.
      IF lt_anlc_tmp IS NOT INITIAL.
        DELETE anlc FROM TABLE lt_anlc_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_anlc_process
*&---------------------------------------------------------------------*
*&      Form  f_anek_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_anek_process.
  DATA: lt_anek TYPE TABLE OF anek,
        lt_anek_tmp TYPE TABLE OF anek,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM anek INTO TABLE lt_anek
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_anek LINES lv_lines.
*    DELETE lt_anek FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_anek_tmp.
      APPEND LINES OF lt_anek FROM 1 TO 10000 TO lt_anek_tmp.
      DELETE lt_anek FROM 1 TO 10000.
      IF lt_anek_tmp IS NOT INITIAL.
        DELETE anek FROM TABLE lt_anek_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_anek_process
*&---------------------------------------------------------------------*
*&      Form  f_anep_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_anep_process.
  DATA: lt_anep TYPE TABLE OF anep,
        lt_anep_tmp TYPE TABLE OF anep,
        lv_lines TYPE i,
        lv_do_cnt TYPE p DECIMALS 5,
        lv_exit TYPE char1.

  CALL FUNCTION 'C14A_POPUP_CONFIRM_DELETE'
    EXPORTING
      i_obj_count    = '1'
    IMPORTING
      e_flg_continue = lv_exit.

  IF lv_exit IS INITIAL.
    MESSAGE 'Action Cancelled' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT * FROM anep INTO TABLE lt_anep
                WHERE bukrs = p_bukrs.
  IF sy-subrc = 0.
    DESCRIBE TABLE lt_anep LINES lv_lines.
*    DELETE lt_anep FROM 5 TO lv_lines.
    IF lv_lines > 10000.
      lv_do_cnt =  lv_lines / 10000.
      lv_do_cnt = ceil( lv_do_cnt ).   " Round off to positive integer
    ELSE.
      lv_do_cnt = 1.
    ENDIF.

    DO lv_do_cnt TIMES.
      CLEAR lt_anep_tmp.
      APPEND LINES OF lt_anep FROM 1 TO 10000 TO lt_anep_tmp.
      DELETE lt_anep FROM 1 TO 10000.
      IF lt_anep_tmp IS NOT INITIAL.
        DELETE anep FROM TABLE lt_anep_tmp.
        IF sy-subrc <> 0.
          WRITE:/ 'Delete failed'.
        ELSE.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    "f_anep_process
*&---------------------------------------------------------------------*
*&      Form  f_CE1AILG_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
                    "f_ce1ailg_process
*&---------------------------------------------------------------------*
*&      Form  f_ce1assl_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
