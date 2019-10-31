class Z_GENERAL_UTILITIES definition
  public
  final
  create public .

public section.

  methods REMOVE_FIELDS
    importing
      !FIELD_NAMES type CHAR255
    returning
      value(LIT_FIELDNAMES) type ZFIELDNAMES .
  methods CREATE_TEMP_TABLE
    importing
      !SRC_NAME type TABNAME16
      !NUMBER type NUMC1 optional
    exporting
      !BALRET type BAPIRET2
      !LIV_NAME type TABNAME16 .
  methods CREATE_TEMP_TABLE_RETURN_NAME
    importing
      !SRC_NAME type TABNAME16
    exporting
      !BALRET type BAPIRET2
    returning
      value(LIV_NAME) type TABNAME16 .
  methods DELETE_TEMP_TABLE
    importing
      !SRC_NAME type TABNAME16
      !NUMBER type NUMC1 optional
      !PASS_THROUGH type BOOLEAN default ABAP_FALSE
    exporting
      !RETURN type BAPIRET2 .
  methods CHECK_TABLE_IN_USE
    importing
      !TABLE_NAME type TABNAME16
    returning
      value(IN_USE) type BOOLEAN .
  methods INSERT_IN_USE_ENTRY
    importing
      !TABLE_NAME type TABNAME16
    exporting
      !RETURN type BAPIRET2 .
  methods DELETE_IN_USE_ENTRY
    importing
      !TABLE_NAME type DDOBJNAME
    returning
      value(OK) type BOOLEAN .
  methods CLEANUP_ZGENERALTABLOCK .
  methods CREATE_DYNAMIC_SORT
    importing
      !SORT_FIELDS type CHAR255
    returning
      value(SORT_ORDER) type ABAP_SORTORDER_TAB .
  methods REMOVE_FIELD
    importing
      !FIELD_NAME type FIELDNAME
    returning
      value(LIT_FIELDNAMES) type ZFIELDNAMES .
protected section.
private section.
ENDCLASS.



CLASS Z_GENERAL_UTILITIES IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->CHECK_TABLE_IN_USE
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        TABNAME16
* | [<-()] IN_USE                         TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CHECK_TABLE_IN_USE.
    in_use = abap_false.
    select single 'X' into @in_use
                      from ZGENERALTABLOCK
                     where ddobjname = @table_name.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->CLEANUP_ZGENERALTABLOCK
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD cleanup_zgeneraltablock.

    data(liv_date_minus_1day) =  sy-datum - 1.

    SELECT * FROM zgeneraltablock INTO TABLE @DATA(lis_zgeneraltablock)
            WHERE cdate < @liv_date_minus_1day.

    LOOP AT lis_zgeneraltablock
         INTO DATA(lis_zgeneraltablock_line).

      me->delete_temp_table( EXPORTING src_name     = CONV tabname16( lis_zgeneraltablock_line-ddobjname )
                                       pass_through = abap_true
                             IMPORTING return       = DATA(lis_return) ).
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->CREATE_DYNAMIC_SORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] SORT_FIELDS                    TYPE        CHAR255
* | [<-()] SORT_ORDER                     TYPE        ABAP_SORTORDER_TAB
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_dynamic_sort.

    TYPES : BEGIN OF    lty_sort_split
          ,  name  TYPE  ddobjname
          , END   OF    lty_sort_split.

    DATA : lit_temp_sort_order     TYPE STANDARD TABLE OF lty_sort_split.

    SPLIT sort_fields AT ',' INTO TABLE lit_temp_sort_order.

    MOVE-CORRESPONDING lit_temp_sort_order[] TO sort_order[].
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->CREATE_TEMP_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SRC_NAME                       TYPE        TABNAME16
* | [--->] NUMBER                         TYPE        NUMC1(optional)
* | [<---] BALRET                         TYPE        BAPIRET2
* | [<---] LIV_NAME                       TYPE        TABNAME16
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_temp_table.
    DATA liv_in_use TYPE boolean.

    data(liv_src_name) = src_name.

    TRANSLATE liv_src_name USING '_ '.
    CONDENSE  liv_src_name NO-GAPS.

    IF number IS SUPPLIED.
      CONCATENATE 'Z' sy-uname(5) liv_src_name(9) number INTO liv_name.
    ELSE.
      CONCATENATE 'Z' sy-uname(5) liv_src_name(9)        INTO liv_name.
    ENDIF.


    liv_in_use = me->check_table_in_use( liv_name ).

    IF liv_in_use = abap_true.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = 'E'
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = balret.
      RETURN.
    ENDIF.

    IF number IS SUPPLIED.
      me->delete_temp_table( EXPORTING src_name = src_name
                                       number   = number
                             IMPORTING return   = balret ).
    ELSE.
      me->delete_temp_table( EXPORTING src_name = src_name
                             IMPORTING return   = balret ).
    ENDIF.
    COMMIT WORK.
*Ignore errors as should have been deleted. This is just a backstop
    me->insert_in_use_entry( liv_name ).
    COMMIT WORK.
    CALL FUNCTION 'DDUT_OBJECT_COPY'
      EXPORTING
        type          = 'TABL'
        src_name      = conv ddobjname( src_name )
        dst_name      = conv ddobjname( liv_name )
      EXCEPTIONS
        illegal_input = 1
        src_not_found = 2
        copy_failure  = 3
        copy_refused  = 4
        OTHERS        = 5.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = balret.
      RETURN.
    ENDIF.
    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = conv ddobjname( liv_name )
*       AUTH_CHK    = 'X'
*       PRID        = -1
*       EXCOMMIT    = 'X'
*     IMPORTING
*       RC          =
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = balret.
      RETURN.
    ENDIF.
    COMMIT WORK.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->CREATE_TEMP_TABLE_RETURN_NAME
* +-------------------------------------------------------------------------------------------------+
* | [--->] SRC_NAME                       TYPE        TABNAME16
* | [<---] BALRET                         TYPE        BAPIRET2
* | [<-()] LIV_NAME                       TYPE        TABNAME16
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_temp_table_return_name.
    DATA liv_in_use TYPE boolean.
    DATA liv_number TYPE numc1.
    DATA(liv_src_name) = src_name.
    DATA lis_return TYPE bapiret2.

    TRANSLATE liv_src_name USING '_ '.
    CONDENSE  liv_src_name NO-GAPS.

    CONCATENATE 'Z' sy-uname(5) liv_src_name(9)        INTO liv_name.

    me->cleanup_zgeneraltablock( ). "Remove any aged entries

    liv_in_use = me->check_table_in_use( liv_name ).

    IF liv_in_use = abap_true.
      DO 9 TIMES.
        CLEAR : liv_in_use
              , lis_return
              , balret.
        liv_number = sy-index.
        me->create_temp_table( EXPORTING src_name = 'ZEAM_R_04_BASE'
                                         number   = liv_number
                               IMPORTING balret   = lis_return
                                         liv_name = liv_name ).
        IF lis_return-type <> 'E' AND
           lis_return-type <> 'A'.
          CLEAR : lis_return
                , balret.
          EXIT.
        ELSE.
          CALL FUNCTION 'BALW_BAPIRETURN_GET2'
            EXPORTING
              type   = 'E'
              cl     = 'ZEAM'
              number = '214'
*              par1   = sy-msgv1
*              par2   = sy-msgv2
*              par3   = sy-msgv3
*              par4   = sy-msgv4
            IMPORTING
              return = balret.
        ENDIF.
      ENDDO.
    ENDIF.
    IF lis_return-type <> 'E' AND
       lis_return-type <> 'A'.
    ELSE.
      RETURN.
    ENDIF.

    me->delete_temp_table( EXPORTING src_name     = liv_name
                                     pass_through = abap_true
                           IMPORTING return   = balret ).

    COMMIT WORK.
*Ignore errors as should have been deleted. This is just a backstop
    me->insert_in_use_entry( liv_name ).
    COMMIT WORK.
    CALL FUNCTION 'DDUT_OBJECT_COPY'
      EXPORTING
        type          = 'TABL'
        src_name      = CONV ddobjname( src_name )
        dst_name      = CONV ddobjname( liv_name )
      EXCEPTIONS
        illegal_input = 1
        src_not_found = 2
        copy_failure  = 3
        copy_refused  = 4
        OTHERS        = 5.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = balret.
      RETURN.
    ENDIF.
    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = CONV ddobjname( liv_name )
*       AUTH_CHK    = 'X'
*       PRID        = -1
*       EXCOMMIT    = 'X'
*     IMPORTING
*       RC          =
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = balret.
      RETURN.
    ENDIF.
    COMMIT WORK.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->DELETE_IN_USE_ENTRY
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        DDOBJNAME
* | [<-()] OK                             TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delete_in_use_entry.
    DELETE FROM zgeneraltablock WHERE ddobjname = table_name.
    IF sy-subrc = 0.
      ok = abap_true.
    ELSE.
      ok = abap_false.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->DELETE_TEMP_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SRC_NAME                       TYPE        TABNAME16
* | [--->] NUMBER                         TYPE        NUMC1(optional)
* | [--->] PASS_THROUGH                   TYPE        BOOLEAN (default =ABAP_FALSE)
* | [<---] RETURN                         TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delete_temp_table.

    DATA(liv_src_name) = src_name.
    DATA : liv_name TYPE ddobjname.

    IF NOT pass_through = abap_true.
      TRANSLATE liv_src_name USING '_ '.
      CONDENSE liv_src_name NO-GAPS.
      IF number IS SUPPLIED.
        CONCATENATE 'Z' sy-uname(5) liv_src_name(9) number INTO liv_name.
      ELSE.
        CONCATENATE 'Z' sy-uname(5) liv_src_name(9)        INTO liv_name.
      ENDIF.
    ELSE.
      IF src_name(1) <> 'Z'.
        MESSAGE a001(00) WITH 'Attmept to delete non user table - denied and logged'.
      ENDIF.
      liv_name = src_name.
    ENDIF.

    CALL FUNCTION 'DDIF_OBJECT_DELETE'
      EXPORTING
        type          = 'TABL'
        name          = CONV ddobjname( liv_name )
      EXCEPTIONS
        illegal_input = 1
        no_authority  = 2
        OTHERS        = 3.

    IF sy-subrc <> 0.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        IMPORTING
          return = return.
      RETURN.
    ENDIF.

    DATA(lif_ok) = me->delete_in_use_entry( liv_name ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->INSERT_IN_USE_ENTRY
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        TABNAME16
* | [<---] RETURN                         TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method INSERT_IN_USE_ENTRY.

    data : lit_zgeneraltablock_line type ZGENERALTABLOCK.

    move : sy-datum to lit_zgeneraltablock_line-cdate
         , sy-uzeit to lit_zgeneraltablock_line-ctime
         , table_name to lit_zgeneraltablock_line-ddobjname.

    insert ZGENERALTABLOCK FROM lit_zgeneraltablock_line.

    if sy-subrc <> 0.
      endif.

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->REMOVE_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAME                     TYPE        FIELDNAME
* | [<-()] LIT_FIELDNAMES                 TYPE        ZFIELDNAMES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD remove_field.
    data : lit_fieldnames_line type zfieldnames_line.

    lit_fieldnames_line-sign   = 'I'.
    lit_fieldnames_line-option = 'EQ'.
    lit_fieldnames_line-low    = field_name.

    APPEND lit_fieldnames_line TO lit_fieldnames.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_GENERAL_UTILITIES->REMOVE_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAMES                    TYPE        CHAR255
* | [<-()] LIT_FIELDNAMES                 TYPE        ZFIELDNAMES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD remove_fields.

    DATA : lit_fieldnames_line TYPE zfieldnames_line
           , lit_fields_high TYPE fieldname.

    SPLIT field_names AT ',' INTO TABLE DATA(lit_fields).

    LOOP AT lit_fields INTO lit_fields_high.
      DATA(lit_temp_fieldnames) = me->remove_field( lit_fields_high ).
      APPEND LINES OF lit_temp_fieldnames TO lit_fieldnames.
      CLEAR lit_temp_fieldnames.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
