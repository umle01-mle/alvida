class Z_ALV_IDA definition
  public
  final
  create public .

public section.

  methods REMOVE_FIELDS
    importing
      !FIELD_NAMES type CHAR255
      !TABLE_NAME type DBTABL
    exporting
      !BALRET_TAB type BAPIRET2_TAB
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods CREATE_OBJECT
    importing
      !TABLE_NAME type DBTABL
    returning
      value(LO_ALV_DISPLAY) type ref to IF_SALV_GUI_TABLE_IDA .
  methods SET_STANDARD
    importing
      !TABLE_NAME type DBTABL optional
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods ENABLE_ZEBRA
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods SET_PERSISTENCE
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods SET_TITLE
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods SORT
    importing
      !FIELD_NAMES type CHAR255
      !TABLE_NAME type DBTABL
      !DESCENDING type BOOLEAN
      !GROUPED type BOOLEAN default 'X'
    exporting
      !BALRET_TAB type BAPIRET2_TAB
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods CREATE_TOTALS
    importing
      !FIELD_NAMES type DATA
      !TABLE_NAME type DBTABL
    exporting
      !BALRET_TAB type BAPIRET2_TAB
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods CHECK_FIELDS_ARE_VALID
    importing
      !FIELD_NAMES type CHAR255
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA
      !TABLE_NAME type DBTABL
      !CHECK_NUMERIC_TYPE type BOOLEAN optional
    exporting
      !BALRET_TAB type BAPIRET2_TAB
      !FIELDNAMES_RANGE type ZFIELDNAMES .
  methods SPLIT_INPUT_TO_TABLE
    importing
      !FIELD_NAMES type CHAR255
    exporting
      !LIT_FIELDS type ANY TABLE .
  methods CREATE_SELECTIONS
    importing
      !TABLE_NAME type DBTABL
    exporting
      !BALRET_TAB type BAPIRET2_TAB
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods TEXT_SEARCH
    importing
      !TABLE_NAME type DBTABL
      !SEARCH type STRING
    exporting
      !BALRET_TAB type BAPIRET2_TAB
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods GET_ALL_TEXT_FIELDS
    importing
      !TABLE_NAME type DBTABL
    returning
      value(FIELD_NAMES) type IF_SALV_GUI_TYPES_IDA=>YTS_FIELD_NAME .
  methods FUZZY_TEXT_SEARCH
    importing
      !TABLE_NAME type DBTABL
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA .
  methods IS_TEXT_SEARCH_SUPPORTED
    importing
      !TABLE_NAME type DBTABL
    returning
      value(BOOLEAN_RESPONSE) type BOOLEAN .
  methods ALV_IDA_STANDARD
    importing
      !FIELD_NAMES_REMOVE type CHAR255 optional
      !TABLE_NAME type DBTABL
      !FIELD_NAMES_SORT type CHAR255 optional
      !FIELD_NAMES_TOTAL type CHAR255 optional
      !SEARCH_STRING type STRING optional
    exporting
      !BALRET_TAB type BAPIRET2_TAB
    changing
      !LO_ALV_DISPLAY type ref to IF_SALV_GUI_TABLE_IDA
      !Z_ALV type ref to Z_ALV_IDA .
protected section.
private section.
ENDCLASS.



CLASS Z_ALV_IDA IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->ALV_IDA_STANDARD
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAMES_REMOVE             TYPE        CHAR255(optional)
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [--->] FIELD_NAMES_SORT               TYPE        CHAR255(optional)
* | [--->] FIELD_NAMES_TOTAL              TYPE        CHAR255(optional)
* | [--->] SEARCH_STRING                  TYPE        STRING(optional)
* | [<---] BALRET_TAB                     TYPE        BAPIRET2_TAB
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* | [<-->] Z_ALV                          TYPE REF TO Z_ALV_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alv_ida_standard.

    DATA : lif_table_name TYPE dbtabl
         , lit_balret_overall TYPE STANDARD TABLE OF bapiret2
         , lit_balret_overall_line LIKE LINE OF lit_balret_overall.

    lif_table_name = table_name.

    me->set_standard( EXPORTING table_name    = lif_table_name
                      CHANGING lo_alv_display = lo_alv_display ).
    IF field_names_remove IS SUPPLIED.
         me->remove_fields( EXPORTING field_names    = field_names_remove
                                      table_name     = lif_table_name
                            IMPORTING balret_tab     = DATA(lit_balret)
                             CHANGING lo_alv_display = lo_alv_display ).

      APPEND LINES OF lit_balret TO lit_balret_overall.
      CLEAR lit_balret.
    ENDIF.

    IF field_names_sort IS SUPPLIED.
         me->sort(  EXPORTING field_names    = field_names_sort
                              table_name     = lif_table_name
                              descending     = abap_false
                    IMPORTING balret_tab     = lit_balret
                     CHANGING lo_alv_display = lo_alv_display ).

      APPEND LINES OF lit_balret TO lit_balret_overall.
      CLEAR lit_balret.
    ENDIF.

    IF field_names_total IS SUPPLIED.
      me->create_totals( EXPORTING field_names    = field_names_total
                                   table_name     = lif_table_name
                         IMPORTING balret_tab     = lit_balret
                          CHANGING lo_alv_display = lo_alv_display ).

      APPEND LINES OF lit_balret TO lit_balret_overall.
      CLEAR lit_balret.
    ENDIF.

    me->create_selections( EXPORTING table_name     = lif_table_name
                           IMPORTING balret_tab     = lit_balret
                            CHANGING lo_alv_display = lo_alv_display ).

    APPEND LINES OF lit_balret TO lit_balret_overall.
    CLEAR lit_balret.
    IF search_string IS SUPPLIED.
      me->text_search( EXPORTING search         = search_string
                                 table_name     = lif_table_name
                       IMPORTING balret_tab     = lit_balret
                        CHANGING lo_alv_display = lo_alv_display ).

      APPEND LINES OF lit_balret TO lit_balret_overall.
      CLEAR lit_balret.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->CHECK_FIELDS_ARE_VALID
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAMES                    TYPE        CHAR255
* | [--->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [--->] CHECK_NUMERIC_TYPE             TYPE        BOOLEAN(optional)
* | [<---] BALRET_TAB                     TYPE        BAPIRET2_TAB
* | [<---] FIELDNAMES_RANGE               TYPE        ZFIELDNAMES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_fields_are_valid.

    DATA : lit_balret        TYPE bapiret2
         , lit_field_names   TYPE STANDARD TABLE OF if_salv_gui_types_ida=>y_field_name
         , liv_tabix         TYPE syst-tabix.

    DATA(lr_gen)         = NEW z_general_utilities( ).

    DATA(lit_fieldnames) = lr_gen->remove_fields( field_names ).
    DATA(lo_fldcatlog)   = lo_alv_display->field_catalog( ).

    lo_fldcatlog->get_all_fields( IMPORTING ets_field_names = DATA(lts_field_names) ).

    lit_field_names[] = lts_field_names[].
    LOOP AT lit_fieldnames INTO DATA(lit_fieldnames_line).
      liv_tabix = sy-tabix.

      FIND FIRST OCCURRENCE OF lit_fieldnames_line-low IN TABLE lit_field_names.
      IF sy-subrc <> 0.
        CALL FUNCTION 'BALW_BAPIRETURN_GET2'
          EXPORTING
            type   = 'E'
            cl     = 'ZALV'
            number = '001'
            par1   = CONV syst_msgv( lit_fieldnames_line-low )
            par2   = CONV syst_msgv( table_name )
          IMPORTING
            return = lit_balret.
        APPEND lit_balret TO balret_tab.
        DELETE lit_fieldnames INDEX liv_tabix.
        CONTINUE.
      ELSE.
        CALL FUNCTION 'BALW_BAPIRETURN_GET2'
          EXPORTING
            type   = 'I'
            cl     = 'ZALV'
            number = '002'
            par1   = CONV syst_msgv( lit_fieldnames_line-low )
            par2   = CONV syst_msgv( table_name )
          IMPORTING
            return = lit_balret.
        APPEND lit_balret TO balret_tab.
      ENDIF.
      IF check_numeric_type = abap_true.
        SELECT SINGLE 'X' FROM dd03l
                          INTO @sy-ctype
                         WHERE tabname =    @table_name              AND
                             fieldname =    @lit_fieldnames_line-low AND
                             inttype  IN ( 'I','b','s','P','F','a','e','X' ).
        IF sy-subrc <> 0.
          CALL FUNCTION 'BALW_BAPIRETURN_GET2'
            EXPORTING
              type   = 'E'
              cl     = 'ZALV'
              number = '005'
              par1   = CONV syst_msgv( lit_fieldnames_line-low )
              par2   = CONV syst_msgv( table_name )
            IMPORTING
              return = lit_balret.
          APPEND lit_balret TO balret_tab.
          DELETE lit_fieldnames INDEX liv_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

    fieldnames_range[] = lit_fieldnames[].
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->CREATE_OBJECT
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [<-()] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method CREATE_OBJECT.

    lo_alv_display = cl_salv_gui_table_ida=>create( iv_table_name =  table_name ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->CREATE_SELECTIONS
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [<---] BALRET_TAB                     TYPE        BAPIRET2_TAB
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_selections.

    DATA : lit_code              TYPE TABLE OF string
         , lit_code_line         LIKE LINE OF lit_code
         , lit_code_reduced      LIKE lit_code
         , lit_code_reduced_line LIKE LINE OF lit_code_reduced
         , lit_balret            TYPE bapiret2
         , lit_field_names       TYPE STANDARD TABLE OF if_salv_gui_types_ida=>y_field_name.

    READ REPORT sy-cprog INTO lit_code.

    REPLACE ALL OCCURRENCES OF 'select-options' IN TABLE lit_code WITH 'SELECT-OPTIONS'.

    FIND ALL OCCURRENCES OF 'SELECT-OPTIONS' IN TABLE lit_code RESULTS  DATA(lit_code_results).

    LOOP AT lit_code_results INTO DATA(lit_code_results_line).
      DATA(lit_code_index) = lit_code_results_line-line.
      DO.
        READ TABLE lit_code INDEX lit_code_index INTO lit_code_line.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        APPEND lit_code_line TO lit_code_reduced.
        FIND FIRST OCCURRENCE OF '.' IN lit_code_line.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
        ADD 1 TO lit_code_index.
      ENDDO.
    ENDLOOP.

    DATA(lo_range_collect) = NEW cl_salv_range_tab_collector( ).
    FIELD-SYMBOLS: <select_option> TYPE any.

    LOOP AT lit_code_reduced INTO lit_code_reduced_line.
      CONDENSE lit_code_reduced_line.
      SHIFT lit_code_reduced_line RIGHT DELETING TRAILING space.
      SHIFT lit_code_reduced_line RIGHT DELETING TRAILING '.'.
      SHIFT lit_code_reduced_line LEFT  DELETING LEADING  space.
      SHIFT lit_code_reduced_line LEFT  DELETING LEADING  'SELECT-OPTIONS'.
      SHIFT lit_code_reduced_line LEFT  DELETING LEADING  space.
      SHIFT lit_code_reduced_line LEFT  DELETING LEADING  ':'.
      SHIFT lit_code_reduced_line LEFT  DELETING LEADING  ','.
      SHIFT lit_code_reduced_line LEFT  DELETING LEADING  space.

      SPLIT lit_code_reduced_line AT space INTO: DATA(so_name) DATA(so_for) DATA(so_table_field).

      TRANSLATE so_table_field TO UPPER CASE.

      SHIFT so_table_field LEFT DELETING LEADING table_name.
      SHIFT so_table_field LEFT DELETING LEADING '-'.

      CONCATENATE '(' sy-cprog ')' so_name '[]' INTO DATA(stack_reference).
      ASSIGN (stack_reference) TO <select_option>.

      lo_range_collect->add_ranges_for_name(
                        EXPORTING iv_name = so_table_field
                                  it_ranges = <select_option> ).
    ENDLOOP.
*
    lo_range_collect->get_collected_ranges(
                      IMPORTING et_named_ranges = DATA(li_range_pair) ).

    lo_alv_display->set_select_options( it_ranges = li_range_pair ).




  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->CREATE_TOTALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAMES                    TYPE        DATA
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [<---] BALRET_TAB                     TYPE        BAPIRET2_TAB
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_totals.

    DATA lr_aggregations         TYPE REF TO cl_salv_aggregations.
    DATA : ls_aggr_rule          TYPE if_salv_gui_types_ida=>ys_aggregation_rule
         , lt_aggr_rules         TYPE if_salv_gui_types_ida=>yt_aggregation_rule
         , fieldnames_range_line TYPE zfieldnames_line.

    check not field_names is INITIAL.

    me->check_fields_are_valid(
      EXPORTING
        field_names        = field_names
        lo_alv_display     = lo_alv_display
        table_name         = table_name
        check_numeric_type = abap_true
      IMPORTING
        balret_tab         = balret_tab
        fieldnames_range   = DATA(fieldnames_range) ).

    LOOP AT fieldnames_range INTO fieldnames_range_line.
      ls_aggr_rule-field_name = fieldnames_range_line-low.
      ls_aggr_rule-function   = if_salv_service_types=>cs_function_code-sum.
      APPEND ls_aggr_rule TO lt_aggr_rules.
    ENDLOOP.

    CHECK NOT lt_aggr_rules[] IS INITIAL.
    lo_alv_display->default_layout( )->set_aggregations( lt_aggr_rules ).






  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->ENABLE_ZEBRA
* +-------------------------------------------------------------------------------------------------+
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method ENABLE_ZEBRA.

    DATA(lo_display_options) = lo_alv_display->display_options( ).

    lo_display_options->enable_alternating_row_pattern( ).

  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->FUZZY_TEXT_SEARCH
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fuzzy_text_search.
    IF abap_true = cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( table_name  ).
*Hmm - expected the above to check if this is a columnar table as text search is
*not available if it is a row table - but it didn't. is_text_search_supported seems to just
*be generic is it supported - not is it supported for this table. Hence a direct lookup on
*DD09L to check if this is a valid table for text search
      IF abap_true = me->is_text_search_supported( table_name ).
        lo_alv_display->standard_functions( )->set_text_search_active( abap_true ).
        DATA(lit_all_text_fields) = me->get_all_text_fields( table_name ).

        LOOP AT lit_all_text_fields INTO DATA(lit_all_text_fields_line).
          lo_alv_display->field_catalog( )->enable_text_search( lit_all_text_fields_line ).
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->GET_ALL_TEXT_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [<-()] FIELD_NAMES                    TYPE        IF_SALV_GUI_TYPES_IDA=>YTS_FIELD_NAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_all_text_fields.

    SELECT fieldname FROM dd03l
                        INTO TABLE @field_names
                       WHERE tabname = @table_name                AND
                             fieldname NOT IN ('MANDT','MANDANT') AND
                             inttype   IN ( 'C','N','g','y','V' ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->IS_TEXT_SEARCH_SUPPORTED
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [<-()] BOOLEAN_RESPONSE               TYPE        BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method IS_TEXT_SEARCH_SUPPORTED.
    clear boolean_response.
    SELECT single 'X' from dd09l
                      into @boolean_response
                     where tabname     = @table_name and
                           roworcolst <> 'R'.
  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->REMOVE_FIELDS
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAMES                    TYPE        CHAR255
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [<---] BALRET_TAB                     TYPE        BAPIRET2_TAB
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD remove_fields.
    DATA : lit_balret                   TYPE bapiret2
         , lit_field_names              TYPE STANDARD TABLE OF if_salv_gui_types_ida=>y_field_name.
*Instantiation
    DATA(lr_gen)         = NEW z_general_utilities( ).

    DATA(lit_fieldnames) = lr_gen->remove_fields( field_names ).
    DATA(lo_fldcatlog)   = lo_alv_display->field_catalog( ).

    CHECK NOT field_names IS INITIAL.

    lo_fldcatlog->get_all_fields( IMPORTING ets_field_names = DATA(lts_field_names) ).

    lit_field_names[] = lts_field_names[].

    me->check_fields_are_valid(
      EXPORTING
        field_names        = field_names
        lo_alv_display     = lo_alv_display
        table_name         = table_name
        check_numeric_type = abap_false
      IMPORTING
        balret_tab         = balret_tab
        fieldnames_range   = lit_fieldnames ).

    DELETE lts_field_names WHERE table_line IN lit_fieldnames.
    check not lts_field_names[] is INITIAL.
    lo_fldcatlog->set_available_fields( lts_field_names ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->SET_PERSISTENCE
* +-------------------------------------------------------------------------------------------------+
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method SET_PERSISTENCE.

  data : ls_persistence_key TYPE if_salv_gui_layout_persistence=>ys_persistence_key.

  ls_persistence_key-report_name     = sy-repid.
  DATA(l_global_save_allowed)        = abap_true.
  DATA(l_user_specific_save_allowed) = abap_true.
  lo_alv_display->layout_persistence( )->set_persistence_options(
    EXPORTING
      is_persistence_key           = ls_persistence_key
      i_global_save_allowed        = l_global_save_allowed
      i_user_specific_save_allowed = l_user_specific_save_allowed ).

 lo_alv_display->toolbar( )->enable_listbox_for_layouts( ).




  endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->SET_STANDARD
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        DBTABL(optional)
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_standard.

    me->enable_zebra(      CHANGING lo_alv_display = lo_alv_display ).

    me->set_persistence(   CHANGING lo_alv_display = lo_alv_display ).

    me->set_title(         CHANGING lo_alv_display = lo_alv_display ).

    me->FUZZY_TEXT_SEARCH( EXPORTING table_name = table_name
                           changing lo_alv_display = lo_alv_display ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->SET_TITLE
* +-------------------------------------------------------------------------------------------------+
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD set_title.

    DATA : liv_title TYPE sytitle
         , liv_title_date(10)
         , liv_title_time(8).

    DATA(lo_display_options) = lo_alv_display->display_options( ).

    WRITE : sy-datum TO liv_title_date USING EDIT MASK '__/__/____'
        , sy-uzeit TO liv_title_time.

    CONCATENATE liv_title_date liv_title_time sy-cprog sy-sysid sy-title INTO liv_title SEPARATED BY space.
    lo_display_options->set_title( EXPORTING  iv_title = liv_title ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->SORT
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAMES                    TYPE        CHAR255
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [--->] DESCENDING                     TYPE        BOOLEAN
* | [--->] GROUPED                        TYPE        BOOLEAN (default ='X')
* | [<---] BALRET_TAB                     TYPE        BAPIRET2_TAB
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD sort.

    TYPES:
      BEGIN OF lty_sort_rule,
        field_name TYPE fieldname,
        descending TYPE abap_bool,
        is_grouped TYPE abap_bool,
      END OF lty_sort_rule
      ,
      BEGIN OF lty_fields,
        field_name TYPE fieldname,
      END OF lty_fields.

    DATA : lit_sort_rule       TYPE STANDARD TABLE OF lty_sort_rule WITH KEY field_name
         , lit_sort_rule_line  LIKE LINE OF lit_sort_rule
         , lit_balret          TYPE bapiret2
         , lit_fields_high     TYPE fieldname
         , lit_fields          TYPE STANDARD TABLE OF lty_fields.

    check not field_names IS INITIAL.

    DATA(lo_layout) = lo_alv_display->default_layout( ).

    me->split_input_to_table(
      EXPORTING
        field_names = field_names
      IMPORTING
        lit_fields  = lit_fields ).

    me->check_fields_are_valid(
      EXPORTING
        field_names        = field_names
        lo_alv_display     = lo_alv_display
        table_name         = table_name
        check_numeric_type = abap_false
      IMPORTING
        balret_tab         = balret_tab
        fieldnames_range   = DATA(lts_field_names_range) ).

    check not lts_field_names_range[] is INITIAL.

    DELETE lit_fields WHERE field_name NOT IN lts_field_names_range.

    LOOP AT lit_fields INTO lit_fields_high.
      lit_sort_rule_line-field_name = lit_fields_high.
      lit_sort_rule_line-descending = descending.
      lit_sort_rule_line-is_grouped = abap_true.
      APPEND lit_sort_rule_line TO lit_sort_rule.
    ENDLOOP.

    lo_layout->set_sort_order( it_sort_order = lit_sort_rule ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->SPLIT_INPUT_TO_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] FIELD_NAMES                    TYPE        CHAR255
* | [<---] LIT_FIELDS                     TYPE        ANY TABLE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD split_input_to_table.

    SPLIT field_names AT ',' INTO TABLE DATA(lit_fields_temp).
    lit_fields[] = lit_fields_temp[].
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method Z_ALV_IDA->TEXT_SEARCH
* +-------------------------------------------------------------------------------------------------+
* | [--->] TABLE_NAME                     TYPE        DBTABL
* | [--->] SEARCH                         TYPE        STRING
* | [<---] BALRET_TAB                     TYPE        BAPIRET2_TAB
* | [<-->] LO_ALV_DISPLAY                 TYPE REF TO IF_SALV_GUI_TABLE_IDA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD text_search.

    CHECK NOT search IS INITIAL.

    IF abap_true = cl_salv_gui_table_ida=>db_capabilities( )->is_table_supported( table_name ).
      IF abap_true = me->is_text_search_supported( table_name ).

        DATA(lit_field_names) = me->get_all_text_fields( table_name ).

        lo_alv_display->text_search( )->set_search_scope(
        its_field_names = lit_field_names ).

        lo_alv_display->text_search( )->set_search_term( search ).
        lo_alv_display->text_search( )->set_field_similarity( '0.8' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
